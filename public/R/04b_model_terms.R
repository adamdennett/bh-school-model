# ======================================================================
# public/R/04b_model_terms.R — what does the model actually need?
# ----------------------------------------------------------------------
# 04 runs Brightopia twice: once with every school identical, once with
# W_j set to weighted preferences. Neither reproduces the offers the
# council made, and the misses are systematic. The two that stand out
# are inside the paired catchments:
#
#   Hove Park       over-predicted, badly, in both runs
#   Blatchington    under-predicted, in both runs
#   Varndean        over-predicted once W_j is real
#   Dorothy Stringer under-predicted throughout
#
# Both pairs share a catchment. In each pair the model sends children to
# the wrong one of the two. That is a specific failure with specific
# candidate causes, and this script adds them one at a time so the
# contribution of each is visible rather than bundled:
#
#   M0  W identical                       Brightopia as originally run
#   M1  + W_j = weighted preferences      what families ask for
#   M2  + faith split                     Cardinal Newman and King's are
#                                         not in every family's choice set
#   M3  + catchment term                  gamma * in_catchment
#   M4  + competing destinations          delta * log(C_j), Fotheringham
#   M5  calibrated to what each catchment asks for: a catchment term per
#       catchment, W balanced to first preferences, and the families in
#       the two paired catchments who would accept only one of the pair,
#       all fitted to the council's catchment x school preference matrix
#
# gamma and delta are fitted here, on ten destination margins. That is a
# weak basis and the fitted values should not be quoted as estimates -
# the identification argument in the report has not gone away. What the
# sequence CAN say is which terms move the model towards the observed
# offers and which do not, and that is what it is used for.
#
# Inputs : public/output/open_inputs.rds, factsheet_panel.rds,
#          adjudicator_preferences.rds (09, which therefore runs first)
# Outputs: public/output/model_terms.rds
# ======================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

message("\n=== Which terms does the model need? ===")

inp <- readRDS(file.path(PUBLIC_OUT, "open_inputs.rds"))
LH  <- "Longhill High School"

CITY_SCHOOLS <- inp$schools$name[inp$schools$name != "Peacehaven Community School"]
zones_city   <- inp$zones %>% filter(area != "Expansion area")

BETA_REF <- 1.7
SIGMA    <- 1.0

# The round the model's cohort corresponds to, derived rather than assumed.
OBS_YEAR <- inp$demand_ts %>%
  filter(area == "Brighton & Hove") %>%
  slice_min(abs(state_demand - sum(zones_city$Oi)), n = 1) %>%
  pull(entry_year)

observed <- readRDS(file.path(PUBLIC_OUT, "factsheet_panel.rds"))$factsheets %>%
  filter(name != "Total", year == OBS_YEAR) %>%
  select(name, observed = off_total)
stopifnot(nrow(observed) == length(CITY_SCHOOLS))

pan26 <- setNames(inp$schools$pan2026, inp$schools$name)

message(sprintf("  Fitting against the %d offers made in the %d round",
                sum(observed$observed), OBS_YEAR))


# ---- Competing destinations -----------------------------------------
# C_j = sum_{k != j} W_k d_jk^-sigma, straight-line km between schools.
# Entered as delta * log(C_j), which is C_j^delta inside the model.
# Fotheringham's expectation is delta < 0: a school with many rivals
# close by loses share to them. Positive delta would mean clustering
# helps, which for schools would be surprising.

sch_xy <- inp$schools %>% filter(name %in% CITY_SCHOOLS) %>%
  select(name, easting, northing)

compete <- function(w = NULL) {
  tidyr::expand_grid(name = sch_xy$name, k = sch_xy$name) %>%
    filter(name != k) %>%
    left_join(sch_xy, by = "name") %>%
    left_join(sch_xy %>% rename(k = name, ke = easting, kn = northing), by = "k") %>%
    mutate(d_km = pmax(euclid_m(easting, northing, ke, kn) / 1000, 0.1),
           Wk   = if (is.null(w)) 1 else unname(w[k])) %>%
    group_by(name) %>%
    summarise(C_j = sum(Wk * d_km^-SIGMA), .groups = "drop")
}

W_WPREFS <- with(inp$attract, setNames(W_wprefs, name))
C_TABLE  <- compete(W_WPREFS)


# ---- One run --------------------------------------------------------
# Every term is optional so the sequence can switch them on in turn.

sch_catch <- purrr::imap_dfr(inp$catchment_schools, ~ tibble(name = .x, sch_catch = .y))

run_model <- function(w_spec = NULL, beta = BETA_REF, gamma = 0, delta = 0,
                      faith = FALSE, capped = FALSE, detail = FALSE,
                      site = "now", pans = NULL) {

  costs <- if (site == "elm") inp$costs_elm else inp$costs_now
  cap   <- if (is.null(pans)) pan26 else pans

  d <- costs %>%
    filter(name %in% CITY_SCHOOLS) %>%
    inner_join(zones_city %>% select(zone, Oi, catchment), by = "zone") %>%
    filter(is.finite(cij), Oi > 0) %>%
    left_join(sch_catch, by = "name") %>%
    left_join(C_TABLE, by = "name") %>%
    mutate(in_catchment = as.integer(!is.na(sch_catch) & sch_catch == catchment))

  d$Wj <- if (is.null(w_spec)) 100 else
    inp$attract[[w_spec]][match(d$name, inp$attract$name)]
  stopifnot(!any(is.na(d$Wj)))

  extra <- gamma * d$in_catchment + delta * log(d$C_j)

  sim <- if (faith) sim_faith_split else prod_constrained_sim
  d <- sim(d, o_col = "Oi", w_col = "Wj", c_col = "cij",
           orig_col = "zone", dest_col = "name",
           alpha = 1, beta = beta, extra_utility = extra)

  if (capped) {
    d <- ipf_capacity(d, cap[CITY_SCHOOLS], orig_col = "zone",
                      dest_col = "name", flow_col = "sim_flow", o_col = "Oi")
    d$flow <- d$sim_flow_capped
  } else {
    d$flow <- d$sim_flow
  }

  # The origin-destination detail, for the flow map in 04c. Dropping
  # zone-school pairs with almost no flow keeps the routing tractable
  # and loses nothing that could be drawn.
  if (detail)
    return(d %>% select(zone, name, Oi, cij, flow) %>% filter(flow > 0.05))

  d %>%
    group_by(name) %>%
    summarise(modelled = sum(flow), .groups = "drop") %>%
    inner_join(observed, by = "name") %>%
    mutate(resid = modelled - observed)
}

# Two scores, and the second is the one to trust.
#
# Seven of the ten schools offered their admission number or more, and
# from M2 onwards the model is TOLD those admission numbers. So a large
# part of any fit from that rung on is the model reproducing a number it
# was handed. Scoring on the schools that were not rationing places
# removes that: those are the ones where the offers reflect how many
# families could be found, and where a model can be right or wrong about
# something.
AT_CEILING <- observed %>%
  inner_join(inp$schools %>% select(name, pan2026), by = "name") %>%
  filter(observed >= pan2026) %>% pull(name)

score <- function(x) {
  free <- x %>% filter(!name %in% AT_CEILING)
  tibble(r2 = CalcRSquared(x$observed, x$modelled),
         rmse = CalcRMSE(x$observed, x$modelled),
         mae = mean(abs(x$resid)),
         mae_free = mean(abs(free$resid)),
         rmse_free = CalcRMSE(free$observed, free$modelled))
}

# ---- Fit gamma and delta --------------------------------------------
# Grid search on RMSE. Ten margins and one or two parameters, so this is
# a search for the best available fit rather than an estimate of a
# behavioural quantity. The grids are wide enough that a value pinned at
# an edge is visible as such.

GAMMA_GRID <- seq(0, 3, by = 0.1)
DELTA_GRID <- seq(-1.5, 1.5, by = 0.1)

fit_one <- function(par, grid, base) {
  purrr::map_dfr(grid, function(v) {
    args <- c(base, setNames(list(v), par))
    cbind(tibble(value = v), score(do.call(run_model, args)))
  })
}

# The capacity ceiling goes in BEFORE the two behavioural terms are
# fitted, and the order is not arbitrary. Seven of the ten schools
# offered their admission number or more, so a model without a ceiling
# is being asked to reproduce numbers that were set by rationing rather
# than by choice. Fitting gamma and delta against those first would be
# fitting them to the ceiling, and they would absorb work that belongs
# to a constraint. With the ceiling in place they are asked a cleaner
# question: is there anything left for them to explain?
base_cap <- list(w_spec = "W_wprefs", capped = TRUE)

message("\n  Fitting the catchment term (gamma), capacity in place...")
g_fit <- fit_one("gamma", GAMMA_GRID, base_cap)
GAMMA_HAT <- g_fit$value[which.min(g_fit$rmse)]
message(sprintf("    best gamma = %.1f (RMSE %.0f against %.0f at gamma = 0)",
                GAMMA_HAT, min(g_fit$rmse), g_fit$rmse[g_fit$value == 0]))

message("  Fitting the competing-destinations term (delta), gamma at its fit...")
d_fit <- fit_one("delta", DELTA_GRID, c(base_cap, list(gamma = GAMMA_HAT)))
DELTA_HAT <- d_fit$value[which.min(d_fit$rmse)]
message(sprintf("    best delta = %+.1f (RMSE %.0f against %.0f at delta = 0)",
                DELTA_HAT, min(d_fit$rmse), d_fit$rmse[d_fit$value == 0]))

# Delta on its own, so the two terms are not credited with each other's
# work, and delta with no ceiling at all, which is the version the
# question was originally asked about.
d_alone <- fit_one("delta", DELTA_GRID, base_cap)
DELTA_ALONE <- d_alone$value[which.min(d_alone$rmse)]
message(sprintf("    delta alone, capacity in place = %+.1f, RMSE %.0f",
                DELTA_ALONE, min(d_alone$rmse)))

d_nocap <- fit_one("delta", DELTA_GRID, list(w_spec = "W_wprefs"))
DELTA_NOCAP <- d_nocap$value[which.min(d_nocap$rmse)]
message(sprintf("    delta alone, NO ceiling = %+.1f, RMSE %.0f (against %.0f at delta = 0)",
                DELTA_NOCAP, min(d_nocap$rmse), d_nocap$rmse[d_nocap$value == 0]))


# ---- The sequence ---------------------------------------------------

MODELS <- list(
  M0 = list(label = "Brightopia: every school identical",
            args = list(w_spec = NULL)),
  M1 = list(label = "+ W = weighted preferences",
            args = list(w_spec = "W_wprefs")),
  M2 = list(label = "+ capacity ceiling",
            args = base_cap),
  M3 = list(label = sprintf("+ catchment term (gamma = %.1f)", GAMMA_HAT),
            args = c(base_cap, list(gamma = GAMMA_HAT))),
  M4 = list(label = sprintf("+ competing destinations (delta = %+.1f)", DELTA_HAT),
            args = c(base_cap, list(gamma = GAMMA_HAT, delta = DELTA_HAT)))
)

# The faith split is reported separately rather than as a rung. It was
# estimated in 01c to fit the published SECOND-PREFERENCE profile, and
# on that target it works. On this one it does not: holding half the
# city ineligible for Cardinal Newman and King's makes it impossible for
# either to fill, and both did. Bolting it onto the ladder would have
# made every later rung look worse for a reason that had nothing to do
# with that rung.
faith_variant <- run_model(w_spec = "W_wprefs", capped = TRUE,
                           gamma = GAMMA_HAT, delta = DELTA_HAT, faith = TRUE)

runs <- purrr::imap(MODELS, function(m, id) do.call(run_model, m$args))

# ---- M5: calibrated to what each catchment asks for -------------------
#
# gamma above is fitted to OFFERS, and from M2 onwards offers are the one
# thing the model is told: most of them are admission numbers. So gamma
# was asked what is left once the ceiling has done its work, and the
# answer was "not much". That is a different question from how strongly
# families follow their catchment, and the answer to that one is in the
# council's own catchment-level preference matrix: for each catchment and
# each of three rounds, how many first, second and third preferences its
# children gave each school. It says four in five first preferences from
# the Stringer/Varndean catchment go to those two schools, where M4 sends
# three in five of that catchment's demand there.
#
# M5 fits three things to the matrix, all on UNCAPPED demand - what
# families ask for - and then puts the ceiling on top exactly as before:
#
#   gamma_h  a catchment term for each of the six catchments, by
#            multinomial deviance of the first preferences each
#            catchment's children gave each school
#   W_j      re-balanced so the model's demand for each school reproduces
#            its share of the city's first preferences
#   e_hj     in the two paired catchments, the share of children who would
#            accept only one school of the pair. A child who names
#            Varndean and not Stringer and is refused at Varndean does not
#            fall back on Stringer; M4 assumes every child does.
#
# The fit uses the catchment map the preferences were made under
# (pre-2024); the runs use the map in force, like every other rung. beta,
# sigma and delta stay where the ladder put them.

M5_ROUNDS_FILE <- file.path(PUBLIC_OUT, "adjudicator_preferences.rds")
if (!file.exists(M5_ROUNDS_FILE))
  stop("M5 needs adjudicator_preferences.rds: run 09_adjudicator_preferences.R first")
ap_m5 <- readRDS(M5_ROUNDS_FILE)$prefs
AREA_M5 <- c("CA-A" = "PACA", "CA-B" = "Hove_Blatch", "CA-C" = "Patcham",
             "CA-D" = "DS_Varndean", "CA-E" = "BACA", "CA-F" = "Longhill")
CATCH_M5 <- unname(AREA_M5)
PAIRS_M5 <- inp$catchment_schools[lengths(inp$catchment_schools) == 2]

# -- What the matrix says ------------------------------------------------
obs_cells <- ap_m5 %>%
  filter(school %in% CITY_SCHOOLS) %>%
  mutate(home = unname(AREA_M5[area])) %>%
  group_by(home, school) %>%
  summarise(pref1 = sum(pref1), .groups = "drop") %>%
  group_by(home) %>% mutate(obs_share = pref1 / sum(pref1)) %>% ungroup()
target_m5 <- obs_cells %>% group_by(school) %>%
  summarise(p = sum(pref1), .groups = "drop") %>% mutate(p = p / sum(p))
target_m5 <- setNames(target_m5$p, target_m5$school)

# Named at any rank, pooled over the three rounds. The matrix counts how
# many children named each school, not who named both, so the share who
# would accept only one of a pair is bounded rather than observed. The
# hard bounds come from the counts alone. The default sits midway between
# the most overlap the counts allow and what independent naming would
# give: families treat a pair as substitutes, so real overlap is above
# independence, and the default is not taken to either end.
named <- ap_m5 %>%
  mutate(home = unname(AREA_M5[area])) %>%
  group_by(home, school) %>%
  summarise(named = sum(pref1 + pref2 + pref3), .groups = "drop")
children <- ap_m5 %>% filter(school == "Grand Total") %>%
  mutate(home = unname(AREA_M5[area])) %>%
  group_by(home) %>% summarise(N = sum(pref1), .groups = "drop")

excl_m5 <- purrr::imap_dfr(PAIRS_M5, function(sch, h) {
  N <- children$N[children$home == h]
  n <- setNames(named$named[named$home == h & named$school %in% sch],
                named$school[named$home == h & named$school %in% sch])[sch]
  both_max <- min(n); both_ind <- prod(n) / N; both_min <- max(0, sum(n) - N)
  both <- (both_max + both_ind) / 2
  tibble(catchment = h, school = sch, children = N, named = unname(n),
         share = unname((n - both) / N),
         share_lo = unname((n - both_max) / N),
         share_hi = unname((n - both_min) / N),
         share_independent = unname((n - both_ind) / N))
})
stopifnot(all(excl_m5$share >= excl_m5$share_lo - 1e-9),
          all(excl_m5$share <= excl_m5$share_hi + 1e-9))
message("\n=== M5: children in the paired catchments who would accept only one of the pair ===")
print(as.data.frame(excl_m5 %>% transmute(catchment, school = sub(" School$", "", school),
  named, children, `share (default)` = sprintf("%.1f%%", 100 * share),
  bounds = sprintf("%.1f%%-%.1f%%", 100 * share_lo, 100 * share_hi))), row.names = FALSE)

# -- A fast uncapped model -------------------------------------------------
pops_m5 <- purrr::imap_dfr(split(excl_m5, excl_m5$catchment), function(e, h)
  bind_rows(tibble(catchment = h, pop = "either", pop_share = 1 - sum(e$share),
                   excluded = NA_character_),
            tibble(catchment = h, pop = paste("only", e$school),
                   pop_share = e$share, excluded = rev(e$school))))

m5_table <- function(site = "now", home_col = "catchment", pops = TRUE) {
  costs <- if (site == "elm") inp$costs_elm else inp$costs_now
  x <- costs %>%
    filter(name %in% CITY_SCHOOLS) %>%
    inner_join(zones_city %>% select(zone, Oi, home = all_of(home_col)), by = "zone") %>%
    filter(is.finite(cij), Oi > 0) %>%
    left_join(sch_catch, by = "name") %>%
    mutate(in_c = as.numeric(!is.na(sch_catch) & sch_catch == home))
  if (pops) {
    x <- x %>% left_join(pops_m5, by = c("home" = "catchment"),
                         relationship = "many-to-many")
  } else {
    x$pop <- NA_character_; x$pop_share <- NA_real_; x$excluded <- NA_character_
  }
  x %>% mutate(pop = coalesce(pop, "either"), pop_share = coalesce(pop_share, 1)) %>%
    filter(is.na(excluded) | name != excluded) %>%
    mutate(orig = paste(zone, pop, sep = "#"), Oi = Oi * pop_share,
           orig_id = match(orig, unique(orig)))
}

m5_flows <- function(x, W, gam) {
  cj <- with(compete(W), setNames(C_j, name))
  g <- unname(gam[x$home]); g[is.na(g)] <- 0
  u <- W[x$name] * x$cij^(-BETA_REF) * exp(g * x$in_c) * cj[x$name]^DELTA_HAT
  u[!is.finite(u)] <- 0
  den <- rowsum(u, x$orig_id, reorder = FALSE)[, 1]
  as.numeric(x$Oi * u / den[x$orig_id])
}

m5_balance <- function(x, gam, W = W_WPREFS[CITY_SCHOOLS], iters = 200) {
  for (i in seq_len(iters)) {
    s <- tapply(m5_flows(x, W, gam), x$name, sum)
    s <- s / sum(s)
    W[names(s)] <- W[names(s)] * (target_m5[names(s)] / s)^0.8
    if (max(abs(s - target_m5[names(s)])) < 1e-6) break
  }
  W / mean(W)
}

m5_cells <- function(x, W, gam) {
  f <- m5_flows(x, W, gam)
  tibble(home = x$home, school = x$name, f = f) %>%
    group_by(home, school) %>% summarise(n = sum(f), .groups = "drop") %>%
    group_by(home) %>% mutate(model_share = n / sum(n)) %>% ungroup() %>%
    right_join(obs_cells, by = c("home", "school")) %>%
    mutate(model_share = coalesce(model_share, 0))
}
m5_deviance <- function(cells)
  2 * sum(ifelse(cells$pref1 > 0,
                 cells$pref1 * log(cells$obs_share / pmax(cells$model_share, 1e-9)), 0))

# -- Fit gamma per catchment ---------------------------------------------
message("\n=== M5: fitting a catchment term for each catchment ===")
x_fit <- m5_table(home_col = "catch_pre2024")
GRID_M5 <- seq(0, 4, by = 0.1)
gam_m5 <- setNames(rep(1.5, length(CATCH_M5)), CATCH_M5)
W_m5 <- m5_balance(x_fit, gam_m5)
profile_m5 <- list()
for (sweep in 1:3) {
  moved <- FALSE
  for (h in CATCH_M5) {
    dev <- vapply(GRID_M5, function(v) {
      g <- gam_m5; g[h] <- v
      m5_deviance(m5_cells(x_fit, m5_balance(x_fit, g, W_m5, iters = 60), g))
    }, numeric(1))
    best_v <- GRID_M5[which.min(dev)]
    if (abs(best_v - gam_m5[h]) > 1e-9) moved <- TRUE
    gam_m5[h] <- best_v
    W_m5 <- m5_balance(x_fit, gam_m5, W_m5)
    profile_m5[[h]] <- tibble(catchment = h, gamma = GRID_M5, deviance = dev)
  }
  message(sprintf("  sweep %d: %s", sweep,
                  paste(sprintf("%s %.1f", names(gam_m5), gam_m5), collapse = ", ")))
  if (!moved) break
}
at_edge <- names(gam_m5)[gam_m5 %in% range(GRID_M5)]
if (length(at_edge))
  warning("M5 catchment term pinned at the edge of its grid for: ",
          paste(at_edge, collapse = ", "), call. = FALSE)
W_m5 <- m5_balance(x_fit, gam_m5, W_m5)

cells_m5 <- m5_cells(x_fit, W_m5, gam_m5)
x_fit_m4 <- m5_table(home_col = "catch_pre2024", pops = FALSE)
cells_m4 <- m5_cells(x_fit_m4, W_WPREFS[CITY_SCHOOLS],
                     setNames(rep(GAMMA_HAT, length(CATCH_M5)), CATCH_M5))
cells_cmp <- cells_m4 %>% select(home, school, pref1, obs_share, m4_share = model_share) %>%
  left_join(cells_m5 %>% select(home, school, m5_share = model_share),
            by = c("home", "school"))
own_m5 <- cells_cmp %>%
  filter(unname(setNames(sch_catch$sch_catch, sch_catch$name)[school]) == home) %>%
  group_by(home) %>%
  summarise(observed = sum(obs_share), M4 = sum(m4_share), M5 = sum(m5_share),
            .groups = "drop")
fit_m5 <- tibble(model = c("M4", "M5"),
                 deviance = c(m5_deviance(cells_m4), m5_deviance(cells_m5)),
                 cell_rmse_pp = c(sqrt(mean((100 * (cells_cmp$obs_share - cells_cmp$m4_share))^2)),
                                  sqrt(mean((100 * (cells_cmp$obs_share - cells_cmp$m5_share))^2))))
message("  fit to the catchment preference matrix:")
print(as.data.frame(fit_m5 %>% mutate(across(-model, ~ round(.x, 2)))), row.names = FALSE)
message("  share of each catchment's first preferences going to its own school(s):")
print(as.data.frame(own_m5 %>% mutate(across(-home, ~ sprintf("%.0f%%", 100 * .x)))),
      row.names = FALSE)

# -- The capped run, on the map in force -----------------------------------
m5_run <- function(site = "now", pans = NULL, capped = TRUE, detail = FALSE) {
  cap <- if (is.null(pans)) pan26 else pans
  x <- m5_table(site = site)
  x$sim_flow <- m5_flows(x, W_m5, gam_m5)
  x$flow <- if (capped)
    ipf_capacity(x, cap[CITY_SCHOOLS], orig_col = "orig", dest_col = "name",
                 flow_col = "sim_flow", o_col = "Oi")$sim_flow_capped
  else x$sim_flow
  if (detail)
    return(x %>% group_by(zone, name) %>%
             summarise(cij = first(cij), flow = sum(flow), .groups = "drop") %>%
             left_join(zones_city %>% select(zone, Oi), by = "zone") %>%
             select(zone, name, Oi, cij, flow) %>% filter(flow > 0.05))
  x %>% group_by(name) %>% summarise(modelled = sum(flow), .groups = "drop") %>%
    inner_join(observed, by = "name") %>%
    mutate(resid = modelled - observed)
}

MODELS$M5 <- list(label = "Calibrated to what each catchment asks for", args = NULL)
runs$M5 <- m5_run()
wanted_m5 <- m5_run(capped = FALSE) %>% select(name, wanted = modelled)

CAL <- list(
  gamma = gam_m5, W = W_m5, W_wprefs = W_WPREFS[CITY_SCHOOLS],
  exclusive = excl_m5, target_first_prefs = target_m5,
  cells = cells_cmp, own = own_m5, fit = fit_m5, profile = bind_rows(profile_m5),
  wanted = wanted_m5, beta = BETA_REF, delta = DELTA_HAT, sigma = SIGMA,
  gamma_m4 = GAMMA_HAT, fit_map = "pre2024", run_map = inp$regime,
  rounds = sort(unique(ap_m5$round)),
  source = "BHCC evidence to the Schools Adjudicator, item 8.1 (catchment x school preferences)")
message(sprintf("  M5 intakes: %s",
                paste(sprintf("%s %.0f", sub(" (School|High School|Community Academy|Catholic School)$", "", runs$M5$name),
                              runs$M5$modelled), collapse = ", ")))

ladder <- purrr::imap_dfr(runs, function(x, id)
  cbind(tibble(model = id, label = MODELS[[id]]$label), score(x)))

message("\n=== Fit against the published offers, adding one term at a time ===")
message(sprintf("    (%d of %d schools were at or above their PAN: %s)",
                length(AT_CEILING), nrow(observed),
                paste(sub(" (School|High School|Community Academy|Catholic School).*", "",
                          AT_CEILING), collapse = ", ")))
print(as.data.frame(ladder %>%
  transmute(Model = model, Term = label,
            R2 = round(r2, 3), RMSE = round(rmse), MAE = round(mae),
            `MAE, unrationed only` = round(mae_free))),
  row.names = FALSE)

fv <- score(faith_variant)
message(sprintf(
  "\n  Variant: the same model with the faith split applied -> R2 %.3f, RMSE %.0f, MAE %.0f",
  fv$r2, fv$rmse, fv$mae))
message(sprintf(
  "    Cardinal Newman modelled %.0f against %d offered; it cannot fill with half the city ineligible.",
  faith_variant$modelled[faith_variant$name == "Cardinal Newman Catholic School"],
  faith_variant$observed[faith_variant$name == "Cardinal Newman Catholic School"]))

# ---- The four schools the exercise is about -------------------------

PAIRS <- c("Blatchington Mill School", "Hove Park School",
           "Dorothy Stringer School", "Varndean School")

resid_wide <- purrr::imap_dfr(runs, function(x, id)
  x %>% mutate(model = id)) %>%
  filter(name %in% PAIRS) %>%
  select(model, name, resid) %>%
  pivot_wider(names_from = model, values_from = resid)

message("\n=== Residuals in the two paired catchments (modelled - observed) ===")
print(as.data.frame(resid_wide %>%
  mutate(across(where(is.numeric), ~ sprintf("%+.0f", .x)))), row.names = FALSE)

# Within each pair the interesting quantity is the SPLIT, not the level:
# the model can get the catchment's total right and still send the
# children to the wrong school of the two.
pair_split <- purrr::imap_dfr(runs, function(x, id) {
  p <- x %>% filter(name %in% PAIRS) %>%
    mutate(pair = if_else(name %in% PAIRS[1:2], "Hove Park / Blatchington",
                          "Stringer / Varndean"))
  p %>% group_by(pair) %>%
    summarise(mod_total = sum(modelled), obs_total = sum(observed),
              # share going to the smaller/second-named school of the pair
              mod_share = modelled[name %in% c("Hove Park School", "Varndean School")] /
                          sum(modelled),
              obs_share = observed[name %in% c("Hove Park School", "Varndean School")] /
                          sum(observed),
              .groups = "drop") %>%
    mutate(model = id)
})

message("\n=== Does the model split each paired catchment the way the offers do? ===")
message("    share going to Hove Park (of the Hove Park + Blatchington pair)")
message("    and to Varndean (of the Varndean + Stringer pair)")
print(as.data.frame(pair_split %>%
  transmute(Model = model, Pair = pair,
            `Modelled total` = round(mod_total), `Observed total` = round(obs_total),
            `Modelled share` = sprintf("%.0f%%", 100 * mod_share),
            `Observed share` = sprintf("%.0f%%", 100 * obs_share)) %>%
  arrange(Pair, Model)), row.names = FALSE)

# ---- Full school-level table for the best model ---------------------

# The full model is M5, named rather than picked by fit to offers. It is
# fitted to preferences, not offers, so the offers table is not the test
# it was built to pass - and it is the model the app and the flow map run.
best_id <- "M5"
message(sprintf("\n=== School-level fit, %s (%s) ===", best_id,
                MODELS[[best_id]]$label))
print(as.data.frame(runs[[best_id]] %>%
  arrange(resid) %>%
  transmute(School = name, Observed = observed,
            Modelled = round(modelled), Residual = sprintf("%+.0f", resid))),
  row.names = FALSE)

# ---- The relocation scenario ----------------------------------------
# Longhill moved to the top of Elm Grove and its admission number cut to
# 150 - configuration D of the scenario suite, the one section 8 of the
# strategic view finds composes best. Run at the same specification as
# M2: weighted preferences and a capacity ceiling, no catchment term.
# That is the spec catchments are designed from, and designing them from
# a model that already contains a catchment term would part-rediscover
# the boundaries being replaced.

# Two admission numbers, because the catchment work found that the
# choice between them decides whether the east of the city has a school
# in reach at all. 150 is configuration D of the scenario suite, "shrink
# and move"; 210 is configuration C, "move only", which keeps the number
# now in force.
ELM_PANS <- c(150, 210)
pan_elm_of <- function(p) { v <- pan26; v[LH] <- p; v }

elm_runs <- purrr::map(ELM_PANS, function(p)
  run_model(w_spec = "W_wprefs", capped = TRUE, site = "elm",
            pans = pan_elm_of(p)))
names(elm_runs) <- paste0("ELM", ELM_PANS)

ELM_PAN <- ELM_PANS[1]
pan_elm <- pan_elm_of(ELM_PAN)
elm_run <- elm_runs[["ELM150"]]

now_run <- run_model(w_spec = "W_wprefs", capped = TRUE)

message("\n=== Longhill at Elm Grove, at each admission number ===")
elm_cmp <- now_run %>% select(name, at_ovingdean = modelled)
for (i in seq_along(ELM_PANS))
  elm_cmp <- elm_cmp %>%
    left_join(elm_runs[[i]] %>%
                select(name, !!paste0("PAN ", ELM_PANS[i]) := modelled),
              by = "name")
print(as.data.frame(elm_cmp %>%
  arrange(desc(.data[[paste0("PAN ", ELM_PANS[1])]] - at_ovingdean)) %>%
  mutate(across(where(is.numeric), round))), row.names = FALSE)

# The origin-destination flows behind each rung, and behind both
# relocation scenarios, for 04c to draw and for the catchment design.
od_flows <- bind_rows(
  purrr::imap_dfr(MODELS[names(MODELS) != "M5"], function(m, id)
    do.call(run_model, c(m$args, list(detail = TRUE))) %>% mutate(model = id)),
  m5_run(detail = TRUE) %>% mutate(model = "M5"),
  purrr::imap_dfr(setNames(as.list(ELM_PANS), names(elm_runs)), function(p, id)
    run_model(w_spec = "W_wprefs", capped = TRUE, site = "elm",
              pans = pan_elm_of(p), detail = TRUE) %>% mutate(model = id)),
  # Uncapped, for the catchment design. A capped run cannot show a
  # school drawing more than its admission number, so seeding a
  # CATCHMENT from one bakes the ceiling into the boundary: Longhill at
  # Elm Grove on 150 places has no modelled flow from the east, and the
  # design then hands the east to a school an hour away. A catchment
  # should be drawn round where children would go; the ceiling binds
  # inside it afterwards.
  run_model(w_spec = "W_wprefs", capped = FALSE, site = "elm",
            detail = TRUE) %>% mutate(model = "ELMU"))

message(sprintf("\n  Origin-destination flows kept for the map: %s rows across %d models",
                format(nrow(od_flows), big.mark = ","), length(MODELS)))

saveRDS(list(
  ladder      = ladder,
  runs        = runs,
  od_flows    = od_flows,
  models      = MODELS,
  resid_pairs = resid_wide,
  pair_split  = pair_split,
  gamma_fit   = g_fit,  gamma_hat   = GAMMA_HAT,
  delta_fit   = d_fit,  delta_hat   = DELTA_HAT,
  delta_alone = d_alone, delta_alone_hat = DELTA_ALONE,
  delta_nocap = d_nocap, delta_nocap_hat = DELTA_NOCAP,
  faith_variant = faith_variant, faith_score = fv,
  observed    = observed, observed_year = OBS_YEAR,
  elm_run     = elm_run, elm_pan = ELM_PAN,
  elm_runs    = elm_runs, elm_pans = ELM_PANS, elm_compare = elm_cmp,
  beta        = BETA_REF, sigma = SIGMA,
  best        = best_id,
  calibrated  = CAL,
  run_at      = Sys.time()
), file.path(PUBLIC_OUT, "model_terms.rds"))

message("\nSaved public/output/model_terms.rds")
