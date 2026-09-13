# ======================================================================
# public/R/04_brightopia.R — the distance-only model
# ----------------------------------------------------------------------
# A reproduction of the "Brightopia" model first run in
# BH_Schools_2/schools_wk3.qmd, on the rebuilt routed travel matrix and
# the current catchment map.
#
# The premise is a Brighton in which every secondary school is identical
# — same size, same quality, same buildings, same staff, same lunches,
# and no religious character. Children are identical too. The single
# thing that separates the schools is where they stand. Ask which school
# each child would choose when the only thing that can distinguish them
# is how long it takes to get there, and the answer is a statement about
# the geography of the city and nothing else.
#
# This is the cleanest test available of whether Longhill is in the
# right place, because it makes no assumption whatever about how good
# any school is. It cannot be dismissed as an artefact of attainment
# proxies, Ofsted grades or preference data: remove every one of those
# and the question of location remains.
#
# Following the original:
#   fixed attractiveness (W identical for all schools), alpha = 1
#   beta = 1.5, production-constrained, no capacity ceiling,
#   no catchment priority, no faith restriction
#
# Departures from the original, all reported rather than assumed:
#   - beta is swept from 0.5 to 3.0 rather than fixed at 1.5, so the
#     conclusion does not rest on one decay value
#   - journey times come from the rebuilt r5r matrix, which reaches the
#     Elm Grove site directly instead of approximating it
#   - the relocated case is run on the same footing as the current one
#
# A SECOND RUN sits alongside the distance-only one, at the bundle's
# central specification: beta 1.7 and W_j set to rank-weighted published
# preferences. The distance-only run answers "where would children go if
# the schools differed only in where they stand?", which is the question
# this file exists to ask and which needs W identical. It cannot answer
# "where would children go given what families actually want?", because
# it has assumed that away. The two are reported together: the first is
# a statement about the geography of the city, the second about the
# geography plus the demand, and the gap between them is what
# attractiveness is doing.
#
# Inputs : public/output/open_inputs.rds
# Outputs: public/output/brightopia.rds, fig_brightopia_*.png
# ======================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

message("\n=== Brightopia: the distance-only model ===")

inp <- readRDS(file.path(PUBLIC_OUT, "open_inputs.rds"))
LH  <- "Longhill High School"

# The original ran on the city as it was: the ten Brighton & Hove state
# secondaries, and the children living in the city. Peacehaven and the
# expansion area are held back for a second run below, so the comparison
# with the original is like for like.
CITY_SCHOOLS <- inp$schools$name[inp$schools$name != "Peacehaven Community School"]

BETAS_B <- seq(0.5, 3.0, by = 0.1)
BETA_ORIGINAL <- 1.5   # what the original used; kept for the reproduction
BETA_REF      <- 1.7   # the bundle's central decay, used for the W run

zones_city <- inp$zones %>% filter(area != "Expansion area")

#' One Brightopia run
#'
#' @param site "now" or "elm"
#' @param beta distance decay
#' @param scope "city" or "expanded"
#' @param w_spec NULL for the distance-only world in which every school
#'   is identical, or a column of inp$attract to use as W_j
brightopia <- function(site, beta, scope = "city", w_spec = NULL) {

  z    <- if (scope == "city") zones_city else inp$zones
  keep <- if (scope == "city") CITY_SCHOOLS else inp$schools$name
  cost <- if (site == "now") inp$costs_now else inp$costs_elm

  d <- cost %>%
    filter(name %in% keep) %>%
    inner_join(z %>% select(zone, Oi), by = "zone") %>%
    filter(is.finite(cij), Oi > 0)

  if (is.null(w_spec)) {
    # Every school identical. The value is arbitrary — in a
    # production-constrained model with a common W it cancels in A_i —
    # but 100 is what the original used.
    d$Wj <- 100
  } else {
    w <- inp$attract %>% select(name, Wj = all_of(w_spec))
    stopifnot(all(keep %in% w$name))
    d <- d %>% inner_join(w, by = "name")
  }

  d <- prod_constrained_sim(d, o_col = "Oi", w_col = "Wj", c_col = "cij",
                            orig_col = "zone", dest_col = "name",
                            alpha = 1, beta = beta)

  d %>%
    group_by(name) %>%
    summarise(modelled = sum(sim_flow),
              mean_travel = weighted.mean(cij, sim_flow), .groups = "drop") %>%
    mutate(site = site, beta = beta, scope = scope,
           w_spec = w_spec %||% "identical")
}

# ====================================================================
# Accessibility and competition are not the same thing
# ====================================================================
# The modelled intake above is a market SHARE: in a production-
# constrained model the denominator is the origin's accessibility to
# every school, so a school's intake falls both when it is hard to
# reach and when it has rivals nearby. Reporting that single number as
# a measure of how well sited a school is conflates the two.
#
# They are separated here:
#
#   Potential (Hansen) accessibility  how many children are within
#     reach, distance-weighted:  A_j = sum_i O_i * c_ij^-beta
#     transportgeography.org/contents/methods/
#       transportation-accessibility/potaccessibility/
#
#   Competing destinations (Fotheringham 1983)  how much company the
#     school has:  C_j = sum_{k != j} d_jk^-sigma
#
# Dorothy Stringer and Longhill both come out low on modelled intake
# for opposite reasons, and only one of them has a location problem.

SIGMA <- 1.0

accessibility <- function(beta = BETA_ORIGINAL) {
  inp$costs_now %>%
    filter(name %in% CITY_SCHOOLS) %>%
    inner_join(zones_city %>% select(zone, Oi), by = "zone") %>%
    filter(is.finite(cij), cij > 0, Oi > 0) %>%
    group_by(name) %>%
    summarise(A_hansen = sum(Oi * cij^-beta), .groups = "drop")
}

sch_xy <- inp$schools %>% filter(name %in% CITY_SCHOOLS) %>%
  select(name, easting, northing)

#' Fotheringham's competing-destinations term
#'
#' C_j = sum_{k != j} W_k d_jk^-sigma
#'
#' The weights matter. In the distance-only world every school is
#' identical, so W_k drops out and the term is a pure count of how much
#' company a school has — which is what this file computed, correctly,
#' when the distance-only run was the only run. Once W_j is real, an
#' unweighted term says a school is equally crowded by a rival nobody
#' asks for as by one everybody does, which is not what competition
#' means. Both versions are computed and reported.
#'
#' @param w named vector of attractiveness, or NULL for identical schools
compete <- function(w = NULL) {
  pairs <- tidyr::expand_grid(name = sch_xy$name, k = sch_xy$name) %>%
    filter(name != k) %>%
    left_join(sch_xy, by = "name") %>%
    left_join(sch_xy %>% rename(k = name, ke = easting, kn = northing),
              by = "k") %>%
    mutate(d_km = pmax(euclid_m(easting, northing, ke, kn) / 1000, 0.1),
           Wk   = if (is.null(w)) 1 else unname(w[k]))
  stopifnot(!any(is.na(pairs$Wk)))
  pairs %>%
    group_by(name) %>%
    summarise(C_j = sum(Wk * d_km^-SIGMA), .groups = "drop")
}

competition <- compete()   # the distance-only world: W_k identical

W_WPREFS <- with(inp$attract, setNames(W_wprefs, name))
competition_w <- compete(W_WPREFS) %>% rename(C_j_w = C_j)


# --- The headline run, at the original beta --------------------------

pan <- inp$schools %>% select(name, pan2024, pan2026)

b_now <- brightopia("now", BETA_ORIGINAL) %>%
  left_join(pan, by = "name") %>%
  mutate(surplus = modelled - pan2024, fill = modelled / pan2024)

# The city has more places than children, so in Brightopia the average
# school sits below its PAN by construction. What matters is a school's
# position relative to that average, not the raw deficit.
CITY_PLACES   <- sum(pan$pan2024[pan$name %in% CITY_SCHOOLS])
CITY_CHILDREN <- sum(zones_city$Oi)
CITY_FILL     <- CITY_CHILDREN / CITY_PLACES

# Measuring a site against its own PAN confounds how well placed the
# school is with how big it was built — Dorothy Stringer scores worst on
# % of PAN only because its PAN is the largest in the city. The PAN-free
# measure is the school's Brightopia intake against an equal share of
# the cohort: what it would receive if every site were equally
# convenient. Below 100 means the site is harder to reach than average.
EQUAL_SHARE <- CITY_CHILDREN / length(CITY_SCHOOLS)

b_now <- b_now %>%
  mutate(share_index = modelled / EQUAL_SHARE) %>%
  left_join(accessibility(), by = "name") %>%
  left_join(competition, by = "name") %>%
  left_join(competition_w, by = "name") %>%
  mutate(access_index    = 100 * A_hansen / mean(A_hansen),
         compete_index   = 100 * C_j / mean(C_j),
         compete_index_w = 100 * C_j_w / mean(C_j_w),
         # kept under the old name so nothing downstream breaks, but it
         # is a market share and is no longer presented as a site score
         site_index = share_index)

message("\n=== Accessibility and competition, separated ===")
print(as.data.frame(b_now %>%
  transmute(School = name,
            `Children reachable` = round(access_index),
            `Rivals nearby` = round(compete_index),
            `Rivals nearby, weighted` = round(compete_index_w),
            `Brightopia intake` = round(100 * share_index),
            `Mean journey` = round(mean_travel, 1)) %>%
  arrange(desc(`Children reachable`))), row.names = FALSE)

message(sprintf("  Correlation between reach and crowding: %.2f",
                cor(log(b_now$access_index), log(b_now$compete_index))))
message(sprintf("  Unweighted against weighted crowding: %.2f",
                cor(log(b_now$compete_index), log(b_now$compete_index_w))))

message(sprintf("\nAt beta = %.1f: %d city schools, %s places, %s children — the average school fills to %.0f%%, and an equal share would be %.0f children.",
                BETA_ORIGINAL, length(CITY_SCHOOLS),
                format(CITY_PLACES, big.mark = ","),
                format(round(CITY_CHILDREN), big.mark = ","),
                100 * CITY_FILL, EQUAL_SHARE))
print(as.data.frame(b_now %>%
  transmute(School = name, `PAN 2024` = pan2024,
            Brightopia = round(modelled),
            `Children reachable` = round(access_index),
            `Rivals nearby` = round(compete_index),
            `% of PAN` = round(100 * fill)) %>%
  arrange(`Children reachable`)), row.names = FALSE)

# --- The same city, but with what families want put back in ----------
# Same geography, same routed times, same production constraint. The one
# change is that W_j is no longer identical: it is the rank-weighted
# published preference rate from 01a, and beta is the bundle's central
# 1.7 rather than the original's 1.5.
#
# This is the run that matches the equation as it is usually written,
# T_ij = A_i O_i W_j^alpha c_ij^-beta, with a W_j that varies. The
# distance-only run above is the same equation with W_j held constant,
# and the difference between the two is the whole of what attractiveness
# contributes.

b_wpref <- brightopia("now", BETA_REF, w_spec = "W_wprefs") %>%
  left_join(pan, by = "name") %>%
  left_join(competition_w, by = "name") %>%
  mutate(surplus     = modelled - pan2024,
         fill        = modelled / pan2024,
         share_index = modelled / EQUAL_SHARE,
         compete_index_w = 100 * C_j_w / mean(C_j_w))

# The distance-only run above is at the ORIGINAL beta of 1.5, because
# reproducing the original is what it is for. Comparing it against the
# demand run would therefore vary beta and W at once and report the sum
# as though it were the effect of W. So the comparison is made against a
# distance-only run at the same beta, and the only difference between
# the two columns is what families want.
b_geog_ref <- brightopia("now", BETA_REF) %>%
  left_join(pan, by = "name") %>%
  left_join(accessibility(BETA_REF), by = "name") %>%
  left_join(competition, by = "name") %>%
  mutate(surplus       = modelled - pan2026,
         fill          = modelled / pan2026,
         share_index   = modelled / EQUAL_SHARE,
         access_index  = 100 * A_hansen / mean(A_hansen),
         compete_index = 100 * C_j / mean(C_j))

b_compare <- b_geog_ref %>%
  select(name, geog_only = modelled, access_index, compete_index,
         pan2024, pan2026) %>%
  left_join(b_wpref %>% select(name, with_demand = modelled), by = "name") %>%
  mutate(shift = with_demand - geog_only,
         shift_pct = 100 * shift / geog_only)

message(sprintf(
  "\n=== Geography alone against geography plus demand, both at beta %.1f ===",
  BETA_REF))
print(as.data.frame(b_compare %>%
  arrange(shift) %>%
  transmute(School = name, `PAN 2026` = pan2026,
            `Geography only` = round(geog_only),
            `With demand` = round(with_demand),
            Shift = round(shift),
            `%` = sprintf("%+.0f", shift_pct))), row.names = FALSE)

message(sprintf("  Schools that geography alone would fill to PAN 2026: %d of %d",
                sum(b_compare$geog_only >= b_compare$pan2026), nrow(b_compare)))
message(sprintf("  Schools that geography plus demand would fill:        %d of %d",
                sum(b_compare$with_demand >= b_compare$pan2026), nrow(b_compare)))


# --- How far does geography alone actually get you? ------------------
# The distance-only model against the offers the council actually made.
# The year has to match and it is easy to get wrong: Brightopia's Oi is
# the entry cohort for OBS_YEAR, so the offers it is compared against
# must be that same round rather than whichever round happens to be in
# the admissions extract. (03's own validation compares a 2026 modelled
# intake against the 2024 offers in that extract; this does not.)
#
# Nothing is capped here, so a school whose modelled intake exceeds its
# admission number is showing demand the real round would have turned
# away. Read the shares, not just the counts.

OBS_YEAR <- inp$demand_ts %>%
  filter(area == "Brighton & Hove") %>%
  slice_min(abs(state_demand - CITY_CHILDREN), n = 1) %>%
  pull(entry_year)

fs_obs <- readRDS(file.path(PUBLIC_OUT, "factsheet_panel.rds"))$factsheets %>%
  filter(name != "Total", year == OBS_YEAR) %>%
  select(name, observed = off_total)

b_obs <- b_now %>%                       # the original: W identical, beta 1.5
  select(name, pan2026, modelled_15 = modelled) %>%
  left_join(b_geog_ref %>% select(name, modelled_17 = modelled), by = "name") %>%
  left_join(b_wpref  %>% select(name, modelled_w = modelled), by = "name") %>%
  inner_join(fs_obs, by = "name") %>%
  mutate(diff_15    = modelled_15 - observed,
         diff_17    = modelled_17 - observed,
         diff_w     = modelled_w  - observed,
         share_mod  = 100 * modelled_17 / sum(modelled_17),
         share_obs  = 100 * observed / sum(observed),
         share_diff = share_mod - share_obs,
         # A school that offered its admission number or more was
         # rationing places. Its observed figure is a ceiling, not a
         # measure of demand, so an uncapped model SHOULD exceed it and
         # the two groups have to be judged separately.
         at_ceiling = observed >= pan2026)

stopifnot(nrow(b_obs) == length(CITY_SCHOOLS))

message(sprintf(
  "\n=== The distance-only model against the %d offers actually made (%d round) ===",
  sum(b_obs$observed), OBS_YEAR))
print(as.data.frame(b_obs %>%
  arrange(diff_17) %>%
  transmute(School = name, `PAN 2026` = pan2026,
            Observed = observed,
            `Brightopia b=1.5` = round(modelled_15),
            `Brightopia b=1.7` = round(modelled_17),
            Diff = sprintf("%+.0f", diff_17),
            `Share obs %` = round(share_obs, 1),
            `Share mod %` = round(share_mod, 1))), row.names = FALSE)

message(sprintf(
  "  Geography alone against observed: R2 = %.3f, RMSE = %.0f (beta %.1f); R2 = %.3f, RMSE = %.0f (beta %.1f)",
  CalcRSquared(b_obs$observed, b_obs$modelled_17),
  CalcRMSE(b_obs$observed, b_obs$modelled_17), BETA_REF,
  CalcRSquared(b_obs$observed, b_obs$modelled_15),
  CalcRMSE(b_obs$observed, b_obs$modelled_15), BETA_ORIGINAL))
message(sprintf("  Largest over-prediction: %s (%+.0f); largest under: %s (%+.0f)",
                b_obs$name[which.max(b_obs$diff_17)], max(b_obs$diff_17),
                b_obs$name[which.min(b_obs$diff_17)], min(b_obs$diff_17)))

# Does adding W_j move the model towards the offers actually made? Only
# the schools that were NOT rationing places can answer: for the rest
# the observed figure is a ceiling, so an uncapped model exceeding it is
# not an error.
message("\n  Does adding W_j close the gap to observed? (uncapped schools only)")
print(as.data.frame(b_obs %>%
  filter(!at_ceiling) %>%
  arrange(abs(diff_w)) %>%
  transmute(School = name, Observed = observed,
            `Geography only` = round(modelled_17), `Gap` = sprintf("%+.0f", diff_17),
            `With demand` = round(modelled_w), `Gap ` = sprintf("%+.0f", diff_w),
            Closer = if_else(abs(diff_w) < abs(diff_17), "yes", "no"))),
  row.names = FALSE)

b_unc <- b_obs %>% filter(!at_ceiling)
message(sprintf(
  "  Mean absolute gap on those %d: %.0f on geography alone, %.0f with demand (%d of %d closer)",
  nrow(b_unc), mean(abs(b_unc$diff_17)), mean(abs(b_unc$diff_w)),
  sum(abs(b_unc$diff_w) < abs(b_unc$diff_17)), nrow(b_unc)))


# --- The same, with Longhill at the top of Elm Grove -----------------

# Relocating changes Longhill's own accessibility, so it is recomputed
# on the moved site rather than carried over.
acc_elm <- inp$costs_elm %>%
  filter(name %in% CITY_SCHOOLS) %>%
  inner_join(zones_city %>% select(zone, Oi), by = "zone") %>%
  filter(is.finite(cij), cij > 0, Oi > 0) %>%
  group_by(name) %>%
  summarise(A_hansen = sum(Oi * cij^-BETA_ORIGINAL), .groups = "drop")

b_elm <- brightopia("elm", BETA_ORIGINAL) %>%
  left_join(pan, by = "name") %>%
  left_join(acc_elm, by = "name") %>%
  mutate(surplus = modelled - pan2024, fill = modelled / pan2024,
         share_index = modelled / EQUAL_SHARE,
         site_index = share_index,
         access_index = 100 * A_hansen / mean(A_hansen))

message(sprintf("\nWith Longhill relocated to the top of Elm Grove (beta = %.1f):",
                BETA_ORIGINAL))
print(as.data.frame(b_elm %>%
  transmute(School = name, `PAN 2024` = pan2024,
            Brightopia = round(modelled),
            `Children reachable` = round(access_index),
            `Mean journey` = round(mean_travel, 1),
            `% of PAN` = round(100 * fill)) %>%
  arrange(`Children reachable`)), row.names = FALSE)

lh_now <- b_now$modelled[b_now$name == LH]
lh_elm <- b_elm$modelled[b_elm$name == LH]
message(sprintf("\nLonghill: %.0f children where it is, %.0f at Elm Grove (%+.0f, %+.0f%%)",
                lh_now, lh_elm, lh_elm - lh_now, 100 * (lh_elm / lh_now - 1)))

# --- Does the conclusion survive the choice of beta? -----------------

sweep <- purrr::map_dfr(BETAS_B, function(b) {
  bind_rows(brightopia("now", b), brightopia("elm", b))
}) %>%
  left_join(pan, by = "name") %>%
  mutate(fill = modelled / pan2024)

lh_sweep <- sweep %>% filter(name == LH) %>%
  select(site, beta, modelled, fill) %>%
  tidyr::pivot_wider(names_from = site, values_from = c(modelled, fill)) %>%
  mutate(gain = modelled_elm - modelled_now)

message("\nAcross the whole beta range, Longhill in Brightopia:")
print(as.data.frame(lh_sweep %>%
  filter(beta %in% c(0.5, 1.0, 1.5, 2.0, 2.5, 3.0)) %>%
  transmute(beta,
            `at Ovingdean` = round(modelled_now),
            `% of PAN` = round(100 * fill_now),
            `at Elm Grove` = round(modelled_elm),
            `% of PAN ` = round(100 * fill_elm),
            gain = round(gain))), row.names = FALSE)

message(sprintf("\nLonghill is below its 2024 PAN in %.0f%% of the beta range where it stands, %.0f%% at Elm Grove.",
                100 * mean(lh_sweep$fill_now < 1),
                100 * mean(lh_sweep$fill_elm < 1)))
message(sprintf("Relocation raises its distance-only intake at every beta tested: %s",
                if (all(lh_sweep$gain > 0)) "yes" else "no"))

# How badly placed is Longhill relative to the other schools? Measured
# against the city average fill, not against its own PAN.
rank_now <- sweep %>% filter(site == "now", name %in% CITY_SCHOOLS) %>%
  group_by(beta) %>% mutate(rk = rank(fill)) %>% ungroup() %>%
  filter(name == LH)
message(sprintf("Its fill rank among the %d city schools: %s (1 = worst placed)",
                length(CITY_SCHOOLS),
                paste(sort(unique(rank_now$rk)), collapse = ", ")))
message(sprintf("Relative to the city average of %.0f%%: %.0f%% where it stands, %.0f%% at Elm Grove.",
                100 * CITY_FILL,
                100 * b_now$fill[b_now$name == LH],
                100 * b_elm$fill[b_elm$name == LH]))

# --- The expanded authority ------------------------------------------

b_exp <- brightopia("now", BETA_ORIGINAL, scope = "expanded") %>%
  left_join(pan, by = "name") %>%
  mutate(surplus = modelled - pan2024)

message("\nWith the 2028 boundary and Peacehaven Community School included:")
print(as.data.frame(b_exp %>%
  transmute(School = name, `PAN 2024` = pan2024,
            Brightopia = round(modelled),
            `Surplus/deficit` = round(surplus)) %>%
  arrange(`Surplus/deficit`)), row.names = FALSE)


# ====================================================================
# Figures
# ====================================================================

theme_b <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

fig_dat <- bind_rows(
  b_now %>% mutate(site_label = "Where the schools are"),
  b_elm %>% mutate(site_label = "Longhill at Elm Grove")
) %>%
  mutate(site_label = factor(site_label,
           levels = c("Where the schools are", "Longhill at Elm Grove")),
         short = str_replace_all(name, c(
           " Community Academy" = "", " Catholic School" = "",
           " School" = "", " High" = "", " Mill" = "")),
         is_lh = name == LH)

p1 <- ggplot(fig_dat, aes(reorder(short, surplus), surplus, fill = is_lh)) +
  geom_col() +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  coord_flip() +
  facet_wrap(~ site_label) +
  scale_fill_manual(values = c(`TRUE` = "#C2413B", `FALSE` = "grey65"),
                    guide = "none") +
  labs(title = "If the only thing that mattered was the journey",
       subtitle = sprintf("Modelled intake less 2024 PAN, all schools identically attractive, beta = %.1f",
                          BETA_ORIGINAL),
       x = NULL, y = "Children above (+) or below (-) PAN") +
  theme_b

ggsave(file.path(PUBLIC_OUT, "fig_brightopia_surplus.png"), p1,
       width = 10, height = 5, dpi = 150)

p2 <- lh_sweep %>%
  select(beta, Ovingdean = fill_now, `Elm Grove` = fill_elm) %>%
  tidyr::pivot_longer(-beta, names_to = "site", values_to = "fill") %>%
  ggplot(aes(beta, 100 * fill, colour = site)) +
  geom_hline(yintercept = 100, linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = 1) +
  annotate("text", x = max(BETAS_B), y = 103, label = "2024 PAN",
           hjust = 1, size = 3, colour = "grey40") +
  scale_colour_manual(values = c(Ovingdean = "#C2413B", `Elm Grove` = "#2C6E9B")) +
  expand_limits(y = 0) +
  labs(title = "Longhill in Brightopia, across every distance decay tested",
       subtitle = "Distance-only model: no attainment, no reputation, no catchment, no faith",
       x = "Distance decay (beta)", y = "Modelled intake as % of 2024 PAN",
       colour = NULL) +
  theme_b

ggsave(file.path(PUBLIC_OUT, "fig_brightopia_beta.png"), p2,
       width = 9, height = 5, dpi = 150)


# ====================================================================
# Save
# ====================================================================

saveRDS(list(
  at_original_beta = b_now,
  geog_only_at_ref = b_geog_ref,
  with_demand      = b_wpref,
  demand_compare   = b_compare,
  observed_compare = b_obs,
  observed_year    = OBS_YEAR,
  relocated        = b_elm,
  expanded         = b_exp,
  sweep            = sweep,
  lh_sweep         = lh_sweep,
  beta_original    = BETA_ORIGINAL,
  beta_ref         = BETA_REF,
  w_spec_demand    = "W_wprefs",
  betas            = BETAS_B,
  city_schools     = CITY_SCHOOLS,
  city_children    = CITY_CHILDREN,
  city_places      = CITY_PLACES,
  city_fill        = CITY_FILL,
  equal_share      = EQUAL_SHARE,
  sigma            = SIGMA,
  competition      = competition,
  competition_w    = competition_w,
  run_at           = Sys.time()
), file.path(PUBLIC_OUT, "brightopia.rds"))

readr::write_csv(sweep, file.path(PUBLIC_OUT, "brightopia_sweep.csv"))

message("\nSaved public/output/brightopia.rds")
