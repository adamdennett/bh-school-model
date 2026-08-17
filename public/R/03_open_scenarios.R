# 03_open_scenarios.R — The full analysis, on open parameters, with bands
# =======================================================================
# Reproduces the substantive scenario work from published sources only.
#
# There is no calibrated parameter here. Where a single number is needed
# to draw a chart, the CENTRAL SPECIFICATION is used — the midpoint of
# each swept range, chosen for legibility and nothing else — and every
# headline figure is reported with the band produced by re-running it
# across a reduced factorial of the same envelope. No result in this file
# depends on a value that could not be defended from open data.
#
#   beta   central 2.0   band {1.4, 2.0, 2.6}   (full sweep 1.0-3.2 in 02)
#   gamma  central 1.2   band {0.4, 1.2, 2.0}   (full sweep 0-2.4 in 02)
#   W      central published first preferences
#          band {PAN, published first preferences, Attainment 8}
#
# Output: public/output/open_scenarios.rds, public/output/os_*.csv
# =======================================================================

source(here::here("public", "R", "00_open_core.R"))

inp <- readRDS(file.path(PUBLIC_OUT, "open_inputs.rds"))

LH   <- "Longhill High School"
PCHS <- "Peacehaven Community School"
DS   <- "Dorothy Stringer School"

BETA_C  <- 2.0;  BETA_BAND  <- c(1.4, 2.0, 2.6)
GAMMA_C <- 1.2;  GAMMA_BAND <- c(0.4, 1.2, 2.0)
W_C     <- "W_prefs"; W_BAND <- c("W_pan", "W_prefs", "W_att8")

YEARS <- c(2026, 2028, 2030, 2033, 2035)
PANS  <- c(240, 210, 180, 150, 120)   # 210 is in force for 2026/27

zones     <- inp$zones
attract   <- inp$attract
sch_catch <- purrr::imap_dfr(inp$catchment_schools, ~ tibble(name = .x, sch_catch = .y))


# ====================================================================
# 1. What are families choosing? (open replication)
# ====================================================================

message("\n=== 1. Published demand against published performance ===")

panel <- read_open_rds(OPEN$panel)
adm   <- read_open_csv(OPEN$admissions)

perf_open <- panel %>%
  filter(URN %in% as.character(inp$schools$urn)) %>%
  group_by(urn = as.numeric(URN)) %>%
  summarise(att8 = mean(ATT8SCR, na.rm = TRUE),
            va   = mean(residual_ATT8SCR_imputed, na.rm = TRUE),
            fsm  = mean(PTFSM6CLA1A, na.rm = TRUE),
            absence = mean(PERCTOT, na.rm = TRUE), .groups = "drop")

choice <- adm %>%
  transmute(urn, school, pan = pan2024, first_pref = first_pref_count) %>%
  left_join(perf_open, by = "urn") %>%
  mutate(prefs_per_place = first_pref / pan, log_ppp = log(prefs_per_place))

choice_models <- list(
  "Attainment 8"          = lm(log_ppp ~ att8, data = choice),
  "Value-added"           = lm(log_ppp ~ va,   data = choice),
  "Attainment 8 + value-added" = lm(log_ppp ~ att8 + va, data = choice)
)

choice_tbl <- purrr::imap_dfr(choice_models, function(m, nm) {
  s <- summary(m)
  tibble(spec = nm, r2 = s$r.squared,
         terms = paste(sprintf("%s = %+.4f", names(coef(m))[-1], coef(m)[-1]),
                       collapse = "; "))
})
print(as.data.frame(choice_tbl %>% mutate(r2 = round(r2, 3))), row.names = FALSE)


# ====================================================================
# 2. The 2028 boundary — point in polygon
# ====================================================================

message("\n=== 2. Which schools join the city in 2028? ===")

wards <- sf::st_read(assert_open(OPEN$wards), quiet = TRUE) %>%
  filter(WD25CD %in% names(LGR_WARDS_OPEN)) %>%
  sf::st_transform(27700) %>% sf::st_make_valid()

gias <- suppressMessages(readxl::read_excel(assert_open(OPEN$gias)))
names(gias) <- make.names(names(gias))

sec <- gias %>%
  filter(EstablishmentStatus..name. == "Open",
         grepl("Secondary|All-through", PhaseOfEducation..name., ignore.case = TRUE),
         suppressWarnings(as.numeric(StatutoryLowAge)) <= 11,
         LA..name. %in% c("Brighton and Hove", "East Sussex"),
         !is.na(Easting), !is.na(Northing)) %>%
  transmute(urn = as.numeric(URN), name = EstablishmentName, la = LA..name.,
            postcode = Postcode, roll = suppressWarnings(as.numeric(NumberOfPupils)),
            easting = as.numeric(Easting), northing = as.numeric(Northing))

pts <- sf::st_as_sf(sec, coords = c("easting", "northing"), crs = 27700, remove = FALSE)
ward_union <- sf::st_union(wards)

sec <- sec %>%
  mutate(joins = lengths(sf::st_within(pts, ward_union)) > 0,
         dist_km = as.numeric(sf::st_distance(pts, sf::st_boundary(ward_union))) / 1000)

pip <- sec %>%
  filter(joins | dist_km < 12) %>%
  arrange(desc(joins), dist_km) %>%
  select(name, la, postcode, roll, joins, dist_km)

print(as.data.frame(pip %>% mutate(dist_km = round(dist_km, 2))), row.names = FALSE)


# ====================================================================
# 3. Scenario engine
# ====================================================================

demand_for <- function(year) {
  tot <- inp$demand_ts %>% filter(entry_year == year) %>%
    group_by(area) %>% summarise(t = sum(state_demand), .groups = "drop")
  zones %>% group_by(area) %>% mutate(share = Oi / sum(Oi)) %>% ungroup() %>%
    left_join(tot, by = "area") %>%
    transmute(zone, catchment, Oi = share * t)
}
demand_cache <- purrr::set_names(purrr::map(YEARS, demand_for), YEARS)

run_open <- function(costs, pans, catch_map, year,
                     beta = BETA_C, gamma = GAMMA_C, w_spec = W_C) {

  d <- costs %>%
    left_join(demand_cache[[as.character(year)]] %>% select(zone, Oi), by = "zone") %>%
    left_join(catch_map, by = "zone") %>%
    left_join(sch_catch, by = "name") %>%
    left_join(attract %>% select(name, W = all_of(w_spec)), by = "name") %>%
    mutate(in_catchment = as.integer(!is.na(sch_catch) & sch_catch == catchment)) %>%
    filter(!is.na(Oi), Oi > 0, is.finite(cij), !is.na(W))

  d <- sim_faith_split(d, o_col = "Oi", w_col = "W", c_col = "cij",
                       orig_col = "zone", dest_col = "name",
                       alpha = 1, beta = beta,
                       extra_utility = gamma * d$in_catchment)

  cap <- pans[names(pans) %in% unique(d$name)]
  d <- ipf_capacity(d, cap, orig_col = "zone", dest_col = "name",
                    flow_col = "sim_flow", o_col = "Oi")

  d %>% group_by(name) %>%
    summarise(intake = sum(sim_flow_capped),
              from_new = sum(sim_flow_capped[catchment == "Peacehaven"]),
              mean_travel = weighted.mean(cij, sim_flow_capped), .groups = "drop") %>%
    mutate(entry_year = year, pan = unname(cap[name]), fill = intake / pan,
           city_mean_travel = weighted.mean(d$cij, d$sim_flow_capped))
}

pan_vec <- function(lh = 240, ds = NULL, free_lh = FALSE) {
  p <- setNames(inp$schools$pan2026, inp$schools$name)
  p[LH] <- if (free_lh) 9999 else lh
  if (!is.null(ds)) p[DS] <- ds
  p
}

current_map <- zones %>% select(zone, catchment)


# ====================================================================
# 4. Catchment design (capacitated power diagram)
# ====================================================================

design_open <- function(costs, pans, tol = 0.03, min_abs = 15,
                        max_iter = 3000, eta0 = 0.8) {
  areas <- inp$catchment_schools
  area_cost <- purrr::imap_dfr(areas, function(members, aname) {
    costs %>% filter(name %in% members) %>%
      group_by(zone) %>% summarise(cost = min(cij), .groups = "drop") %>%
      mutate(catchment = aname)
  }) %>% tidyr::pivot_wider(names_from = catchment, values_from = cost) %>%
    arrange(match(zone, zones$zone))

  C   <- as.matrix(area_cost[, names(areas), drop = FALSE])
  Oi  <- zones$Oi[match(area_cost$zone, zones$zone)]
  cap <- purrr::map_dbl(areas, ~ sum(pans[.x], na.rm = TRUE))
  target <- cap / sum(cap) * sum(Oi)
  band <- pmax(tol * target, min_abs)

  price <- rep(0, ncol(C)); names(price) <- colnames(C)
  scale <- mean(C) / 10
  best <- list(score = Inf)

  for (it in seq_len(max_iter)) {
    eta <- eta0 / (1 + it / 200)
    idx <- max.col(-sweep(C, 2, price, "+"), ties.method = "first")
    assigned <- tapply(Oi, factor(colnames(C)[idx], levels = colnames(C)), sum, default = 0)
    excess <- assigned - target[colnames(C)]
    score <- max(abs(excess) / band[colnames(C)])
    if (score < best$score) best <- list(score = score, idx = idx, assigned = assigned,
                                         excess = excess, iter = it)
    if (score <= 1) break
    price <- price + eta * (excess / target[colnames(C)]) * scale
  }

  list(assignment = tibble(zone = area_cost$zone, catchment = colnames(C)[best$idx]),
       target = target, assigned = best$assigned, excess = best$excess,
       worst = best$score, iterations = best$iter)
}

message("\n=== 3. Designing catchments (open inputs) ===")
des_now <- design_open(inp$costs_now, pan_vec(150, ds = 270))
des_elm <- design_open(inp$costs_elm, pan_vec(150, ds = 270))
message(sprintf("  Ovingdean design: worst gap %.2f band-widths in %d iters", des_now$worst, des_now$iterations))
message(sprintf("  Elm Grove design: worst gap %.2f band-widths in %d iters", des_elm$worst, des_elm$iterations))


# ====================================================================
# 5. Configurations, at the central spec and across the band
# ====================================================================

CONFIGS <- list(
  list(id = "A", label = "Today: Ovingdean, PAN 210, current catchments",
       costs = inp$costs_now, pans = pan_vec(210), map = current_map),
  list(id = "B", label = "Shrink only: Ovingdean, PAN 150",
       costs = inp$costs_now, pans = pan_vec(150), map = current_map),
  list(id = "C", label = "Move only: Elm Grove, PAN 210",
       costs = inp$costs_elm, pans = pan_vec(210), map = current_map),
  list(id = "D", label = "Shrink + move: Elm Grove, PAN 150",
       costs = inp$costs_elm, pans = pan_vec(150), map = current_map),
  list(id = "E", label = "Shrink + move + redrawn catchments",
       costs = inp$costs_elm, pans = pan_vec(150, ds = 270), map = des_elm$assignment),
  list(id = "F", label = "Redrawn catchments, Longhill stays at Ovingdean",
       costs = inp$costs_now, pans = pan_vec(150, ds = 270), map = des_now$assignment),
  list(id = "G", label = "Elm Grove, PAN 120, redrawn catchments",
       costs = inp$costs_elm, pans = pan_vec(120, ds = 270), map = des_elm$assignment)
)

message("\n=== 4. Running configurations ===")

central <- purrr::map_dfr(CONFIGS, function(cf) {
  purrr::map_dfr(YEARS, ~ run_open(cf$costs, cf$pans, cf$map, .x) %>%
                   mutate(config = paste0(cf$id, ". ", cf$label)))
})

# Bands: re-run the reduced factorial for Longhill only
band_grid <- tidyr::expand_grid(cfg = seq_along(CONFIGS), year = YEARS,
                                beta = BETA_BAND, gamma = GAMMA_BAND, w = W_BAND)
message("  Band runs: ", nrow(band_grid))

t0 <- Sys.time()
bands <- purrr::pmap_dfr(band_grid, function(cfg, year, beta, gamma, w) {
  cf <- CONFIGS[[cfg]]
  run_open(cf$costs, cf$pans, cf$map, year, beta = beta, gamma = gamma, w_spec = w) %>%
    filter(name == LH) %>%
    transmute(config = paste0(cf$id, ". ", cf$label), entry_year = year,
              beta, gamma, w_spec = w, intake, fill)
})
message(sprintf("  done in %.0f s", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

lh_band <- bands %>%
  group_by(config, entry_year) %>%
  summarise(lo = min(fill), med = median(fill), hi = max(fill), .groups = "drop") %>%
  left_join(central %>% filter(name == LH) %>% select(config, entry_year, central = fill),
            by = c("config", "entry_year"))

message("\n  Longhill fill rate (%), central estimate with band:")
print(as.data.frame(
  lh_band %>% filter(entry_year %in% c(2026, 2030, 2035)) %>%
    mutate(txt = sprintf("%.0f (%.0f-%.0f)", 100*pmin(central,1),
                         100*pmin(lo,1), 100*pmin(hi,1))) %>%
    select(config, entry_year, txt) %>%
    tidyr::pivot_wider(names_from = entry_year, values_from = txt)),
  row.names = FALSE)


# ====================================================================
# 6. Natural recruitment, and what PAN it supports
# ====================================================================

natural <- purrr::map_dfr(CONFIGS, function(cf) {
  p <- cf$pans; p[LH] <- 9999
  purrr::map_dfr(YEARS, ~ run_open(cf$costs, p, cf$map, .x) %>%
                   filter(name == LH) %>%
                   transmute(config = paste0(cf$id, ". ", cf$label),
                             entry_year = .x, natural = intake))
})

message("\n  Natural recruitment at the central specification:")
print(as.data.frame(natural %>% mutate(natural = round(natural)) %>%
  tidyr::pivot_wider(names_from = entry_year, values_from = natural)), row.names = FALSE)


# ====================================================================
# 7. Validation against published offers
# ====================================================================

pub_offers <- adm %>% transmute(name = school, observed = total_offer_count) %>%
  mutate(name = dplyr::recode(name,
    "Hove Park School and Sixth Form Centre" = "Hove Park School"))

val <- central %>%
  filter(grepl("^A\\.", config), entry_year == 2026) %>%
  select(name, modelled = intake) %>%
  inner_join(pub_offers, by = "name")

message(sprintf("\n=== 5. Validation against published offers: R2 = %.3f, RMSE = %.0f ===",
                CalcRSquared(val$observed, val$modelled),
                CalcRMSE(val$observed, val$modelled)))
print(as.data.frame(val %>% mutate(across(where(is.numeric), round),
                                   diff = modelled - observed)), row.names = FALSE)


# ====================================================================
# 8. Longhill finances (open, DfE returns)
# ====================================================================

lh_fin <- panel %>%
  filter(URN == "114581") %>%
  select(year_label, roll = TOTPUPS, income_pp = fin_total_income_pp,
         expenditure_pp = fin_total_expenditure_pp,
         balance = fin_in_year_balance, reserve = fin_revenue_reserve) %>%
  arrange(year_label)

size_fin <- panel %>%
  filter(MINORGROUP %in% c("Academy", "Maintained school"),
         !is.na(fin_total_income_pp), !is.na(TOTPUPS), TOTPUPS > 150,
         fin_total_income_pp > 2000, fin_total_income_pp < 20000) %>%
  mutate(size_band = cut(TOTPUPS, c(0, 500, 700, 900, 1100, 1400, 1800, 1e5),
                         labels = c("<500","500-700","700-900","900-1,100",
                                    "1,100-1,400","1,400-1,800","1,800+"))) %>%
  group_by(size_band) %>%
  summarise(n = n(), income_pp = median(fin_total_income_pp),
            pct_deficit = 100 * mean(fin_in_year_balance < 0, na.rm = TRUE),
            .groups = "drop")

reserve_burn <- (lh_fin$reserve[1] - lh_fin$reserve[nrow(lh_fin)]) / (nrow(lh_fin) - 1)
years_left <- lh_fin$reserve[nrow(lh_fin)] / reserve_burn

message(sprintf("\n=== 6. Longhill finances: reserves fell %s/yr; %.1f years left ===",
                scales::comma(round(reserve_burn)), years_left))


# ====================================================================
# 9. Save
# ====================================================================

saveRDS(list(
  choice = choice, choice_tbl = choice_tbl, choice_models = choice_models,
  pip = pip, wards = wards,
  central = central, bands = bands, lh_band = lh_band, natural = natural,
  val = val, lh_fin = lh_fin, size_fin = size_fin,
  reserve_burn = reserve_burn, years_left = years_left,
  des_now = des_now, des_elm = des_elm,
  configs = purrr::map_chr(CONFIGS, ~ paste0(.x$id, ". ", .x$label)),
  spec = list(beta = BETA_C, gamma = GAMMA_C, w = W_C,
              beta_band = BETA_BAND, gamma_band = GAMMA_BAND, w_band = W_BAND),
  years = YEARS, pans = PANS, run_at = Sys.time()
), file.path(PUBLIC_OUT, "open_scenarios.rds"))

readr::write_csv(central, file.path(PUBLIC_OUT, "os_central.csv"))
readr::write_csv(lh_band, file.path(PUBLIC_OUT, "os_longhill_bands.csv"))
readr::write_csv(pip,     file.path(PUBLIC_OUT, "os_point_in_polygon.csv"))

message("\nSaved public/output/open_scenarios.rds and os_*.csv")
