# 01_open_inputs.R — Build the model inputs from published sources only
# =====================================================================
# Produces everything the model needs without touching a single pupil
# record:
#
#   zones        LSOAs in the expanded authority, with cohort-aged child
#                populations from ONS mid-2022 estimates
#   catchments   each zone's current catchment, by point in polygon
#                against the council's published catchment map
#   costs        zone-to-school walk/bus travel minutes
#   attract      four alternative attractiveness specifications, so the
#                sensitivity analysis can test whether the choice matters
#
# Output: public/output/open_inputs.rds
# =====================================================================

source(here::here("public", "R", "00_open_core.R"))

message("\n=== 1. Zones and populations ===")

# --- Postcodes, from ONSPD ------------------------------------------
onspd <- read_open_csv(
  OPEN$onspd,
  col_types = readr::cols_only(pcds = "c", doterm = "c", oslaua = "c",
                               osward = "c", lsoa21 = "c",
                               oseast1m = "d", osnrth1m = "d")
) %>%
  filter(is.na(doterm)) %>%
  mutate(postcode = normalise_pcd(pcds))

in_scope <- onspd %>%
  filter(oslaua == "E06000043" | osward %in% names(LGR_WARDS_OPEN)) %>%
  mutate(area = if_else(osward %in% names(LGR_WARDS_OPEN),
                        "Expansion area", "Brighton & Hove"))

# --- Catchment, assigned per postcode --------------------------------
# Catchment is assigned to postcodes rather than to whole LSOAs. The
# 2024 consultation redrew the Longhill/Stringer-Varndean boundary, and
# that boundary runs through LSOAs: assigning each LSOA wholesale to the
# catchment containing its centroid registers only 127 of the children
# the redraw actually moves, against 429 postcodes (8.4% of the city).
# Origin zones below are therefore LSOA x catchment, as in the private
# model, so a split LSOA contributes to both.

message("\n=== 1b. Catchment, by postcode ===")

pcd_pts <- sf::st_as_sf(in_scope, coords = c("oseast1m", "osnrth1m"),
                        crs = 27700, remove = FALSE)

assign_catchment <- function(g) {
  ix  <- as.integer(sf::st_within(pcd_pts, g))
  out <- g$catchment[ix]
  # Postcodes just outside the mapped area (the map is clipped to the
  # coast and the downs) take the nearest catchment.
  gap <- is.na(out) & in_scope$area == "Brighton & Hove"
  if (any(gap)) out[gap] <- g$catchment[sf::st_nearest_feature(pcd_pts[gap, ], g)]
  out
}

for (rg in names(CATCHMENT_REGIMES)) {
  in_scope[[paste0("catch_", rg)]] <- assign_catchment(read_catchments(rg))
}

CATCH_REGIME <- Sys.getenv("OPEN_REGIME", "optionZ")
stopifnot(CATCH_REGIME %in% names(CATCHMENT_REGIMES))

in_scope <- in_scope %>%
  mutate(across(starts_with("catch_"),
                ~ if_else(area == "Expansion area", "Peacehaven", .x))) %>%
  filter(!is.na(catch_pre2024), !is.na(catch_optionZ))

changed <- with(in_scope, sum(catch_pre2024 != catch_optionZ))
message("  Regime in use: ", CATCHMENT_REGIMES[[CATCH_REGIME]]$label)
message(sprintf("  Postcodes changing catchment between regimes: %d of %d (%.1f%%)",
                changed, nrow(in_scope), 100 * changed / nrow(in_scope)))

# Zones are LSOA x (pre-2024 catchment, option Z catchment). Splitting on
# the pair rather than on one regime means every zone has a well-defined
# catchment under either map and a single set of journey times, so the two
# regimes can be compared on identical geography. Switching regime then
# only changes which column drives catchment priority.
zones <- in_scope %>%
  group_by(lsoa = lsoa21, catch_pre2024, catch_optionZ) %>%
  summarise(zone_e = mean(oseast1m), zone_n = mean(osnrth1m),
            n_pcd  = n(),
            area   = names(sort(table(area), decreasing = TRUE))[1],
            .groups = "drop") %>%
  filter(!is.na(lsoa)) %>%
  mutate(zone = paste(lsoa, catch_pre2024, catch_optionZ, sep = "|"),
         catchment = .data[[paste0("catch_", CATCH_REGIME)]])

in_scope <- in_scope %>%
  mutate(zone = paste(lsoa21, catch_pre2024, catch_optionZ, sep = "|"),
         catchment = .data[[paste0("catch_", CATCH_REGIME)]])

message("  Zones (LSOA x catchment pair): ", nrow(zones),
        " from ", dplyr::n_distinct(zones$lsoa), " LSOAs",
        " (", sum(zones$area == "Expansion area"), " in the expansion area)")
message("  LSOAs split: ", sum(table(zones$lsoa) > 1),
        " | zones that change catchment between regimes: ",
        sum(zones$catch_pre2024 != zones$catch_optionZ))

# --- Cohort-aged child populations, ONS mid-2022 ---------------------
# A child aged `a` in mid-2022 enters Year 7 in 2022 + (11 - a), so ages
# 0-11 give entry years 2022-2033 from children already counted.

sape <- suppressMessages(readxl::read_excel(assert_open(OPEN$sape),
                                            sheet = "Mid-2022 Ward 2023", skip = 3))

ward_ages <- sape %>%
  filter(`LAD 2023 Code` == "E06000043" | `Ward 2023 Code` %in% names(LGR_WARDS_OPEN)) %>%
  select(ward = `Ward 2023 Code`, matches("^[FM]([0-9]|1[0-1])$")) %>%
  tidyr::pivot_longer(-ward, names_to = "k", values_to = "n") %>%
  mutate(age = as.integer(sub("^[FM]", "", k)), n = as.numeric(n)) %>%
  filter(age <= 11) %>%
  group_by(ward, age) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  mutate(entry_year = 2022L + (11L - age),
         area = if_else(ward %in% names(LGR_WARDS_OPEN),
                        "Expansion area", "Brighton & Hove"))

cohort <- ward_ages %>%
  group_by(area, entry_year) %>%
  summarise(children = sum(n), .groups = "drop") %>%
  mutate(extrapolated = FALSE)

# 2034-2035 are not yet born in a mid-2022 estimate. Continue the trend of
# the five most recent observed cohorts and label it as extrapolation.
extra <- cohort %>%
  group_split(area) %>%
  purrr::map_dfr(function(d) {
    r <- d %>% filter(entry_year >= 2029, entry_year <= 2033) %>% arrange(entry_year)
    slope <- coef(lm(children ~ entry_year, data = r))[["entry_year"]]
    tibble(area = d$area[1], entry_year = c(2034L, 2035L),
           children = r$children[r$entry_year == 2033] + slope * c(1, 2),
           extrapolated = TRUE)
  })

cohort <- bind_rows(cohort, extra) %>% arrange(area, entry_year)

# Resident children to state-sector Year 7 demand. Derived from the
# council's own published allocation factsheets: total offers made against
# the resident cohort of the same year.
admissions <- read_open_csv(OPEN$admissions)
offers_2024 <- sum(admissions$total_offer_count, na.rm = TRUE)
resident_2024 <- cohort$children[cohort$area == "Brighton & Hove" &
                                   cohort$entry_year == 2024]
STATE_SHARE <- offers_2024 / resident_2024

message(sprintf("  State-sector share, from published 2024 offers (%.0f) over resident children (%.0f): %.3f",
                offers_2024, resident_2024, STATE_SHARE))

demand_ts <- cohort %>% mutate(state_demand = children * STATE_SHARE)

# --- Zone shares -----------------------------------------------------
# The ONS cohort estimates are published at ward level. Within-ward
# weighting comes from ONS small-area age-11 population, not postcode
# counts - see the note below the ward totals.
zone_ward <- in_scope %>%
  count(zone, ward = osward, name = "n_pcd_w") %>%
  filter(!grepl("^NA\\|", zone)) %>%
  group_by(zone) %>%
  slice_max(n_pcd_w, n = 1, with_ties = FALSE) %>%
  ungroup()

ward_totals <- ward_ages %>%
  filter(entry_year == 2026) %>%
  select(ward, ward_children = n)

# Splitting a ward's children between its LSOAs by POSTCODE COUNT assumes
# every postcode holds the same number of eleven-year-olds. It does not:
# family housing and student or retirement housing have very different
# child densities, and the difference runs along exactly the lines school
# catchment boundaries follow. ONS publishes small-area population by
# single year of age, so the real age-11 distribution can be used instead.
#
# This matters most where a catchment boundary runs THROUGH a ward, which
# is the normal case in Hove. Under the postcode-count apportionment the
# deprivation contrast between the two halves of a paired catchment was
# almost entirely smoothed away.

LSOA_AGE11 <- OPEN$lsoa_age11

lsoa_child_share <- if (file.exists(LSOA_AGE11)) {
  readxl::read_excel(LSOA_AGE11) %>%
    select(lsoa = geography, prop = avg_18_20) %>%
    filter(!is.na(prop), prop > 0)
} else {
  message("  ! ONS LSOA age-11 file not found; falling back to postcode counts")
  tibble(lsoa = character(), prop = numeric())
}

zones <- zones %>%
  left_join(zone_ward %>% select(zone, ward), by = "zone") %>%
  left_join(ward_totals, by = "ward") %>%
  left_join(lsoa_child_share, by = "lsoa") %>%
  group_by(ward) %>%
  # Postcode count is retained as the fallback for zones the ONS file does
  # not cover, which is the expansion area outside the city.
  mutate(w = dplyr::coalesce(prop, n_pcd / sum(n_pcd) * mean(prop, na.rm = TRUE)),
         w = dplyr::if_else(is.finite(w) & w > 0, w, n_pcd / sum(n_pcd)),
         share_in_ward = w / sum(w),
         Oi = ward_children * share_in_ward * STATE_SHARE) %>%
  ungroup() %>%
  select(-w) %>%
  filter(!is.na(Oi), Oi > 0)

message(sprintf("  Child counts: %d zones from ONS age-11 estimates, %d from postcode counts",
                sum(!is.na(zones$prop)), sum(is.na(zones$prop))))

message("  Zones with demand: ", nrow(zones),
        " | modelled 2026 cohort: ", round(sum(zones$Oi)))


# ====================================================================
# 2. Catchment populations under each regime
# ====================================================================
# Catchment was assigned per postcode in section 1b. What is left is to
# report the resulting cohort, and to record the same cohort under the
# other regime so the two can be compared without re-running the whole
# pipeline.

message("\n=== 2. Catchment populations ===")

print(as.data.frame(zones %>% count(catchment, wt = round(Oi), name = "children_2026")),
      row.names = FALSE)

# Children by catchment under both maps. Because zones are split on the
# regime pair, this is an exact regrouping of the same zones.
regime_pop <- purrr::map_dfr(names(CATCHMENT_REGIMES), function(rg) {
  zones %>%
    group_by(catchment = .data[[paste0("catch_", rg)]]) %>%
    summarise(children = sum(Oi), .groups = "drop") %>%
    mutate(regime = rg)
})

message("\n  2026 cohort by catchment, under each regime:")
print(as.data.frame(regime_pop %>%
        mutate(children = round(children)) %>%
        tidyr::pivot_wider(names_from = regime, values_from = children) %>%
        mutate(change = optionZ - pre2024)), row.names = FALSE)


# ====================================================================
# 3. Travel costs
# ====================================================================

message("\n=== 3. Travel costs ===")

travel <- read_open_csv(OPEN$travel) %>%
  mutate(postcode = normalise_pcd(id)) %>%
  select(postcode, starts_with("time_")) %>%
  tidyr::pivot_longer(starts_with("time_"), names_to = "short",
                      values_to = "cij_pcd", names_prefix = "time_")

pcd_zone <- in_scope %>% select(postcode, zone)

zone_cost <- pcd_zone %>%
  inner_join(travel, by = "postcode") %>%
  group_by(zone, short) %>%
  summarise(cij = mean(cij_pcd, na.rm = TRUE), .groups = "drop") %>%
  filter(is.finite(cij))

message("  Observed (routed) zone-school pairs: ", nrow(zone_cost))

# Does the matrix carry the expansion area and the relocation site? The
# rebuilt one does; the older published file does not, and then the
# distance model below has to stand in.
ROUTED_ELM  <- "elm_grove" %in% zone_cost$short

# Without a routed Elm Grove column the relocated cost table is identical
# to the current one and every relocation result collapses silently to
# zero. That has happened once, from a reverted path in OPEN$travel, so
# it now stops the build rather than producing null findings.
if (!ROUTED_ELM) {
  stop("The travel matrix has no `time_elm_grove` column, so relocation ",
       "cannot be modelled and every relocation result would be zero.\n",
       "  Using: ", OPEN$travel, "\n",
       "  Expected the rebuilt matrix from public/R/00b_build_travel_matrix.R.")
}
east_zones  <- zones$zone[zones$area == "Expansion area"]
ROUTED_EAST <- mean(east_zones %in% zone_cost$zone) > 0.9

message("  Elm Grove routed directly: ", ROUTED_ELM)
message("  Expansion area routed directly: ", ROUTED_EAST,
        sprintf(" (%.0f%% of its zones)", 100 * mean(east_zones %in% zone_cost$zone)))

# The distance-to-time model is only a fallback now — for any pair the
# routed matrix does not cover.
pcd_xy <- in_scope %>% select(postcode, oseast1m, osnrth1m)

cost_train <- travel %>%
  inner_join(pcd_xy, by = "postcode") %>%
  inner_join(SCHOOLS_OPEN %>% select(short, name, easting, northing), by = "short") %>%
  mutate(euclid_km = pmax(euclid_m(oseast1m, osnrth1m, easting, northing) / 1000, 0.1)) %>%
  filter(is.finite(cij_pcd), cij_pcd > 0)

cost_mod <- lm(cij_pcd ~ euclid_km + sqrt(euclid_km) + factor(name), data = cost_train)
dest_eff <- coef(cost_mod)[grepl("^factor\\(name\\)", names(coef(cost_mod)))]
names(dest_eff) <- sub("^factor\\(name\\)", "", names(dest_eff))
mean_eff <- mean(c(0, dest_eff))

message(sprintf("  Distance-to-time model: n = %s, R2 = %.3f, residual SD = %.1f min, to %.1f km",
                format(nrow(cost_train), big.mark = ","),
                summary(cost_mod)$r.squared, summary(cost_mod)$sigma,
                max(cost_train$euclid_km)))

predict_time <- function(km, school, use_mean = FALSE) {
  b <- coef(cost_mod); km <- pmax(km, 0.1)
  use_mean <- rep_len(use_mean, length(school))
  own <- as.numeric(ifelse(school %in% names(dest_eff), dest_eff[school], mean_eff))
  pmax(b[["(Intercept)"]] + b[["euclid_km"]] * km + b[["sqrt(euclid_km)"]] * sqrt(km) +
         ifelse(use_mean, mean_eff, own), 1)
}

#' Full zone x school cost table, with Longhill optionally relocated
#'
#' Where the routed matrix has a value it is used. The relocation now has a
#' routed column of its own (`time_elm_grove`), so moving the school is a
#' matter of swapping which routed column Longhill reads from, rather than
#' predicting its accessibility from straight-line distance and assuming an
#' average site penalty. Anything the matrix does not reach still falls back
#' to the distance model, and how much of the table that accounts for is
#' reported below.
build_costs_open <- function(lh_site = NULL) {

  sch <- SCHOOLS_OPEN %>% select(short, name, easting, northing)
  relocating <- !is.null(lh_site)

  base <- tidyr::expand_grid(zones %>% select(zone, zone_e, zone_n), sch) %>%
    mutate(
      moved = relocating & name == "Longhill High School",
      # Coordinates follow the school when it moves, for the fallback only
      easting  = if_else(moved, if (relocating) lh_site$easting  else easting,  easting),
      northing = if_else(moved, if (relocating) lh_site$northing else northing, northing),
      km = euclid_m(zone_e, zone_n, easting, northing) / 1000,
      # A relocated school reads the routed Elm Grove column if there is one
      lookup_short = if_else(moved & ROUTED_ELM, "elm_grove", short)
    )

  out <- base %>%
    left_join(zone_cost %>% rename(lookup_short = short),
              by = c("zone", "lookup_short")) %>%
    mutate(
      routed = is.finite(cij),
      cij = if_else(routed, cij,
                    predict_time(km, name, use_mean = moved & !ROUTED_ELM))
    )

  attr(out, "pct_routed") <- 100 * mean(out$routed)
  out %>% select(zone, name, cij, km, routed)
}

costs_now <- build_costs_open(NULL)
costs_elm <- build_costs_open(ELM_GROVE)

message(sprintf("  Cost tables: %s pairs | routed %.0f%% (current sites), %.0f%% (relocated)",
                format(nrow(costs_now), big.mark = ","),
                100 * mean(costs_now$routed), 100 * mean(costs_elm$routed)))

if (ROUTED_ELM) {
  cmp <- costs_now %>%
    filter(name == "Longhill High School") %>%
    select(zone, now = cij) %>%
    inner_join(costs_elm %>% filter(name == "Longhill High School") %>%
                 select(zone, elm = cij), by = "zone")
  message(sprintf("  Relocation changes the median journey to Longhill by %+.1f minutes",
                  median(cmp$elm - cmp$now, na.rm = TRUE)))
}


# ====================================================================
# 4. Attractiveness — four open specifications
# ====================================================================

message("\n=== 4. Attractiveness specifications ===")

panel <- read_open_rds(OPEN$panel)

perf <- panel %>%
  filter(URN %in% as.character(SCHOOLS_OPEN$urn)) %>%
  group_by(urn = as.numeric(URN)) %>%
  # P8MEA is Progress 8: the DfE's published value-added measure. Carried
  # alongside Attainment 8 so the two can be contrasted - families sort on
  # one of them and not the other, which matters most for Hove Park.
  # Three measures of the same schools, deliberately kept side by side:
  #   att8     raw Attainment 8 - what league tables lead on
  #   p8       DfE Progress 8 - the published value-added measure
  #   va_lever Attainment 8 residualised on intake, the measure used in
  #            "How to Pull the Right Lever" - the most demanding of the
  #            three, because Progress 8 is itself correlated with intake
  summarise(att8 = mean(ATT8SCR, na.rm = TRUE),
            p8 = mean(suppressWarnings(as.numeric(P8MEA)), na.rm = TRUE),
            va_lever = mean(residual_ATT8SCR_imputed, na.rm = TRUE),
            # Published share of pupils eligible for free school meals at
            # any point in the last six years - the DfE disadvantage measure.
            fsm = mean(suppressWarnings(as.numeric(PTFSM6CLA1A)), na.rm = TRUE),
            .groups = "drop")

# (c) uses the council's published first-preference counts, averaged over
# the recent admissions rounds by 01a rather than taken from a single
# year. Note that first preferences are themselves shaped by where
# families live, so this proxy bundles location with reputation — which is
# why the sensitivity analysis sweeps across specifications rather than
# committing to one.
fp <- readRDS(file.path(PUBLIC_OUT, "factsheet_panel.rds"))

prefs <- fp$attract_panel %>%
  select(name, prefs_per_place, mean_first_prefs = pref1, n_years)

message("  Attractiveness from the ", fp$attract_from, "-onwards factsheet mean (",
        max(prefs$n_years), " rounds)")

attract <- SCHOOLS_OPEN %>%
  select(urn, name, pan = pan2024) %>%
  left_join(perf, by = "urn") %>%
  left_join(prefs %>% select(name, prefs_per_place), by = "name") %>%
  mutate(
    # Peacehaven is outside the council's factsheets; use its own area's
    # published figures as the best open stand-in.
    att8 = if_else(is.na(att8), mean(att8, na.rm = TRUE), att8),
    p8 = if_else(is.na(p8) | is.nan(p8), NA_real_, p8),
    va_lever = if_else(is.na(va_lever) | is.nan(va_lever), NA_real_, va_lever),
    fsm = if_else(is.na(fsm) | is.nan(fsm), NA_real_, fsm),
    prefs_per_place = if_else(is.na(prefs_per_place),
                              min(prefs_per_place, na.rm = TRUE), prefs_per_place),
    W_equal  = 1,
    W_pan    = pan / mean(pan),
    W_prefs  = prefs_per_place / mean(prefs_per_place),
    W_att8   = exp(0.093 * (att8 - mean(att8)))   # slope from the open stage-2 form
  )

message("  Specifications: equal, PAN, published first preferences, Attainment 8")
print(as.data.frame(attract %>%
  transmute(School = name, PAN = pan, ATT8 = round(att8, 1),
            `Prefs/place` = round(prefs_per_place, 2),
            W_pan = round(W_pan, 2), W_prefs = round(W_prefs, 2),
            W_att8 = round(W_att8, 2))), row.names = FALSE)


# ====================================================================
# 5. Save
# ====================================================================

saveRDS(list(
  zones       = zones,
  cohort      = cohort,
  demand_ts   = demand_ts,
  state_share = STATE_SHARE,
  costs_now   = costs_now,
  costs_elm   = costs_elm,
  cost_model  = list(r2 = summary(cost_mod)$r.squared,
                     sigma = summary(cost_mod)$sigma,
                     n = nrow(cost_train),
                     max_km = max(cost_train$euclid_km)),
  attract     = attract,
  schools     = SCHOOLS_OPEN,
  catchment_schools = CATCHMENT_SCHOOLS_OPEN,
  elm_grove   = ELM_GROVE,
  regime      = CATCH_REGIME,
  regime_label = CATCHMENT_REGIMES[[CATCH_REGIME]]$label,
  regime_pop  = regime_pop,
  # Per-postcode catchment under both maps, so the regimes can be
  # compared downstream without re-reading the boundary files.
  pcd_regime  = in_scope %>%
    transmute(postcode, zone, lsoa = lsoa21, area,
              pre2024 = catch_pre2024, optionZ = catch_optionZ),
  built_at    = Sys.time()
), file.path(PUBLIC_OUT, "open_inputs.rds"))

readr::write_csv(demand_ts, file.path(PUBLIC_OUT, "open_demand_projection.csv"))
readr::write_csv(attract,   file.path(PUBLIC_OUT, "open_attractiveness.csv"))

message("\nSaved public/output/open_inputs.rds")
