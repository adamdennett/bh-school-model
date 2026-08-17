# public/R/08_catchment_design_open.R — designed catchments, open data
# ======================================================================
# The open counterpart to R/06_catchment_design.R. It runs the *same*
# designer — public/R/lib_catchment_design.R, sourced by both bundles —
# over published inputs only:
#
#   * LSOA child counts apportioned from ward-level projections
#   * admission numbers from the adjudicator's determination of
#     20 October 2025
#   * routed journey times from the r5r network built in 00c
#
# None of that is pupil-level, so the catchment geometry in the open
# report is fully reproducible from this repository. What the open
# bundle cannot do is validate the designs against actual offers.
#
# Output: public/output/catchment_design_open.rds
# ======================================================================

source(here::here("public", "R", "00_open_core.R"))
source(here::here("public", "R", "lib_catchment_design.R"))

suppressPackageStartupMessages({ library(tidyverse) })

message("\n=== Designed catchments, from published sources ===")

inp <- readRDS(file.path(PUBLIC_OUT, "open_inputs.rds"))

AREAS <- inp$catchment_schools
LH    <- "Longhill High School"
DS    <- "Dorothy Stringer School"

# ---- Zones ------------------------------------------------------------
# The designer needs one row per zone with a child count and a centroid.

zones <- inp$zones %>%
  select(zone, zone_e, zone_n, Oi) %>%
  filter(!is.na(Oi), Oi > 0, !is.na(zone_e), !is.na(zone_n)) %>%
  distinct(zone, .keep_all = TRUE)

message(sprintf("  %d zones, %.0f children", nrow(zones), sum(zones$Oi)))

# Not every child enters the catchment system. The two city-wide faith
# schools, the independent sector and other authorities take a share, so
# the geographic catchments between them hold fewer children than the
# city contains. Scaling the zone counts to the catchment schools' total
# capacity leaves the geography of demand untouched and stops every
# catchment coming out over target — which is what the private bundle
# gets for free, its zone counts already being offers to these schools.

catch_cap <- sum(inp$schools$pan2026[inp$schools$name %in% unlist(AREAS)])
leakage   <- 1 - catch_cap / sum(zones$Oi)
zones$Oi  <- zones$Oi * catch_cap / sum(zones$Oi)

message(sprintf("  %.0f%% assumed to leave the catchment system (faith, independent, out-of-authority); %.0f children remain",
                100 * leakage, sum(zones$Oi)))

# ---- Costs ------------------------------------------------------------
# The library expects `pref_name`; the open inputs call it `name`. The
# Elm Grove matrix must be present — an earlier version of this bundle
# silently fell back to an unrouted approximation and every relocation
# result collapsed, so this is a hard failure rather than a warning.

to_costs <- function(x) {
  stopifnot(all(c("zone", "name", "cij") %in% names(x)))
  x %>% rename(pref_name = name) %>%
    filter(zone %in% zones$zone, is.finite(cij))
}

costs_now <- to_costs(inp$costs_now)
costs_elm <- to_costs(inp$costs_elm)

if (!any(costs_elm$cij != costs_now$cij[match(
      paste(costs_elm$zone, costs_elm$pref_name),
      paste(costs_now$zone, costs_now$pref_name))], na.rm = TRUE)) {
  stop("costs_elm is identical to costs_now — the Elm Grove routing is missing")
}

# ---- Admission numbers ------------------------------------------------

pan_base <- setNames(inp$schools$pan2026, inp$schools$name)

make_pans <- function(lh = 210, ds = NULL) {
  p <- pan_base
  p[LH] <- lh
  if (!is.null(ds)) p[DS] <- ds
  p
}

# ---- Designs ----------------------------------------------------------

designs <- list(
  now_210       = design_catchments(costs_now, make_pans(210), zones = zones,
                                    areas = AREAS),
  elm_210       = design_catchments(costs_elm, make_pans(210), zones = zones,
                                    areas = AREAS),
  now_150       = design_catchments(costs_now, make_pans(150), zones = zones,
                                    areas = AREAS),
  elm_150       = design_catchments(costs_elm, make_pans(150), zones = zones,
                                    areas = AREAS),
  elm_120       = design_catchments(costs_elm, make_pans(120), zones = zones,
                                    areas = AREAS),
  elm_150_ds270 = design_catchments(costs_elm, make_pans(150, ds = 270),
                                    zones = zones, areas = AREAS)
)

design_costs <- list(now_210 = costs_now, elm_210 = costs_elm,
                     now_150 = costs_now, elm_150 = costs_elm,
                     elm_120 = costs_elm, elm_150_ds270 = costs_elm)

designs <- purrr::imap(designs, function(d, nm)
  repair_fragments(d, design_costs[[nm]], areas = AREAS, zones = zones))

# ---- One catchment per school -----------------------------------------
# Brighton pairs Hove Park with Blatchington Mill and Dorothy Stringer
# with Varndean. Splitting the pairs gives each school its own geography.
# It reassigns every neighbourhood in the city, so it is a thought
# experiment rather than a proposal, but it answers two questions the
# paired designs cannot: what it would do to travel, and what it would
# do to the social composition of each catchment.

SINGLE <- purrr::set_names(as.list(unlist(AREAS, use.names = FALSE)),
                           unlist(AREAS, use.names = FALSE))

message(sprintf("\n  Single-school specification: %d catchments instead of %d",
                length(SINGLE), length(AREAS)))

designs$single_now_210 <- repair_fragments(
  design_catchments(costs_now, make_pans(210), zones = zones, areas = SINGLE),
  costs_now, areas = SINGLE, zones = zones)
designs$single_elm_210 <- repair_fragments(
  design_catchments(costs_elm, make_pans(210), zones = zones, areas = SINGLE),
  costs_elm, areas = SINGLE, zones = zones)

design_costs$single_now_210 <- costs_now
design_costs$single_elm_210 <- costs_elm

areas_for <- function(nm) if (grepl("^single_", nm)) SINGLE else AREAS

# ---- Does any of this shorten the journey to school? ------------------
# Each design minimises total travel subject to capacity, so mean journey
# time compares specifications against each other and against the map the
# city actually uses, costed identically.

catchment_travel <- function(assignment, costs, areas) {
  am <- purrr::imap_dfr(areas, ~ tibble(pref_name = .x, catchment = .y))
  assignment %>%
    inner_join(am, by = "catchment", relationship = "many-to-many") %>%
    inner_join(costs, by = c("zone", "pref_name")) %>%
    group_by(zone, catchment) %>%
    summarise(cij = min(cij), .groups = "drop") %>%
    left_join(zones %>% select(zone, Oi), by = "zone") %>%
    summarise(m = weighted.mean(cij, Oi, na.rm = TRUE)) %>% pull(m)
}

baseline_travel <- catchment_travel(
  inp$zones %>% distinct(zone, .keep_all = TRUE) %>%
    select(zone, catchment = catch_optionZ) %>% filter(zone %in% zones$zone),
  costs_now, AREAS)

travel_cmp <- purrr::imap_dfr(designs, function(d, nm) {
  ar <- areas_for(nm)
  tibble(design = nm,
         spec = if (grepl("^single_", nm)) "One catchment per school" else "Paired catchments",
         site = if (grepl("elm", nm)) "Elm Grove" else "Ovingdean",
         catchments = length(ar),
         mean_minutes = catchment_travel(d$assignment_repaired,
                                         design_costs[[nm]], ar))
}) %>%
  mutate(vs_current = mean_minutes - baseline_travel) %>%
  arrange(mean_minutes)

message(sprintf("\n=== Mean journey to the catchment school (current map: %.2f min) ===",
                baseline_travel))
print(as.data.frame(travel_cmp %>% mutate(across(where(is.numeric), ~ round(.x, 2)))),
      row.names = FALSE)

# ---- What it would do to deprivation ----------------------------------

dop <- readRDS(file.path(PUBLIC_OUT, "deprivation_open.rds"))

gorard <- function(p) {
  F_j <- p$children * p$pct_deprived / 100
  0.5 * sum(abs(F_j / sum(F_j) - p$children / sum(p$children)))
}

catch_profile <- function(assignment) {
  assignment %>%
    left_join(zones %>% select(zone, Oi), by = "zone") %>%
    left_join(dop$zones %>% distinct(zone, .keep_all = TRUE) %>%
                select(zone, idaci_score, deprived), by = "zone") %>%
    filter(!is.na(idaci_score), !is.na(Oi), Oi > 0) %>%
    group_by(School = catchment) %>%
    summarise(children = sum(Oi),
              pct_deprived = 100 * weighted.mean(deprived, Oi), .groups = "drop")
}

SPEC_MAPS <- list(
  `Current map` = inp$zones %>% distinct(zone, .keep_all = TRUE) %>%
    select(zone, catchment = catch_optionZ) %>% filter(zone %in% zones$zone),
  `Paired, redrawn`           = designs$now_210$assignment_repaired,
  `One per school, Ovingdean` = designs$single_now_210$assignment_repaired,
  `One per school, Elm Grove` = designs$single_elm_210$assignment_repaired
)

spec_profiles <- purrr::imap_dfr(SPEC_MAPS, ~ catch_profile(.x) %>% mutate(spec = .y))

spec_gorard <- spec_profiles %>% group_by(spec) %>% group_split() %>%
  purrr::map_dfr(~ tibble(spec = .x$spec[1], catchments = nrow(.x),
                          gorard = gorard(.x),
                          range_pp = diff(range(.x$pct_deprived))))

message("\n=== Segregation of where children live, by specification ===")
print(as.data.frame(spec_gorard %>% mutate(across(where(is.numeric), ~ round(.x, 3)))),
      row.names = FALSE)

pairs_split <- spec_profiles %>%
  filter(spec == "One per school, Ovingdean",
         School %in% c("Hove Park School", "Blatchington Mill School",
                       DS, "Varndean School")) %>%
  transmute(School, children = round(children), pct_deprived = round(pct_deprived, 1))

message("\n=== What splitting the two paired catchments would give each school ===")
print(as.data.frame(pairs_split), row.names = FALSE)

# ---- Diagnostics ------------------------------------------------------

accuracy <- purrr::imap_dfr(designs, function(d, nm) {
  tibble(design = nm, catchment = names(d$target),
         target = round(d$target), designed = round(d$repaired_assigned[names(d$target)]),
         difference = round(d$repaired_excess[names(d$target)]))
})

message("\n=== Capacity accuracy (Elm Grove, PAN 150, Stringer 270) ===")
print(as.data.frame(accuracy %>% filter(design == "elm_150_ds270") %>%
        select(-design)), row.names = FALSE)

contiguity <- purrr::imap_dfr(designs, function(d, nm)
  check_contiguity(d$assignment_repaired, zones) %>% mutate(design = nm))

message(sprintf("\n  Contiguous catchments: %d of %d across %d designs",
                sum(contiguity$contiguous), nrow(contiguity), length(designs)))

# ---- The eastern hinterland test --------------------------------------
# The clearest single test of what relocation costs Longhill: holding the
# admission number at the adjudicator's 210 and designing the catchment
# around each candidate site in turn, which neighbourhoods belong to
# Longhill at Ovingdean but not at Elm Grove? Those are the places the
# move severs the school from. Stated as a comparison of designs rather
# than by naming particular LSOAs, so it holds under any zone vintage.

hinterland <- {
  a_now <- designs$now_210$assignment_repaired
  a_elm <- designs$elm_210$assignment_repaired
  tibble(zone = a_now$zone,
         at_ovingdean = a_now$catchment,
         at_elm_grove = a_elm$catchment[match(a_now$zone, a_elm$zone)]) %>%
    filter(at_ovingdean == "Longhill", at_elm_grove != "Longhill") %>%
    left_join(zones %>% select(zone, Oi), by = "zone") %>%
    left_join(costs_now %>% filter(pref_name == LH) %>%
                select(zone, to_longhill_now = cij), by = "zone") %>%
    left_join(costs_elm %>% filter(pref_name == LH) %>%
                select(zone, to_longhill_elm = cij), by = "zone") %>%
    arrange(to_longhill_now)
}

message(sprintf(
  "\n=== Zones Longhill holds at Ovingdean but loses at Elm Grove (PAN 210) ===\n  %d zones, %.0f children",
  nrow(hinterland), sum(hinterland$Oi, na.rm = TRUE)))
print(as.data.frame(head(hinterland, 12) %>%
        mutate(across(where(is.numeric), ~ round(.x, 1)))), row.names = FALSE)

# The reverse: what the move buys, so the trade is stated both ways.
gained <- {
  a_now <- designs$now_210$assignment_repaired
  a_elm <- designs$elm_210$assignment_repaired
  tibble(zone = a_elm$zone,
         at_elm_grove = a_elm$catchment,
         at_ovingdean = a_now$catchment[match(a_elm$zone, a_now$zone)]) %>%
    filter(at_elm_grove == "Longhill", at_ovingdean != "Longhill") %>%
    left_join(zones %>% select(zone, Oi), by = "zone")
}

message(sprintf("  Against %d zones (%.0f children) it gains by moving",
                nrow(gained), sum(gained$Oi, na.rm = TRUE)))

east_times <- purrr::imap_dfr(
  list(Ovingdean = costs_now, `Elm Grove` = costs_elm),
  function(cst, site) cst %>%
    filter(zone %in% hinterland$zone,
           pref_name %in% c(LH, DS, "Varndean School")) %>%
    group_by(pref_name) %>%
    summarise(mean_minutes = mean(cij), .groups = "drop") %>%
    mutate(site = site)) %>%
  pivot_wider(names_from = pref_name, values_from = mean_minutes)

message("\n=== Mean journey time from the severed zones (minutes) ===")
print(as.data.frame(east_times %>% mutate(across(where(is.numeric), ~ round(.x, 1)))),
      row.names = FALSE)

# ---- How much the map moves ------------------------------------------

change <- purrr::imap_dfr(designs, function(d, nm) {
  a <- d$assignment_repaired %>%
    left_join(inp$zones %>% select(zone, current = catch_optionZ), by = "zone") %>%
    left_join(zones %>% select(zone, Oi), by = "zone")
  tibble(design = nm,
         zones_changed = sum(a$catchment != a$current, na.rm = TRUE),
         zones_total = nrow(a),
         children_moved = round(sum(a$Oi[a$catchment != a$current], na.rm = TRUE)))
})

message("\n=== Catchment change relative to the current map ===")
print(as.data.frame(change), row.names = FALSE)

saveRDS(list(
  designs = designs, accuracy = accuracy, contiguity = contiguity,
  hinterland = hinterland, gained = gained, leakage = leakage,
  travel_cmp = travel_cmp, baseline_travel = baseline_travel,
  spec_profiles = spec_profiles, spec_gorard = spec_gorard,
  pairs_split = pairs_split, single_catchments = SINGLE, east_times = east_times, change = change,
  zones = zones, areas = AREAS, run_at = Sys.time()
), file.path(PUBLIC_OUT, "catchment_design_open.rds"))

message("\nSaved public/output/catchment_design_open.rds")
