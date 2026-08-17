# 01b_reception_cohort.R — projecting Year 7 from Reception offers
# ======================================================================
# The demand projection used elsewhere in this bundle ages the ONS
# mid-2022 small-area estimates forward: a child aged a in 2022 enters
# Year 7 in 2022 + (11 - a). That is sound, but it counts *children in
# the population*, and the thing schools actually need is *children who
# will take a state-sector Year 7 place*. The gap between the two is
# independent schools, home education and migration, and it is large in
# Brighton — and very unevenly distributed across the city.
#
# There is a better-grounded alternative in the published record, and it
# is the method already used in
# BH_Schools_2/brighton_and_hove_allocations.qmd: compare Reception
# offers with the Year 7 offers made to the SAME cohort seven years
# later. A child offered a Reception place in 2014 was offered a Year 7
# place in 2021. Where the two differ, the difference is net movement
# into or out of the city's state sector between ages 4 and 11.
#
# That gives a survival ratio which can be measured, not assumed, and
# then applied to cohorts already sitting in primary school. Reception
# offers are published to 2026, so Year 7 demand is grounded in observed
# admissions all the way out to 2033 entry — without extrapolating a
# single birth.
#
# Both inputs are published factsheets. Nothing here uses pupil records.
#
# Output: public/output/reception_cohort.rds
# ======================================================================

source(here::here("public", "R", "00_open_core.R"))

suppressPackageStartupMessages({
  library(tidyverse)
})

ALLOCATIONS_QMD <- Sys.getenv("OPEN_ALLOCATIONS",
                              "E:/BH_Schools_2/brighton_and_hove_allocations.qmd")

LAG <- 7L   # Reception (age 4) to Year 7 (age 11)

message("\n=== Reception-to-Year-7 cohort projection ===")

txt <- paste(readLines(assert_open(ALLOCATIONS_QMD), warn = FALSE), collapse = "\n")

#' Pull named `<name> <- data.frame(...)` blocks out of the document and
#' evaluate each on its own in a clean environment. The surrounding
#' document is never executed.
extract_frames <- function(pattern, env = new.env()) {
  starts <- gregexpr(pattern, txt)[[1]]
  if (starts[1] == -1) return(env)
  for (s in starts) {
    i <- s; depth <- 0L; started <- FALSE
    while (i <= nchar(txt)) {
      ch <- substr(txt, i, i)
      if (ch == "(") { depth <- depth + 1L; started <- TRUE }
      if (ch == ")") depth <- depth - 1L
      if (started && depth == 0L) break
      i <- i + 1L
    }
    try(eval(parse(text = substr(txt, s, i)), envir = env), silent = TRUE)
  }
  env
}

# ---- Reception allocations, 2014-2026 -------------------------------

penv <- extract_frames("primary_[0-9]{4}\\s*<-\\s*data\\.frame\\(")
pnames <- grep("^primary_[0-9]{4}$", ls(penv), value = TRUE)

# The factsheets record each cell as "preferences (offers)", e.g.
# "169 (58)". Split it the same way the source document does.
split_nm <- function(x) {
  x <- as.character(x)
  prefs  <- suppressWarnings(as.numeric(gsub(",", "", trimws(sub("\\s*\\(.*$", "", x)))))
  offers <- suppressWarnings(as.numeric(gsub("[),*]", "",
                                             sub("^[^(]*\\(", "", x))))
  list(prefs = prefs, offers = offers)
}

primary_all <- purrr::map_dfr(pnames, function(nm) {
  d  <- get(nm, envir = penv)
  yr <- as.integer(sub("^primary_", "", nm))
  if (!all(c("School", "Total") %in% names(d))) return(NULL)
  s <- split_nm(d$Total)
  tibble(year = yr, school = as.character(d$School),
         prefs = s$prefs, offers = s$offers)
}) %>%
  filter(school != "Total", !is.na(offers))

message("  Reception factsheets found: ", length(pnames),
        " (", min(primary_all$year), "-", max(primary_all$year), ")")

# ---- Year 7 allocations, from the panel built in 01a ----------------

fs <- readRDS(file.path(PUBLIC_OUT, "factsheet_panel.rds"))

sec_city <- fs$factsheets %>%
  filter(!is.na(off_total)) %>%
  group_by(year) %>%
  summarise(y7_offers = sum(off_total, na.rm = TRUE), .groups = "drop")

rec_city <- primary_all %>%
  group_by(year) %>%
  summarise(rec_offers = sum(offers), .groups = "drop")

# ---- The survival ratio ---------------------------------------------

cohort <- rec_city %>%
  rename(cohort_year = year) %>%
  mutate(y7_year = cohort_year + LAG) %>%
  inner_join(sec_city %>% rename(y7_year = year), by = "y7_year") %>%
  mutate(survival = y7_offers / rec_offers,
         change = y7_offers - rec_offers)

message("\n=== Same cohort, Reception against Year 7 seven years later ===")
print(as.data.frame(cohort %>%
  transmute(`Reception year` = cohort_year,
            `Reception offers` = rec_offers,
            `Year 7 year` = y7_year,
            `Year 7 offers` = y7_offers,
            Change = change,
            `Survival` = sprintf("%.3f", survival))), row.names = FALSE)

# The rate has a trend, so the recent window is the honest basis for a
# projection rather than the whole series.
recent <- cohort %>% slice_max(cohort_year, n = 5)
surv_recent <- mean(recent$survival)
surv_sd     <- sd(recent$survival)
surv_all    <- mean(cohort$survival)

message(sprintf("\n  Survival, all overlapping cohorts: %.3f", surv_all))
message(sprintf("  Survival, five most recent (%d-%d): %.3f (sd %.3f)",
                min(recent$cohort_year), max(recent$cohort_year),
                surv_recent, surv_sd))

trend <- lm(survival ~ cohort_year, data = cohort)
message(sprintf("  Trend in survival: %+.4f a year (p = %.3f)",
                coef(trend)[["cohort_year"]],
                summary(trend)$coefficients["cohort_year", 4]))

# ---- Project Year 7 from cohorts already in primary ------------------

proj <- rec_city %>%
  rename(cohort_year = year) %>%
  mutate(entry_year = cohort_year + LAG) %>%
  filter(entry_year > max(sec_city$year)) %>%
  mutate(central = rec_offers * surv_recent,
         lo      = rec_offers * (surv_recent - surv_sd),
         hi      = rec_offers * (surv_recent + surv_sd),
         basis   = "Reception offers already made")

message("\n=== Year 7 demand implied by children already in primary school ===")
print(as.data.frame(proj %>%
  transmute(`Entry year` = entry_year,
            `Reception year` = cohort_year,
            `Reception offers` = rec_offers,
            `Projected Year 7` = round(central),
            Range = sprintf("%.0f - %.0f", lo, hi))), row.names = FALSE)

# ---- How does this compare with the ONS-based projection? ------------

ons <- NULL
inp_path <- file.path(PUBLIC_OUT, "open_inputs.rds")
if (file.exists(inp_path)) {
  inp <- readRDS(inp_path)
  ons <- inp$demand_ts %>%
    filter(area == "Brighton & Hove") %>%
    transmute(entry_year, ons_demand = state_demand, extrapolated)
}

cmp <- NULL
if (!is.null(ons)) {
  cmp <- proj %>%
    select(entry_year, reception_based = central) %>%
    inner_join(ons, by = "entry_year") %>%
    mutate(diff = reception_based - ons_demand,
           pct  = 100 * diff / ons_demand)

  message("\n=== Reception-based against the ONS cohort-ageing projection ===")
  print(as.data.frame(cmp %>%
    transmute(`Entry year` = entry_year,
              `From Reception offers` = round(reception_based),
              `From ONS estimates` = round(ons_demand),
              Difference = round(diff),
              `%` = sprintf("%+.1f%%", pct),
              Note = if_else(extrapolated, "ONS figure extrapolated", ""))),
    row.names = FALSE)
}

# ---- The same thing by catchment ------------------------------------
# Primary schools are mapped to the secondary catchment they sit in.
# This is not an admissions relationship — primary admissions do not
# follow secondary catchments — but it is a reasonable geography for
# asking where in the city the cohort is shrinking.

# The source document derives its lookup through a chain of joins. Here
# the primary school coordinates are lifted from the same `school_locs`
# table and assigned a catchment directly, by point in polygon against
# the option Z map — so this bundle's own boundary file governs.
lenv <- extract_frames("school_locs\\s*<-\\s*tibble::tribble\\(")
catch_lookup <- NULL
if (exists("school_locs", envir = lenv)) {
  locs <- as_tibble(get("school_locs", envir = lenv)) %>%
    filter(Phase == "Primary", !is.na(lat), !is.na(lon))
  cg <- read_catchments("optionZ")
  pts <- sf::st_as_sf(locs, coords = c("lon", "lat"), crs = 4326) %>%
    sf::st_transform(27700)
  ix <- as.integer(sf::st_within(pts, cg))
  gap <- is.na(ix)
  if (any(gap)) ix[gap] <- sf::st_nearest_feature(pts[gap, ], cg)
  catch_lookup <- locs %>%
    mutate(School = School, CatchmentGroup = cg$catchment[ix]) %>%
    select(School, CatchmentGroup)
  message("\n  Primary schools placed in a catchment: ", nrow(catch_lookup))
}

by_catch <- NULL
if (!is.null(catch_lookup) &&
    all(c("School", "CatchmentGroup") %in% names(catch_lookup))) {
  by_catch <- primary_all %>%
    inner_join(catch_lookup %>% select(school = School, catchment = CatchmentGroup),
               by = "school") %>%
    group_by(cohort_year = year, catchment) %>%
    summarise(rec_offers = sum(offers), .groups = "drop") %>%
    mutate(entry_year = cohort_year + LAG,
           projected = rec_offers * surv_recent)

  message("\n=== Reception cohorts by secondary catchment, and the Year 7 year they reach ===")
  print(as.data.frame(by_catch %>%
    filter(entry_year >= 2026) %>%
    mutate(projected = round(projected)) %>%
    select(catchment, entry_year, projected) %>%
    pivot_wider(names_from = entry_year, values_from = projected)),
    row.names = FALSE)

  lh <- by_catch %>% filter(grepl("Longhill", catchment)) %>% arrange(entry_year)
  if (nrow(lh) > 1) {
    lh_fut <- lh %>% filter(entry_year >= 2026)
    message(sprintf("\n  The Longhill catchment's Reception intake fell from %.0f (entering Y7 in %d) to %.0f (entering %d), %+.0f%%.",
                    first(lh_fut$rec_offers), first(lh_fut$entry_year),
                    last(lh_fut$rec_offers), last(lh_fut$entry_year),
                    100 * (last(lh_fut$rec_offers) / first(lh_fut$rec_offers) - 1)))
  }
}


# ====================================================================
# The council's own forecast
# ====================================================================
# Brighton & Hove published a Year 7 pupil forecast from the October
# 2025 school census, covering entry in 2026-2032 (Appendix 7 to the
# 2027-28 admission arrangements). It is built by a different method:
# roll the primary rolls forward, subtract a catchment-specific leakage
# rate, and adjust for the two city-wide faith schools.
#
# Two things in it are worth carrying into this analysis. The first is
# that the council plans around a Longhill PAN of 210, not the 240 used
# elsewhere in this bundle. The second is how low its 2032 figure is.

benv <- extract_frames("bhcc_forecast\\s*<-\\s*tibble::tribble\\(")
bhcc <- NULL
if (exists("bhcc_forecast", envir = benv)) {
  bhcc <- as_tibble(get("bhcc_forecast", envir = benv)) %>%
    pivot_longer(starts_with("Y2"), names_to = "entry_year", values_to = "council") %>%
    mutate(entry_year = as.integer(sub("^Y", "", entry_year)))

  message("\n=== The council's Oct-2025 forecast, by catchment ===")
  print(as.data.frame(bhcc %>%
    select(CatchmentGroup, PAN, entry_year, council) %>%
    pivot_wider(names_from = entry_year, values_from = council)), row.names = FALSE)

  lh_b <- bhcc %>% filter(CatchmentGroup == "Longhill")
  message(sprintf("\n  The council's planning PAN for Longhill is %d, not the %d used elsewhere here.",
                  unique(lh_b$PAN), 240L))
  message(sprintf("  Its own forecast has the Longhill catchment at %d children in 2032 — %d under that PAN.",
                  lh_b$council[lh_b$entry_year == 2032],
                  unique(lh_b$PAN) - lh_b$council[lh_b$entry_year == 2032]))
  message(sprintf("  Across 2026-2032 the forecast falls from %d to %d, a drop of %.0f%%.",
                  lh_b$council[lh_b$entry_year == 2026],
                  lh_b$council[lh_b$entry_year == 2032],
                  100 * (lh_b$council[lh_b$entry_year == 2032] /
                           lh_b$council[lh_b$entry_year == 2026] - 1)))

  # Why the council's Longhill line falls so much faster than the
  # Reception cohorts behind it. The forecast applies a catchment-
  # specific rate for children expected to leave the state-maintained
  # sector, and Longhill's is several times every other catchment's.
  # Published in the same appendix.
  LEAKAGE <- c("PACA" = 2.46, "Hove Park / Blatchington Mill" = 5.13,
               "Varndean / Dorothy Stringer" = 4.58, "BACA" = 6.52,
               "Patcham" = 3.03, "Longhill" = 23.89)

  message("\n  Leakage rates assumed in the council's forecast:")
  print(as.data.frame(tibble(Catchment = names(LEAKAGE),
                             `Assumed leakage` = sprintf("%.2f%%", LEAKAGE))),
        row.names = FALSE)
  message(sprintf("\n  Longhill's assumed leakage is %.1f times the average of the other five.",
                  LEAKAGE[["Longhill"]] / mean(LEAKAGE[names(LEAKAGE) != "Longhill"])))

  if (!is.null(by_catch)) {
    own <- by_catch %>% filter(catchment == "Longhill", entry_year >= 2026) %>%
      arrange(entry_year)
    message(sprintf("  For comparison, the Longhill catchment's own Reception cohorts fall only %.0f%% over the same span,",
                    100 * (own$rec_offers[own$entry_year == 2032] /
                             own$rec_offers[own$entry_year == 2026] - 1)))
    message("  so most of the council's projected decline is that assumption rather than the children.")
  }

  # City totals, three independent projections side by side
  council_city <- bhcc %>% group_by(entry_year) %>%
    summarise(council = sum(council), .groups = "drop")

  three <- proj %>%
    select(entry_year, reception = central) %>%
    full_join(council_city, by = "entry_year")
  if (!is.null(ons)) three <- three %>% left_join(ons, by = "entry_year")

  message("\n=== Three projections of Year 7 demand, side by side ===")
  message("  (the council's covers catchment schools only, so it excludes")
  message("   Cardinal Newman and King's and is not directly comparable in level)")
  print(as.data.frame(three %>%
    filter(!is.na(entry_year)) %>%
    transmute(`Entry year` = entry_year,
              `From Reception offers` = round(reception),
              `Council Oct-25` = council,
              `From ONS estimates` = round(ons_demand)) %>%
    arrange(`Entry year`)), row.names = FALSE)
}


# ====================================================================
# Save
# ====================================================================

saveRDS(list(
  primary_all   = primary_all,
  reception_city = rec_city,
  secondary_city = sec_city,
  cohort        = cohort,
  survival_recent = surv_recent,
  survival_sd   = surv_sd,
  survival_all  = surv_all,
  survival_trend = coef(trend)[["cohort_year"]],
  survival_trend_p = summary(trend)$coefficients["cohort_year", 4],
  projection    = proj,
  comparison    = cmp,
  by_catchment  = by_catch,
  council       = bhcc,
  council_lh_pan = if (!is.null(bhcc))
    unique(bhcc$PAN[bhcc$CatchmentGroup == "Longhill"]) else NA_integer_,
  lag           = LAG,
  run_at        = Sys.time()
), file.path(PUBLIC_OUT, "reception_cohort.rds"))

readr::write_csv(proj, file.path(PUBLIC_OUT, "reception_projection.csv"))

message("\nSaved public/output/reception_cohort.rds")
