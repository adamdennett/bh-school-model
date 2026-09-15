# public/R/11_simulator_results.R — the simulator's model, for the open report
# ======================================================================
# The open report's scenario sections were first built on the swept model
# (one catchment term for the whole city, attractiveness taken straight
# from preferences per place). The policy simulator and the strategic view
# have since moved to the full model, M5 (04b_model_terms.R), run with the
# council's 2026/27 oversubscription priorities, refused children
# re-offered by second preference, the East Sussex schools children leave
# for, finances, and a disadvantaged-pupil measure fitted to the council's
# Year 7 free school meal offers. This script brings those results into
# the open bundle, and runs the one set of scenarios the report needs that
# the simulator's own build does not: other schools reducing their
# admission numbers.
#
# The simulator lives in its own public repository, bh-school-system. Its
# model code (app/R/model.R, app/R/outcomes.R) and input bundle
# (app/data/sim_inputs.rds) are built from this bundle's outputs and other
# published data only, so nothing here reads restricted data. Set
# OPEN_SIMULATOR to point elsewhere.
#
# Output: public/output/simulator_results.rds
# ======================================================================

source(here::here("public", "R", "00_open_core.R"))
suppressPackageStartupMessages({ library(tidyverse) })

message("\n=== The simulator's model, for the open report ===")

SIM <- Sys.getenv("OPEN_SIMULATOR", "E:/bh_school_system")
stopifnot(dir.exists(SIM))
source(file.path(SIM, "app", "R", "model.R"))
source(file.path(SIM, "app", "R", "outcomes.R"))
inp <- readRDS(file.path(SIM, "app", "data", "sim_inputs.rds"))
sim_data <- function(f) readRDS(file.path(SIM, "data", f))

LH   <- "Longhill High School"
CN   <- "Cardinal Newman Catholic School"
KING <- "King's School"
DS   <- "Dorothy Stringer School"
V    <- "Varndean School"
BMS  <- "Blatchington Mill School"
BACA <- "Brighton Aldridge Community Academy"
YEARS <- c(2026, 2030, 2035)
PRIORITIES <- list(rule = "priorities")

city <- inp$schools %>% filter(city)

# ---- What if other schools shrink too? --------------------------------
# The same questions the swept model asked, on the simulator's model.
# Longhill keeps the admission number in force (210), so the figures are
# the children it would actually take, and each run is also repeated with
# Longhill's own number unbinding, for comparison with the swept model's
# "natural recruitment".
PAN_SCENARIOS <- list(
  "As determined"                                  = c(),
  "Cardinal Newman 300"                            = setNames(300, CN),
  "Cardinal Newman 270"                            = setNames(270, CN),
  "King's 150"                                     = setNames(150, KING),
  "Cardinal Newman 270, King's 150"                = setNames(c(270, 150), c(CN, KING)),
  "Dorothy Stringer 300"                           = setNames(300, DS),
  "Dorothy Stringer 270"                           = setNames(270, DS),
  "Stringer 300, Varndean 270, Blatchington Mill 300" = setNames(c(300, 270, 300), c(DS, V, BMS)),
  "Brighton Aldridge 150"                          = setNames(150, BACA),
  "Cardinal Newman 270, Stringer 270"              = setNames(c(270, 270), c(CN, DS)),
  "Cardinal Newman 270, Stringer 270, Brighton Aldridge 150" =
    setNames(c(270, 270, 150), c(CN, DS, BACA)))

pan_run <- function(overrides, year, free_lh = FALSE) {
  p <- setNames(city$pan, city$name)
  if (length(overrides)) p[names(overrides)] <- overrides
  if (free_lh) p[LH] <- 9999
  r <- run_sim(inp, pans = p, year = year, rules = PRIORITIES)
  m <- outcomes(inp, r)
  s <- r$schools
  list(r = r, m = m, s = s)
}

base_s <- purrr::map(setNames(YEARS, YEARS), function(y) pan_run(c(), y)$s)

pan_scen <- purrr::imap_dfr(PAN_SCENARIOS, function(ov, nm) {
  purrr::map_dfr(YEARS, function(y) {
    capd <- pan_run(ov, y)
    nat  <- pan_run(ov, y, free_lh = TRUE)
    b <- base_s[[as.character(y)]]
    tibble(scenario = nm, entry_year = y,
           longhill = capd$s$intake[capd$s$name == LH],
           longhill_natural = nat$s$intake[nat$s$name == LH],
           places_removed = sum(city$pan) - sum(capd$s$pan[capd$s$city], na.rm = TRUE),
           displaced = capd$m$catchment$displaced,
           left_city = capd$m$catchment$left_city,
           gorard = capd$m$gorard)
  })
})

# Where the children a reduction frees actually go, in the first year.
pan_moves <- purrr::imap_dfr(PAN_SCENARIOS[-1], function(ov, nm) {
  s <- pan_run(ov, 2026)$s
  b <- base_s[["2026"]]
  s %>% select(name, short, intake) %>%
    inner_join(b %>% select(name, base = intake), by = "name") %>%
    mutate(scenario = nm, change = intake - base) %>%
    filter(abs(change) >= 0.5)
})

message("  Longhill under other schools' reductions (2026 / 2030 / 2035):")
print(as.data.frame(pan_scen %>% select(scenario, entry_year, longhill) %>%
  mutate(longhill = round(longhill)) %>%
  pivot_wider(names_from = entry_year, values_from = longhill)), row.names = FALSE)

# ---- Longhill's fill at each admission number ------------------------
fill_ladder <- tidyr::expand_grid(pan = c(270, 240, 210, 180, 150, 120),
                                  entry_year = 2026:2035) %>%
  purrr::pmap_dfr(function(pan, entry_year) {
    p <- setNames(city$pan, city$name); p[LH] <- pan
    r <- run_sim(inp, pans = p, year = entry_year, rules = PRIORITIES)
    tibble(pan, entry_year, intake = r$schools$intake[r$schools$name == LH])
  }) %>%
  mutate(fill = intake / pan)

saveRDS(list(
  options        = sim_data("council_options.rds"),
  whitehawk      = sim_data("whitehawk_explained.rds"),
  priority6      = sim_data("priority6_sweep.rds"),
  comart         = sim_data("comart_scenarios.rds"),
  dis            = inp$params$dis,
  params         = inp$params[c("beta", "delta", "sigma", "gamma")],
  pan_scenarios  = pan_scen,
  pan_moves      = pan_moves,
  fill_ladder    = fill_ladder,
  school_pans    = setNames(city$pan, city$name),
  simulator_repo = "https://github.com/adamdennett/bh-school-system",
  simulator_app  = "https://adamdennett-bh-school-system.share.connect.posit.cloud/",
  strategic_view = "https://adamdennett.github.io/bh-school-system/",
  built_at       = Sys.time()),
  file.path(PUBLIC_OUT, "simulator_results.rds"))
message("Saved public/output/simulator_results.rds")
