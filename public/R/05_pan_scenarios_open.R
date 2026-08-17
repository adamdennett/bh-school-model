# public/R/05_pan_scenarios_open.R — what if other schools shrink too?
# ======================================================================
# The open counterpart to the private model's admission-number scenarios.
# Every scenario elsewhere in this bundle treats Longhill as the only
# school whose size is in question. A falling cohort presses on every
# school at once, and several have their own reasons to want to be
# smaller — Cardinal Newman most of all, at 360 the largest in the city.
#
# Nothing here needs restricted data. Capacity is a published number and
# the model already redistributes displaced demand by iterative
# proportional fitting, so removing places at one school and watching
# where the children go is a straightforward extension.
#
# Admission numbers are the adjudicator's binding figures for 2026/27
# (determination of 20 October 2025), which matters: the council's
# proposed reductions at Blatchington Mill and Dorothy Stringer were
# overturned, so 60 central places the council intended to remove stay
# in the system.
#
# Output: public/output/pan_scenarios_open.rds
# ======================================================================

source(here::here("public", "R", "00_open_core.R"))

suppressPackageStartupMessages({ library(tidyverse) })

message("\n=== What if other schools shrink too? (open) ===")

inp <- readRDS(file.path(PUBLIC_OUT, "open_inputs.rds"))
env <- readRDS(file.path(PUBLIC_OUT, "sensitivity_envelope.rds"))

LH   <- "Longhill High School"
CN   <- "Cardinal Newman Catholic School"
DS   <- "Dorothy Stringer School"
BACA <- "Brighton Aldridge Community Academy"

YEARS <- c(2026, 2030, 2035)

# The central specification used elsewhere in this bundle
BETA  <- 2.0
GAMMA <- 1.2
WSPEC <- "W_prefs"

zones   <- inp$zones
attract <- inp$attract
sch_catch <- purrr::imap_dfr(inp$catchment_schools,
                             ~ tibble(name = .x, sch_catch = .y))

demand_for <- function(year) {
  tot <- inp$demand_ts %>% filter(entry_year == year) %>%
    group_by(area) %>% summarise(t = sum(state_demand), .groups = "drop")
  zones %>% group_by(area) %>% mutate(share = Oi / sum(Oi)) %>% ungroup() %>%
    left_join(tot, by = "area") %>%
    transmute(zone, catchment, Oi = share * t)
}

PAN_BASE <- setNames(inp$schools$pan2026, inp$schools$name)

SCENARIOS <- list(
  "S0. As determined"                       = c(),
  "A. Cardinal Newman 300"                  = setNames(300, CN),
  "B. Cardinal Newman 270"                  = setNames(270, CN),
  # Aligned with R/12_pan_scenarios.R so the two analyses test an
  # identical set and can be compared row for row. 270 is the reduction
  # Dorothy Stringer itself sought and the adjudicator refused for
  # 2026/27; 300 is the intermediate this bundle previously tested alone.
  # Both are carried, in both bundles.
  "C. Dorothy Stringer 270"                 = setNames(270, DS),
  "D. BACA 150"                             = setNames(150, BACA),
  "E. Newman 270 + Stringer 270"            = c(setNames(270, CN), setNames(270, DS)),
  "F. Newman 270 + Stringer 270 + BACA 150" =
    c(setNames(270, CN), setNames(270, DS), setNames(150, BACA)),
  "G. Dorothy Stringer 300"                 = setNames(300, DS),
  "H. Newman 270 + Stringer 300 + BACA 150" =
    c(setNames(270, CN), setNames(300, DS), setNames(150, BACA))
)

run_open <- function(year, overrides, free_lh = TRUE) {
  d <- inp$costs_now %>%
    left_join(demand_for(year), by = "zone") %>%
    left_join(sch_catch, by = "name") %>%
    left_join(attract %>% select(name, W = all_of(WSPEC)), by = "name") %>%
    mutate(in_catchment = as.integer(!is.na(sch_catch) & sch_catch == catchment)) %>%
    filter(!is.na(Oi), Oi > 0, is.finite(cij), !is.na(W))

  d <- sim_faith_split(d, o_col = "Oi", w_col = "W", c_col = "cij",
                       orig_col = "zone", dest_col = "name",
                       alpha = 1, beta = BETA,
                       extra_utility = GAMMA * d$in_catchment)

  cap <- PAN_BASE
  if (length(overrides)) cap[names(overrides)] <- overrides
  if (free_lh) cap[LH] <- 1e6

  d <- ipf_capacity(d, cap[names(cap) %in% unique(d$name)],
                    orig_col = "zone", dest_col = "name",
                    flow_col = "sim_flow", o_col = "Oi")

  d %>% group_by(name) %>%
    summarise(intake = sum(sim_flow_capped), .groups = "drop") %>%
    mutate(entry_year = year, pan = unname(cap[name]))
}

res <- purrr::imap_dfr(SCENARIOS, function(ov, nm) {
  purrr::map_dfr(YEARS, function(y) {
    run_open(y, ov) %>% filter(name == LH) %>%
      transmute(scenario = nm, entry_year, natural = intake)
  })
})

message("\n=== Longhill's natural recruitment under each scenario ===")
print(as.data.frame(res %>% mutate(natural = round(natural)) %>%
  pivot_wider(names_from = entry_year, values_from = natural)), row.names = FALSE)

gain <- res %>%
  left_join(res %>% filter(scenario == "S0. As determined") %>%
              select(entry_year, base = natural), by = "entry_year") %>%
  mutate(gain = natural - base)

message("\n  Children gained against the determined baseline:")
print(as.data.frame(gain %>% filter(scenario != "S0. As determined") %>%
  mutate(gain = round(gain, 1)) %>% select(scenario, entry_year, gain) %>%
  pivot_wider(names_from = entry_year, values_from = gain)), row.names = FALSE)

# Where the displaced children go, and how much slack there is
f0 <- run_open(2026, c(), free_lh = FALSE)
f1 <- run_open(2026, setNames(270, CN), free_lh = FALSE)

cascade <- f0 %>% select(name, before = intake) %>%
  inner_join(f1 %>% select(name, after = intake), by = "name") %>%
  mutate(change = after - before) %>% arrange(desc(change))

message("\n=== Where Cardinal Newman's children go if it admits 270 (2026) ===")
print(as.data.frame(cascade %>% filter(abs(change) > 0.5) %>%
  transmute(School = name, Change = round(change, 1))), row.names = FALSE)

slack <- f0 %>% mutate(spare = pan - intake, full = spare < 5)
message(sprintf("\n  %d of %d schools sit within five places of their admission number.",
                sum(slack$full), nrow(slack)))

saveRDS(list(longhill = res, gain = gain, cascade = cascade, slack = slack,
             scenarios = names(SCENARIOS), pan_base = PAN_BASE,
             years = YEARS, beta = BETA, gamma = GAMMA, w_spec = WSPEC,
             run_at = Sys.time()),
        file.path(PUBLIC_OUT, "pan_scenarios_open.rds"))

message("\nSaved public/output/pan_scenarios_open.rds")
