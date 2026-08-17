# public/R/07_deprivation_open.R — deprivation, from published sources
# ======================================================================
# The open counterpart to R/16_deprivation_profile.R.
#
# IDACI — the Income Deprivation Affecting Children Index — is published
# by LSOA, so the geography of child poverty in Brighton is fully open.
# What is NOT open is which school each child actually attends, so the
# open version cannot compute a school-level intake profile.
#
# It can do the next best thing, and arguably a more useful thing for
# judging catchment policy: compute the deprivation profile of each
# CATCHMENT's child population. That is the distribution the admissions
# system starts from, before any choice or allocation happens, and it is
# what a catchment redraw directly changes.
#
# Output: public/output/deprivation_open.rds
# ======================================================================

source(here::here("public", "R", "00_open_core.R"))

suppressPackageStartupMessages({ library(tidyverse); library(sf) })

message("\n=== Deprivation, from published sources ===")

inp <- readRDS(file.path(PUBLIC_OUT, "open_inputs.rds"))

IDACI_CSV <- Sys.getenv("OPEN_IDACI",
                        "E:/BH_Schools_Consultation/bn_postcodes_pop1.csv")

idaci <- read_open_csv(IDACI_CSV) %>%
  select(any_of(c("lsoa21", "idaci_rank", "idaci_decile", "idaci_score"))) %>%
  filter(!is.na(idaci_score)) %>%
  group_by(lsoa = lsoa21) %>%
  summarise(idaci_score = first(idaci_score),
            idaci_decile = first(idaci_decile), .groups = "drop") %>%
  mutate(deprived = idaci_decile <= 3)

message(sprintf("  IDACI for %d LSOAs; %.0f%% in the most deprived 30%% nationally",
                nrow(idaci), 100 * mean(idaci$deprived)))

# ---- Attach to the model's zones -------------------------------------

zones <- inp$zones %>%
  left_join(idaci, by = "lsoa") %>%
  filter(!is.na(idaci_score))

message(sprintf("  Zones matched: %d of %d (%.0f%% of modelled children)",
                nrow(zones), nrow(inp$zones),
                100 * sum(zones$Oi) / sum(inp$zones$Oi)))

# ---- Deprivation profile of each catchment ---------------------------
# Under each regime, so the redraw's distributional effect is visible.

prof <- purrr::map_dfr(names(CATCHMENT_REGIMES), function(rg) {
  zones %>%
    group_by(catchment = .data[[paste0("catch_", rg)]]) %>%
    summarise(children = sum(Oi),
              mean_idaci = weighted.mean(idaci_score, Oi),
              pct_deprived = 100 * weighted.mean(deprived, Oi),
              .groups = "drop") %>%
    mutate(regime = rg)
})

message("\n=== Deprivation of each catchment's own children ===")
print(as.data.frame(prof %>%
  filter(regime == "optionZ") %>%
  transmute(Catchment = catchment, Children = round(children),
            `Mean IDACI` = round(mean_idaci, 3),
            `% deprived` = round(pct_deprived, 1)) %>%
  arrange(desc(`% deprived`))), row.names = FALSE)

#' Gorard Segregation Index
gorard <- function(p) {
  F_j <- p$children * p$pct_deprived / 100
  0.5 * sum(abs(F_j / sum(F_j) - p$children / sum(p$children)))
}

g <- prof %>% group_by(regime) %>% group_map(~ gorard(.x)) %>% unlist()
names(g) <- names(CATCHMENT_REGIMES)[order(names(CATCHMENT_REGIMES))]

message("\n  Gorard index across catchment populations:")
for (nm in names(g)) message(sprintf("    %-10s %.3f", nm, g[[nm]]))
message("  (This is segregation of where children LIVE, by catchment — the")
message("   distribution the admissions system starts from, not where they end up.)")

# What the redraw did, distributionally
cmp <- prof %>%
  select(catchment, regime, pct_deprived) %>%
  pivot_wider(names_from = regime, values_from = pct_deprived) %>%
  mutate(change = optionZ - pre2024) %>%
  arrange(desc(abs(change)))

message("\n=== What the 2024 redraw did to each catchment's deprivation ===")
print(as.data.frame(cmp %>%
  transmute(Catchment = catchment,
            `Pre-2024 %` = round(pre2024, 1),
            `Option Z %` = round(optionZ, 1),
            Change = round(change, 1))), row.names = FALSE)

# ---- Segregation across SCHOOLS, not just catchments ------------------
# The index above measures where children live. What matters for the
# policy argument is where they end up, which needs modelled intakes.
# The open model produces those, so the index can be computed on them —
# but it inherits the model's parameter uncertainty, so it is swept
# rather than quoted as a single number.

env <- readRDS(file.path(PUBLIC_OUT, "sensitivity_envelope.rds"))
sch_catch <- purrr::imap_dfr(inp$catchment_schools,
                             ~ tibble(name = .x, sch_catch = .y))

demand_2026 <- inp$demand_ts %>% filter(entry_year == 2026) %>%
  group_by(area) %>% summarise(t = sum(state_demand), .groups = "drop")

zdem <- zones %>%
  group_by(area) %>% mutate(share = Oi / sum(Oi)) %>% ungroup() %>%
  left_join(demand_2026, by = "area") %>%
  transmute(zone, catchment = catch_optionZ, deprived,
            idaci_score, Oi = share * t)

gorard_intake <- function(beta, gamma, w_spec) {
  d <- inp$costs_now %>%
    inner_join(zdem, by = "zone") %>%
    left_join(sch_catch, by = "name") %>%
    left_join(inp$attract %>% select(name, W = all_of(w_spec)), by = "name") %>%
    mutate(in_catchment = as.integer(!is.na(sch_catch) & sch_catch == catchment)) %>%
    filter(!is.na(Oi), Oi > 0, is.finite(cij), !is.na(W))

  d <- sim_faith_split(d, o_col = "Oi", w_col = "W", c_col = "cij",
                       orig_col = "zone", dest_col = "name",
                       alpha = 1, beta = beta,
                       extra_utility = gamma * d$in_catchment)

  cap <- setNames(inp$schools$pan2026, inp$schools$name)
  d <- ipf_capacity(d, cap[names(cap) %in% unique(d$name)],
                    orig_col = "zone", dest_col = "name",
                    flow_col = "sim_flow", o_col = "Oi")

  p <- d %>% group_by(name) %>%
    summarise(children = sum(sim_flow_capped),
              pct_deprived = 100 * weighted.mean(deprived, sim_flow_capped),
              .groups = "drop") %>%
    filter(children > 0)
  gorard(p)
}

message("\n=== Segregation across school intakes, swept ===")
grid <- tidyr::expand_grid(beta = c(1.5, 2.0, 2.5, 3.0),
                           gamma = c(0, 0.8, 1.6, 2.4),
                           w_spec = c("W_pan", "W_prefs", "W_att8"))
sweep <- grid %>%
  mutate(gorard = purrr::pmap_dbl(list(beta, gamma, w_spec), gorard_intake))

message(sprintf("  %d runs. Gorard across school intakes: %.3f to %.3f, median %.3f",
                nrow(sweep), min(sweep$gorard), max(sweep$gorard),
                median(sweep$gorard)))
message(sprintf("  For comparison, across catchment POPULATIONS: %.3f",
                g[["optionZ"]]))

by_gamma <- sweep %>% group_by(gamma) %>%
  summarise(lo = min(gorard), med = median(gorard), hi = max(gorard),
            .groups = "drop")
message("\n  By how much catchment priority is assumed to matter:")
print(as.data.frame(by_gamma %>% mutate(across(where(is.numeric), ~ round(.x, 3)))),
      row.names = FALSE)

saveRDS(list(
  idaci = idaci, zones = zones %>% select(zone, lsoa, zone_e, zone_n, Oi,
                                          idaci_score, idaci_decile, deprived,
                                          catch_pre2024, catch_optionZ),
  profile = prof, comparison = cmp, gorard = g,
  intake_sweep = sweep, intake_by_gamma = by_gamma,
  run_at = Sys.time()
), file.path(PUBLIC_OUT, "deprivation_open.rds"))

message("\nSaved public/output/deprivation_open.rds")
