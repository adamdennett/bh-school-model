# 02_sensitivity_envelope.R — Do the conclusions depend on the restricted data?
# ============================================================================
# The model has three parameters that a pupil-level admissions extract lets
# you estimate precisely and open data does not:
#
#   beta   how sharply demand falls with journey time
#   W_j    how attractive each school is, net of location
#   gamma  how much the catchment rule matters
#
# Rather than assert values for those, this script sweeps them across the
# full range any reasonable analyst could defend, and asks whether the
# conclusions about Longhill survive the whole envelope. If they do, the
# restricted data is a refinement rather than a foundation, and every
# substantive claim can be published from open sources alone.
#
# The sweep covers:
#   beta   1.5 to 3.2          (the lower bound is empirical: the
#                               identification test in 01c shows the
#                               published second-preference profile fits
#                               badly below about 1.5, so that end of the
#                               range is trimmed on evidence)
#   W      four specifications  (equal, PAN, published first preferences,
#                               Attainment 8)
#   gamma  0 to 2.5            (from no catchment effect at all, to a
#                               twelve-fold odds multiplier)
#   years  2026, 2030, 2035
#   site   Ovingdean or Elm Grove
#
# For each combination the school's PAN is set high enough not to bind, so
# what is recorded is its NATURAL recruitment — the number of children it
# would attract. That is then compared against candidate admission numbers.
#
# Output: public/output/sensitivity_envelope.rds, public/output/env_*.csv
# ============================================================================

source(here::here("public", "R", "00_open_core.R"))

inp <- readRDS(file.path(PUBLIC_OUT, "open_inputs.rds"))

LH <- "Longhill High School"

# --- The sweep -------------------------------------------------------
BETAS  <- seq(1.5, 3.2, by = 0.1)
GAMMAS <- c(0, 0.8, 1.6, 2.4)
W_SPECS <- c("W_equal", "W_pan", "W_prefs", "W_att8")
YEARS  <- c(2026, 2030, 2035)
SITES  <- c("Ovingdean" = "now", "Elm Grove" = "elm")
PANS   <- c(240, 210, 180, 150, 120)   # 210 is in force for 2026/27

# Set this to a specific estimate to mark it on the outputs. Left empty so
# that this bundle carries no value derived from restricted data.
REFERENCE_BETA <- NULL

message("Sweep size: ", length(BETAS) * length(GAMMAS) * length(W_SPECS) *
          length(YEARS) * length(SITES), " model runs")


# --- Inputs ----------------------------------------------------------

zones   <- inp$zones
attract <- inp$attract
sch_catch <- purrr::imap_dfr(inp$catchment_schools,
                             ~ tibble(name = .x, sch_catch = .y))

cost_tbl <- list(now = inp$costs_now, elm = inp$costs_elm)

# Demand scaled to the cohort-aged projection for each year. Zones are
# split on the pair of catchment regimes, so switching regime regroups
# the same zones rather than changing the geography underneath them.
demand_for <- function(year, regime) {
  tot <- inp$demand_ts %>%
    filter(entry_year == year) %>%
    group_by(area) %>% summarise(t = sum(state_demand), .groups = "drop")
  zones %>%
    group_by(area) %>%
    mutate(share = Oi / sum(Oi)) %>%
    ungroup() %>%
    left_join(tot, by = "area") %>%
    transmute(zone, catchment = .data[[paste0("catch_", regime)]],
              Oi = share * t)
}

REGIMES <- names(CATCHMENT_REGIMES)
demand_cache <- purrr::set_names(
  purrr::map(REGIMES, function(rg)
    purrr::set_names(purrr::map(YEARS, demand_for, regime = rg), YEARS)),
  REGIMES)

# PAN vector with Longhill unbinding, so we measure natural recruitment
cap_free <- setNames(inp$schools$pan2026, inp$schools$name)
cap_free[LH] <- 9999


run_one <- function(site, beta, gamma, w_spec, year, regime = "optionZ") {

  d <- cost_tbl[[site]] %>%
    left_join(demand_cache[[regime]][[as.character(year)]], by = "zone") %>%
    left_join(sch_catch, by = "name") %>%
    left_join(attract %>% select(name, W = all_of(w_spec)), by = "name") %>%
    mutate(in_catchment = as.integer(!is.na(sch_catch) & sch_catch == catchment)) %>%
    filter(!is.na(Oi), Oi > 0, is.finite(cij), !is.na(W))

  d <- sim_faith_split(
    d, o_col = "Oi", w_col = "W", c_col = "cij",
    orig_col = "zone", dest_col = "name",
    alpha = 1, beta = beta, extra_utility = gamma * d$in_catchment
  )

  d <- ipf_capacity(d, cap_free[names(cap_free) %in% unique(d$name)],
                    orig_col = "zone", dest_col = "name",
                    flow_col = "sim_flow", o_col = "Oi")

  lh <- d %>% filter(name == LH)
  tibble(site = site, beta = beta, gamma = gamma, w_spec = w_spec,
         entry_year = year, regime = regime,
         natural_intake = sum(lh$sim_flow_capped),
         mean_travel = weighted.mean(lh$cij, lh$sim_flow_capped))
}

grid <- tidyr::expand_grid(site = unname(SITES), beta = BETAS, gamma = GAMMAS,
                           w_spec = W_SPECS, entry_year = YEARS,
                           regime = REGIMES)

message("Sweep including both catchment regimes: ", nrow(grid), " runs")
message("Running ...")
t0 <- Sys.time()
env <- purrr::pmap_dfr(grid, function(site, beta, gamma, w_spec, entry_year, regime)
  run_one(site, beta, gamma, w_spec, entry_year, regime))
message(sprintf("  done in %.1f s", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

env <- env %>%
  mutate(site_label = if_else(site == "now", "Ovingdean", "Elm Grove"),
         w_label = dplyr::recode(w_spec,
           W_equal = "All schools equal", W_pan = "Proportional to PAN",
           W_prefs = "Published first preferences", W_att8 = "Attainment 8"),
         regime_label = purrr::map_chr(regime, ~ CATCHMENT_REGIMES[[.x]]$label))

# The headline questions below are asked of the regime actually in force.
# `env` keeps both so the comparison can be made explicitly.
env_all <- env
env <- env %>% filter(regime == "optionZ")


# ====================================================================
# The robustness questions
# ====================================================================

message("\n", strrep("=", 68))
message("ROBUSTNESS")
message(strrep("=", 68))

# --- 1. Could Longhill ever sustain its historic PAN? ----------------

sustain <- tidyr::expand_grid(env, pan = PANS) %>%
  mutate(fills = natural_intake >= 0.9 * pan)

q1 <- sustain %>%
  group_by(entry_year, pan) %>%
  summarise(pct_of_envelope = 100 * mean(fills), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = pan, values_from = pct_of_envelope,
                     names_prefix = "PAN ")

message("\n1. Share of the parameter envelope in which Longhill recruits to")
message("   at least 90% of each admission number (%):")
print(as.data.frame(q1 %>% mutate(across(where(is.numeric), ~ round(.x)))),
      row.names = FALSE)

# --- 2. What is the range of its natural intake? ---------------------

q2 <- env %>%
  group_by(entry_year, site_label) %>%
  summarise(min = min(natural_intake), q25 = quantile(natural_intake, .25),
            median = median(natural_intake), q75 = quantile(natural_intake, .75),
            max = max(natural_intake), .groups = "drop")

message("\n2. Natural intake across the whole envelope:")
print(as.data.frame(q2 %>% mutate(across(where(is.numeric), round))), row.names = FALSE)

# --- 3. Does relocation help, and does that depend on the parameters? -

q3 <- env %>%
  select(beta, gamma, w_spec, entry_year, site_label, natural_intake) %>%
  tidyr::pivot_wider(names_from = site_label, values_from = natural_intake) %>%
  mutate(gain = `Elm Grove` - Ovingdean)

message("\n3. Effect of relocating to Elm Grove, holding catchments as they are:")
print(as.data.frame(
  q3 %>% group_by(entry_year) %>%
    summarise(median_gain = round(median(gain), 1),
              min_gain = round(min(gain), 1), max_gain = round(max(gain), 1),
              pct_positive = round(100 * mean(gain > 0)), .groups = "drop")),
  row.names = FALSE)

# --- 4. Which parameter actually drives the answer? ------------------

va <- aov(natural_intake ~ factor(beta) + factor(gamma) + w_spec +
            factor(entry_year) + site_label, data = env)
ss <- summary(va)[[1]][["Sum Sq"]]
imp <- tibble(term = rownames(summary(va)[[1]]), ss = ss) %>%
  mutate(term = trimws(term), pct = 100 * ss / sum(ss)) %>%
  arrange(desc(pct))

message("\n4. Which input the answer is most sensitive to (share of variance):")
print(as.data.frame(imp %>% mutate(pct = round(pct, 1)) %>% select(term, pct)),
      row.names = FALSE)

# --- 5. The envelope without the deliberately extreme case -----------
# "All schools equal" is the Brightopia assumption: it strips out every
# difference between schools and asks what pure geography would do. It is
# a useful bound but nobody believes it, so report the envelope both ways.

core <- env %>% filter(w_spec != "W_equal")

q5 <- core %>%
  group_by(entry_year, site_label) %>%
  summarise(min = min(natural_intake), median = median(natural_intake),
            max = max(natural_intake), .groups = "drop")

message("\n5. Natural intake excluding the 'all schools equal' case:")
print(as.data.frame(q5 %>% mutate(across(where(is.numeric), round))), row.names = FALSE)

q5b <- tidyr::expand_grid(core, pan = PANS) %>%
  mutate(fills = natural_intake >= 0.9 * pan) %>%
  group_by(entry_year, pan) %>%
  summarise(pct = 100 * mean(fills), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = pan, values_from = pct, names_prefix = "PAN ")

message("\n   ... and the share of that narrower envelope reaching 90% of PAN (%):")
print(as.data.frame(q5b %>% mutate(across(where(is.numeric), ~ round(.x)))),
      row.names = FALSE)

# --- 6. What the 2024 boundary redraw does ---------------------------
# Every run is paired: same site, beta, gamma, attractiveness and year,
# differing only in which catchment map assigns priority.

q6 <- env_all %>%
  select(site, beta, gamma, w_spec, entry_year, regime, natural_intake) %>%
  tidyr::pivot_wider(names_from = regime, values_from = natural_intake) %>%
  mutate(shift = optionZ - pre2024)

message("\n6. Effect of the 2024 redraw (option Z less pre-2024), current site:")
print(as.data.frame(q6 %>% filter(site == "now") %>% group_by(entry_year) %>%
  summarise(median = round(median(shift), 1), min = round(min(shift), 1),
            max = round(max(shift), 1),
            pct_worse = round(100 * mean(shift < 0)), .groups = "drop")),
  row.names = FALSE)

message("\n   ... and by how much catchment priority is assumed to matter:")
print(as.data.frame(q6 %>% filter(site == "now") %>% group_by(gamma) %>%
  summarise(median_shift = round(median(shift), 1),
            pct_worse = round(100 * mean(shift < 0)), .groups = "drop")),
  row.names = FALSE)


# ====================================================================
# Figures
# ====================================================================

pan_lab <- tibble(beta = 3.24, natural_intake = PANS, lab = paste("PAN", PANS))

p1 <- ggplot(env %>% filter(site == "now"),
             aes(beta, natural_intake, colour = w_label, group = interaction(w_label, gamma))) +
  geom_hline(yintercept = PANS, linetype = "dotted", colour = "grey65") +
  geom_text(data = pan_lab, aes(x = beta, y = natural_intake, label = lab),
            hjust = 0, size = 2.9, colour = "grey40", inherit.aes = FALSE) +
  geom_line(alpha = 0.55, linewidth = 0.6) +
  facet_wrap(~ entry_year, nrow = 1) +
  scale_x_continuous(limits = c(1, 3.65), breaks = seq(1, 3, 0.5)) +
  scale_colour_brewer(palette = "Dark2") +
  expand_limits(y = 0) +
  labs(title = "Longhill's natural intake across every plausible parameterisation",
       subtitle = "Each line is one attractiveness specification at one catchment-effect strength; open data only",
       x = "Distance decay (beta)", y = "Children recruited", colour = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

ggsave(file.path(PUBLIC_OUT, "fig_envelope_beta.png"), p1,
       width = 11, height = 5, dpi = 150)

p2 <- sustain %>%
  group_by(entry_year, pan) %>%
  summarise(pct = 100 * mean(fills), .groups = "drop") %>%
  ggplot(aes(factor(pan), factor(entry_year), fill = pct)) +
  geom_tile(colour = "white", linewidth = 1.2) +
  geom_text(aes(label = paste0(round(pct), "%")), size = 3.6) +
  scale_fill_gradient2(low = "#d73027", mid = "#fee08b", high = "#1a9850",
                       midpoint = 50, limits = c(0, 100), guide = "none") +
  labs(title = "How often does Longhill recruit to its number?",
       subtitle = "Share of the full parameter envelope in which the school reaches 90% of PAN",
       x = "Published Admission Number", y = "Year 7 entry year") +
  theme_minimal(base_size = 11)

ggsave(file.path(PUBLIC_OUT, "fig_envelope_pan.png"), p2,
       width = 8, height = 4, dpi = 150)


# ====================================================================
# Save
# ====================================================================

saveRDS(list(envelope = env, envelope_all = env_all, sustain = sustain,
             q1 = q1, q2 = q2, q3 = q3, q5 = q5, q5b = q5b, q6 = q6,
             core = core,
             importance = imp, betas = BETAS, gammas = GAMMAS,
             w_specs = W_SPECS, pans = PANS, years = YEARS,
             regimes = REGIMES, regime_used = "optionZ",
             reference_beta = REFERENCE_BETA, run_at = Sys.time()),
        file.path(PUBLIC_OUT, "sensitivity_envelope.rds"))

readr::write_csv(env, file.path(PUBLIC_OUT, "env_runs.csv"))
readr::write_csv(q1,  file.path(PUBLIC_OUT, "env_sustain_by_pan.csv"))

message("\nSaved public/output/sensitivity_envelope.rds and env_*.csv")
