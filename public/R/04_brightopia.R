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
BETA_ORIGINAL <- 1.5

zones_city <- inp$zones %>% filter(area != "Expansion area")

#' One Brightopia run
#'
#' @param site "now" or "elm"
#' @param beta distance decay
#' @param scope "city" or "expanded"
brightopia <- function(site, beta, scope = "city") {

  z    <- if (scope == "city") zones_city else inp$zones
  keep <- if (scope == "city") CITY_SCHOOLS else inp$schools$name
  cost <- if (site == "now") inp$costs_now else inp$costs_elm

  d <- cost %>%
    filter(name %in% keep) %>%
    inner_join(z %>% select(zone, Oi), by = "zone") %>%
    filter(is.finite(cij), Oi > 0) %>%
    # Every school identical. The value is arbitrary — in a
    # production-constrained model with a common W it cancels in A_i —
    # but 100 is what the original used.
    mutate(Wj = 100)

  d <- prod_constrained_sim(d, o_col = "Oi", w_col = "Wj", c_col = "cij",
                            orig_col = "zone", dest_col = "name",
                            alpha = 1, beta = beta)

  d %>%
    group_by(name) %>%
    summarise(modelled = sum(sim_flow),
              mean_travel = weighted.mean(cij, sim_flow), .groups = "drop") %>%
    mutate(site = site, beta = beta, scope = scope)
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

competition <- tidyr::expand_grid(name = sch_xy$name, k = sch_xy$name) %>%
  filter(name != k) %>%
  left_join(sch_xy, by = "name") %>%
  left_join(sch_xy %>% rename(k = name, ke = easting, kn = northing), by = "k") %>%
  mutate(d_km = pmax(euclid_m(easting, northing, ke, kn) / 1000, 0.1)) %>%
  group_by(name) %>%
  summarise(C_j = sum(d_km^-SIGMA), .groups = "drop")


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
  mutate(access_index  = 100 * A_hansen / mean(A_hansen),
         compete_index = 100 * C_j / mean(C_j),
         # kept under the old name so nothing downstream breaks, but it
         # is a market share and is no longer presented as a site score
         site_index = share_index)

message("\n=== Accessibility and competition, separated ===")
print(as.data.frame(b_now %>%
  transmute(School = name,
            `Children reachable` = round(access_index),
            `Rivals nearby` = round(compete_index),
            `Brightopia intake` = round(100 * share_index),
            `Mean journey` = round(mean_travel, 1)) %>%
  arrange(desc(`Children reachable`))), row.names = FALSE)

message(sprintf("  Correlation between reach and crowding: %.2f",
                cor(log(b_now$access_index), log(b_now$compete_index))))

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
  relocated        = b_elm,
  expanded         = b_exp,
  sweep            = sweep,
  lh_sweep         = lh_sweep,
  beta_original    = BETA_ORIGINAL,
  betas            = BETAS_B,
  city_schools     = CITY_SCHOOLS,
  city_children    = CITY_CHILDREN,
  city_places      = CITY_PLACES,
  city_fill        = CITY_FILL,
  equal_share      = EQUAL_SHARE,
  sigma            = SIGMA,
  competition      = competition,
  run_at           = Sys.time()
), file.path(PUBLIC_OUT, "brightopia.rds"))

readr::write_csv(sweep, file.path(PUBLIC_OUT, "brightopia_sweep.csv"))

message("\nSaved public/output/brightopia.rds")
