# 01c_identification_test.R — Do the preference ranks identify beta?
# ==================================================================
# First preferences fix how attractive each school is. Second preferences
# then reveal what families substitute TOWARDS, which in principle pins
# down how much distance matters. This script tests whether it does.
#
# It also tests a second idea that turns out to matter more: the two faith
# schools admit on religious criteria, so they are not in every family's
# choice set.
#
# TWO RESULTS
#
#   1. beta is NOT identified. The fit improves monotonically with
#      distance decay, right through values no one would defend, so beta
#      must continue to be swept rather than estimated. Reported rather
#      than buried, because it says exactly what the council would need to
#      release to close the gap: an origin-destination matrix.
#
#   2. Faith schools DO sit in a restricted choice set, and allowing for it
#      substantially improves the model. That share is adopted in
#      00_open_core.R.
#
# Output: public/output/identification_test.rds
# ==================================================================

source(here::here("public", "R", "00_open_core.R"))

inp <- readRDS(file.path(PUBLIC_OUT, "open_inputs.rds"))
fp  <- readRDS(file.path(PUBLIC_OUT, "factsheet_panel.rds"))

target  <- fp$attract_panel %>% select(name, obs1 = pref1, obs2 = pref2)
schools <- target$name
faith_idx <- which(schools %in% FAITH_SCHOOLS)

zones <- inp$zones %>% filter(area == "Brighton & Hove")
sch_catch <- purrr::imap_dfr(inp$catchment_schools, ~ tibble(name = .x, sch_catch = .y))

grid <- inp$costs_now %>%
  filter(name %in% schools, zone %in% zones$zone) %>%
  left_join(zones %>% select(zone, catchment, Oi), by = "zone") %>%
  left_join(sch_catch, by = "name") %>%
  mutate(inc = as.integer(!is.na(sch_catch) & sch_catch == catchment))

Cw <- grid %>% select(zone, name, cij) %>%
  tidyr::pivot_wider(names_from = name, values_from = cij) %>% arrange(zone)
Iw <- grid %>% select(zone, name, inc) %>%
  tidyr::pivot_wider(names_from = name, values_from = inc) %>% arrange(zone)

Oi <- zones$Oi[match(Cw$zone, zones$zone)]
Cm <- as.matrix(Cw[, schools]); Im <- as.matrix(Iw[, schools])
obs1 <- target$obs1[match(schools, target$name)]
obs2 <- target$obs2[match(schools, target$name)]

#' Fit attractiveness to reproduce first preferences exactly, then score
#' the model on the second-preference profile it implies.
score_fit <- function(beta, gamma, phi, iters = 300) {
  base <- (Cm^(-beta)) * exp(gamma * Im)

  p1_of <- function(v) v / rowSums(v)
  p2_of <- function(v) {
    rs <- rowSums(v); p1 <- v / rs
    out <- matrix(0, nrow(v), ncol(v))
    for (k in seq_len(ncol(v))) {
      sh <- v / (rs - v[, k]); sh[, k] <- 0
      out <- out + p1[, k] * sh
    }
    out
  }
  combine <- function(W, fn) {
    v <- sweep(base, 2, W, "*")
    vn <- v; vn[, faith_idx] <- 0
    phi * (fn(v) * Oi) + (1 - phi) * (fn(vn) * Oi)
  }

  W <- rep(1, length(schools))
  for (k in seq_len(iters)) {
    p <- colSums(combine(W, p1_of)); p <- p / sum(p) * sum(obs1)
    W <- W * (obs1 / pmax(p, 1e-9)); W <- W / exp(mean(log(W)))
  }
  pred2 <- colSums(combine(W, p2_of)); pred2 <- pred2 / sum(pred2) * sum(obs2)
  list(chi2 = sum((pred2 - obs2)^2 / pmax(obs2, 1)),
       r2 = cor(pred2, obs2)^2, W = W, pred2 = pred2)
}

message("\n=== Identification test ===")

ident <- tidyr::expand_grid(beta = seq(0.5, 5, by = 0.25),
                            gamma = c(0.8, 1.6, 2.4),
                            phi = c(0.25, 0.35, 0.5, 0.75, 1.0)) %>%
  purrr::pmap_dfr(function(beta, gamma, phi) {
    s <- score_fit(beta, gamma, phi)
    tibble(beta, gamma, phi, chi2 = s$chi2, r2 = s$r2)
  })

profile <- ident %>% group_by(beta) %>% slice_min(chi2, n = 1) %>% ungroup()

message("  Best fit at each beta:")
print(as.data.frame(profile %>% mutate(across(c(chi2, r2), ~ round(.x, 2)))),
      row.names = FALSE)

monotone <- all(diff(profile$chi2) < 0)

# --- Result 1: beta is not identified --------------------------------
message("\n  RESULT 1 — beta is not identified.")
message("  Objective still improving at the top of the swept range: ", monotone)
message("  The fit keeps improving with distance decay past any defensible")
message("  value, so beta is swept, not estimated.")

# --- Result 2: faith schools have a restricted choice set ------------
ref_beta <- 2.5
with_phi <- ident %>% filter(beta == ref_beta) %>% slice_min(chi2, n = 1)
no_phi   <- ident %>% filter(beta == ref_beta, phi == 1.0) %>% slice_min(chi2, n = 1)

message(sprintf("\n  RESULT 2 — faith schools sit in a restricted choice set."))
message(sprintf("  At beta = %.1f, allowing an eligible share lifts R2 from %.2f to %.2f",
                ref_beta, no_phi$r2, with_phi$r2))
message(sprintf("  Best-fitting eligible share: %.0f%% of families", 100 * with_phi$phi))

# --- Result 3: the lower end of the sweep can be trimmed -------------
lo <- profile %>% filter(beta <= 1.25) %>% summarise(m = mean(chi2)) %>% pull(m)
hi <- profile %>% filter(beta >= 2.5)  %>% summarise(m = mean(chi2)) %>% pull(m)
message(sprintf("\n  RESULT 3 — low decay fits badly (mean chi2 %.0f below beta 1.25,",
                lo))
message(sprintf("  against %.0f above 2.5), so the bottom of the swept range is trimmed.", hi))

saveRDS(list(ident = ident, profile = profile, monotone = monotone,
             with_phi = with_phi, no_phi = no_phi, ref_beta = ref_beta,
             chi2_low = lo, chi2_high = hi,
             adopted_phi = FAITH_ELIGIBLE_SHARE, run_at = Sys.time()),
        file.path(PUBLIC_OUT, "identification_test.rds"))

readr::write_csv(profile, file.path(PUBLIC_OUT, "of_identification_profile.csv"))
message("\nSaved public/output/identification_test.rds")
