# run_public.R — Build the open-data model end to end
# ====================================================
# Usage:  source("public/run_public.R")   (from the project root)
#
#   01  Assemble zones, populations, costs and attractiveness from
#       published sources only
#   02  Sweep the parameter envelope and report what survives it
#   03  Run the full scenario suite at a central specification, with the
#       band from the envelope reported around every headline figure
#
# Then render the report:
#   quarto::quarto_render("public/bh_school_sim_open.qmd")
#
# Nothing here reads pupil-level records. See public/README.md.
# ====================================================

steps <- c(
  "public/R/01a_factsheet_panel.R",      # 16 years of published factsheets
  "public/R/01_open_inputs.R",           # zones, costs, attractiveness
  "public/R/00c_network_maps.R",         # the routing network, for the method section
  "public/R/01b_reception_cohort.R",     # Year 7 projected from Reception offers
  "public/R/01d_adjudicator.R",          # published evidence from the determination
  "public/R/01c_identification_test.R",  # what the preference ranks can pin down
  "public/R/02_sensitivity_envelope.R",  # sweep the remaining unknowns
  "public/R/03_open_scenarios.R",        # scenarios, with bands
  "public/R/04_brightopia.R",            # the distance-only model
  "public/R/05_pan_scenarios_open.R",    # admission-number scenarios
  "public/R/06_fsm_criterion_open.R",    # the FSM priority, from published sources
  "public/R/07_deprivation_open.R"       # IDACI and catchment deprivation
)

t0 <- Sys.time()
for (i in seq_along(steps)) {
  cat("\n", strrep("=", 70), "\n[", i, "/", length(steps), "] ", steps[i], "\n",
      strrep("=", 70), "\n", sep = "")
  source(steps[i], local = new.env(), echo = FALSE)
}
cat(sprintf("\nOpen-data build complete in %.1f min\n",
            as.numeric(difftime(Sys.time(), t0, units = "mins"))))
