# public/R/06_fsm_criterion_open.R — the FSM admissions priority (open)
# ======================================================================
# The open counterpart to R/15_fsm_criterion.R. Everything here is drawn
# from published sources: the council's secondary allocation factsheets,
# and its response to the Jurisdiction and Further Information Paper in
# the Schools Adjudicator cases ADA4622, ADA4631, ADA4633 and REF4730.
#
# That response is unusually informative, because the adjudicator
# required the council to publish both the 2026 outturn by
# oversubscription criterion and its own projection for 2027. So the
# effect of narrowing eligibility can be read off the council's own
# arithmetic rather than modelled.
#
# The criteria, in order of precedence:
#
#   3  sibling link
#   4  children in catchment eligible for FSM, up to the city average
#   5  other children eligible for FSM, up to the city average
#   6  open admissions (5% quota)
#   7  children in catchment
#   8  other children
#
# Criteria 4 and 5 sit ABOVE catchment residence. Criterion 5 places a
# child at a school they do not live near, ahead of children who do.
#
# Output: public/output/fsm_criterion_open.rds
# ======================================================================

source(here::here("public", "R", "00_open_core.R"))

suppressPackageStartupMessages({ library(tidyverse) })

message("\n=== The FSM admissions priority (published sources) ===")

SOURCE <- paste("Council response to the Jurisdiction and Further Information",
                "Paper, ADA4622/4631/4633/REF4730, items 6 and 15(a)")

# The council's stated basis for the contraction
TARGETED_SHARE <- 0.56   # item 15(a)
STATED_DECREASE <- 0.44  # "a decrease of 44% in pupils eligible"

# ---- City-wide, 2026 outturn ----------------------------------------
# Offers by criterion across the city, from the council's item 6 answer.

city <- tribble(
  ~criterion,                    ~offers, ~share,
  "3 — sibling link",                301,  21.8,
  "4 — FSM in catchment",            194,  14.0,
  "7 — in catchment",                597,  43.2
)

message("\n  City-wide offers by criterion, 2026:")
print(as.data.frame(city), row.names = FALSE)
message(sprintf("  Criteria 4 and 7 together — the catchment-dependent ones — are %.1f%% of offers.",
                sum(city$share[city$criterion != "3 — sibling link"])))

# ---- The three oversubscribed schools --------------------------------
# 2026 outturn against the council's own 2027 projection (Question 6).

proj <- tribble(
  ~school,                    ~criterion,                 ~y2026, ~y2027,
  "Blatchington Mill School", "3 — sibling link",              73,     92,
  "Blatchington Mill School", "4 — FSM in catchment",          45,     25,
  "Blatchington Mill School", "5 — FSM out of catchment",      14,      8,
  "Blatchington Mill School", "6 — open admissions",           17,     17,
  "Dorothy Stringer School",  "3 — sibling link",              65,     81,
  "Dorothy Stringer School",  "4 — FSM in catchment",          35,     22,
  "Dorothy Stringer School",  "5 — FSM out of catchment",      32,     18,
  "Dorothy Stringer School",  "6 — open admissions",           17,     17,
  "Varndean School",          "3 — sibling link",              70,     89,
  "Varndean School",          "4 — FSM in catchment",          61,     64,
  "Varndean School",          "5 — FSM out of catchment",       5,      0,
  "Varndean School",          "6 — open admissions",           15,     15
) %>%
  mutate(change = y2027 - y2026,
         implied_share = if_else(y2026 > 0, y2027 / y2026, NA_real_))

message("\n  2026 outturn against the council's 2027 projection:")
print(as.data.frame(proj %>%
  transmute(School = school, Criterion = criterion,
            `2026` = y2026, `2027` = y2027, Change = change)),
  row.names = FALSE)

fsm_rows <- proj %>% filter(grepl("^[45]", criterion))
message(sprintf("\n  FSM places across the three schools: %d in 2026, %d projected for 2027 (%+.0f%%).",
                sum(fsm_rows$y2026), sum(fsm_rows$y2027),
                100 * (sum(fsm_rows$y2027) / sum(fsm_rows$y2026) - 1)))

# ---- Does the council's own projection follow its own basis? ---------
# Item 15(a) says ~56% of currently eligible pupils qualify. Two of the
# three criterion-4 projections are consistent with that; one is not.

check <- proj %>% filter(criterion == "4 — FSM in catchment") %>%
  mutate(expected_at_56 = round(y2026 * TARGETED_SHARE),
         consistent = abs(implied_share - TARGETED_SHARE) < 0.10)

message("\n  Criterion 4 against the council's stated 56% basis:")
print(as.data.frame(check %>%
  transmute(School = school, `2026` = y2026, `Projected 2027` = y2027,
            `Implied share` = sprintf("%.0f%%", 100 * implied_share),
            `Expected at 56%` = expected_at_56,
            Consistent = if_else(consistent, "yes", "NO"))), row.names = FALSE)

odd <- check %>% filter(!consistent)
if (nrow(odd)) {
  message(sprintf("\n  %s does not follow the stated basis: projected to RISE from %d to %d",
                  odd$school[1], odd$y2026[1], odd$y2027[1]))
  message(sprintf("  where the council's own 56%% would give about %d.", odd$expected_at_56[1]))
  message("  Criteria 4, 5 and 6 all outrank criterion 7, so a discrepancy of this")
  message("  size directly affects how many places remain for in-catchment children.")
}

saveRDS(list(
  targeted_share = TARGETED_SHARE,
  stated_decrease = STATED_DECREASE,
  city = city, projection = proj, check = check, anomaly = odd,
  fsm_2026 = sum(fsm_rows$y2026), fsm_2027 = sum(fsm_rows$y2027),
  source = SOURCE, run_at = Sys.time()
), file.path(PUBLIC_OUT, "fsm_criterion_open.rds"))

message("\nSaved public/output/fsm_criterion_open.rds")
