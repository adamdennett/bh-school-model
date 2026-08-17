# public/R/01d_adjudicator.R — evidence from the adjudicator's determination
# ======================================================================
# The Office of the Schools Adjudicator determined objections to Brighton
# & Hove's 2026/27 admission arrangements on 20 October 2025 (case
# references ADA4423, ADA4452 to ADA4454, ADA4456 and ADA4458). The
# determination is a public document and its evidence tables are
# published data, so everything here can be used in the open bundle.
#
# It supplies three things the open model could not otherwise get:
#
#   1. Binding admission numbers for 2026/27, which differ from the ones
#      the council determined.
#   2. The number of children in each catchment offered a place OUTSIDE
#      Brighton & Hove, for three admissions rounds. This is the only
#      published measure of out-of-city outflow by area, and it is the
#      open counterpart to what pupil records would show.
#   3. The council's forecast demand by catchment to 2031/32, both gross
#      and net of the adjustments it applies.
#
# Source: https://www.gov.uk/government/publications/
#         school-admissions-adjudicator-decisions
#
# Output: public/output/adjudicator.rds
# ======================================================================

source(here::here("public", "R", "00_open_core.R"))

suppressPackageStartupMessages({ library(tidyverse) })

message("\n=== The adjudicator's determination, 20 October 2025 ===")

CASE <- "ADA4423, ADA4452–4454, ADA4456, ADA4458"
DETERMINED <- as.Date("2025-10-20")

# ---- The binding admission numbers ----------------------------------

pans <- tribble(
  ~school,                     ~pan_2025, ~council_proposed, ~determined, ~note,
  "Blatchington Mill School",        330,               300,         330, "Reduction not upheld",
  "Dorothy Stringer School",         330,               300,         330, "Reduction not upheld",
  "Longhill High School",            270,               210,         210, "Unopposed"
)

message("\n  Admission numbers for 2026/27:")
print(as.data.frame(pans), row.names = FALSE)

# ---- Table 11: children offered places outside Brighton & Hove -------
# On National Offer Day, by home catchment area. CA-F is Longhill.

CA <- c(`CA-A` = "PACA", `CA-B` = "Hove_Blatch", `CA-C` = "Patcham",
        `CA-D` = "DS_Varndean", `CA-E` = "BACA", `CA-F` = "Longhill")

outside <- tribble(
  ~ca,     ~`2023/24`, ~`2024/25`, ~`2025/26`,
  "CA-A",           3,          3,          9,
  "CA-B",           3,          7,          7,
  "CA-C",           2,          1,          0,
  "CA-D",           2,          5,          1,
  "CA-E",           3,          5,          3,
  "CA-F",          50,         46,         54
) %>%
  mutate(catchment = unname(CA[ca])) %>%
  pivot_longer(starts_with("20"), names_to = "round", values_to = "children")

totals <- outside %>% group_by(round) %>%
  summarise(total = sum(children), .groups = "drop")

outside <- outside %>% left_join(totals, by = "round") %>%
  mutate(share = 100 * children / total)

message("\n  Children offered a place outside Brighton & Hove, by home catchment:")
print(as.data.frame(outside %>%
  transmute(Catchment = catchment, round,
            v = sprintf("%d (%.0f%%)", children, share)) %>%
  pivot_wider(names_from = round, values_from = v)), row.names = FALSE)

lh <- outside %>% filter(catchment == "Longhill")
message(sprintf("\n  The Longhill catchment accounts for %.0f–%.0f%% of all children leaving the city,",
                min(lh$share), max(lh$share)))
message(sprintf("  from %.0f%% of its cohort. In %s it was %d children of %d city-wide.",
                100 * 54 / 291, "2025/26",
                lh$children[lh$round == "2025/26"], lh$total[lh$round == "2025/26"]))

# ---- Table 9: the council's forecast demand by catchment -------------
# First row per catchment is demand for places at ANY city secondary;
# second is demand for that catchment's own schools, after the council's
# leakage and faith-school adjustments.

forecast <- tribble(
  ~ca,    ~basis,       ~`2026/27`, ~`2027/28`, ~`2028/29`, ~`2029/30`, ~`2030/31`, ~`2031/32`,
  "CA-A", "all",              264,        228,        234,        225,        257,        192,
  "CA-A", "own_schools",      221,        185,        191,        182,        214,        150,
  "CA-B", "all",              750,        760,        725,        728,        662,        677,
  "CA-B", "own_schools",      434,        443,        410,        413,        351,        365,
  "CA-C", "all",              241,        249,        237,        230,        198,        214,
  "CA-C", "own_schools",      205,        212,        201,        194,        163,        179,
  "CA-D", "all",              726,        692,        688,        659,        656,        637,
  "CA-D", "own_schools",      624,        592,        588,        560,        557,        539,
  "CA-E", "all",              165,        175,        180,        147,        129,        161,
  "CA-E", "own_schools",      129,        138,        143,        112,         95,        125,
  "CA-F", "all",              301,        290,        302,        280,        270,        274,
  "CA-F", "own_schools",      175,        166,        176,        159,        151,        154
) %>%
  mutate(catchment = unname(CA[ca])) %>%
  pivot_longer(starts_with("20"), names_to = "round", values_to = "children")

retention <- forecast %>%
  pivot_wider(names_from = basis, values_from = children) %>%
  mutate(retained = 100 * own_schools / all)

message("\n  Share of each catchment's forecast children expected at its own schools:")
print(as.data.frame(retention %>%
  transmute(Catchment = catchment, round, v = sprintf("%.0f%%", retained)) %>%
  pivot_wider(names_from = round, values_from = v)), row.names = FALSE)

lhr <- retention %>% filter(catchment == "Longhill")
othr <- retention %>% filter(catchment != "Longhill")
message(sprintf("\n  The Longhill catchment retains %.0f–%.0f%% of its children in the council's own forecast;",
                min(lhr$retained), max(lhr$retained)))
message(sprintf("  every other catchment retains %.0f–%.0f%%.",
                min(othr$retained), max(othr$retained)))
message("  (This gap combines the leakage assumption with the two city-wide faith schools.)")

saveRDS(list(
  case = CASE, determined = DETERMINED,
  pans = pans, outside = outside, forecast = forecast, retention = retention,
  ca_lookup = CA, run_at = Sys.time()
), file.path(PUBLIC_OUT, "adjudicator.rds"))

message("\nSaved public/output/adjudicator.rds")
