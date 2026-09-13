# public/R/03b_school_finance.R — the city's secondary schools' finances
# ======================================================================
# Section 6 of the strategic view had two things in it: a national
# picture of how deficit risk varies with school size, and Longhill's
# own accounts. It did not say anything about the other nine schools,
# which is a strange gap in a document arguing that the city has a
# system-wide problem.
#
# This builds the whole city's position from the same open source the
# Longhill table already uses: the DfE's school income and expenditure
# collections, published per school on the schools financial
# benchmarking service. Maintained schools report on the Consistent
# Financial Reporting (CFR) return to 31 March; academies report on the
# Academies Accounts Return (AAR) to 31 August. The two are close enough
# in definition to sit in one table and different enough in TIMING that
# the latest published year is not the same year for every school, which
# is carried through explicitly rather than papered over.
#
# WHAT COUNTS AS TROUBLE. Three measures, all of them ratios rather than
# cash, because a £500k reserve means something different at Longhill
# and at Cardinal Newman:
#
#   revenue reserve / total income   the cushion, in years of income
#   in-year balance / total income   whether this year added or spent
#   staff costs / total income       the fixed cost that is hard to move
#
# THE LINK TO PUPIL NUMBERS. Funding is per-pupil, so the roll drives
# the income. Each Year 7 place a school loses removes five pupils from
# its 11-16 roll once the smaller cohort has worked through, and the
# projections in section 3 say how many places the city is about to
# lose. Multiplying that through at each school's own income per pupil
# gives the steady-state income change it is heading for.
#
# Two honest limits on that arithmetic, both stated in the output:
#   - The intakes are MODELLED (configuration A of the open scenarios,
#     the city as it stands). They are not a forecast, and for the two
#     faith schools they are the least reliable numbers in the bundle,
#     because the model admits to them on distance alone.
#   - Four of the ten schools have a sixth form, so their income per
#     pupil blends 11-16 and 16-18 rates. For those four the pound
#     figure is an approximation, and it is flagged.
#
# Output: output/school_finance.rds
# ======================================================================

source(here::here("public", "R", "00_open_core.R"))

message("\n=== The city's secondary school finances ===")

panel_raw <- read_open_rds(OPEN$panel)
os        <- readRDS(file.path(PUBLIC_OUT, "open_scenarios.rds"))

# ---- 1. The city's state secondaries, four years -----------------
# Independent schools file no return and are not part of the place
# system this document is about. Peacehaven is in the model because
# Brighton children go there, but it is an East Sussex school and its
# finances are not Brighton's problem.

CITY_LA <- "Brighton and Hove"

fin_panel <- panel_raw %>%
  filter(LANAME == CITY_LA,
         MINORGROUP %in% c("Academy", "Maintained school"),
         !is.na(fin_total_income), fin_total_income > 0) %>%
  transmute(
    urn = as.character(URN), name = SCHNAME, type = MINORGROUP,
    year_label, year = as.integer(substr(year_label, 1, 4)),
    post16 = ISPOST16 == 1, roll = TOTPUPS,
    income = fin_total_income, spend = fin_total_expenditure,
    balance = fin_in_year_balance, reserve = fin_revenue_reserve,
    income_pp = fin_total_income_pp, spend_pp = fin_total_expenditure_pp,
    staff = fin_staff_costs, source = fin_source) %>%
  mutate(reserve_pct = reserve / income,
         balance_pct = balance / income,
         staff_pct   = staff / income) %>%
  arrange(name, year)

stopifnot(nrow(fin_panel) > 0, !any(is.na(fin_panel$reserve)))

message(sprintf("  %d schools, %s to %s, %d school-years",
                n_distinct(fin_panel$name), min(fin_panel$year_label),
                max(fin_panel$year_label), nrow(fin_panel)))

# Academies report to a different year end, so their latest published
# return is a year behind the maintained schools'. Reporting a 2023-24
# academy against a 2024-25 maintained school is the like-for-like
# comparison available; pretending they are the same year is not.
fin_latest <- fin_panel %>%
  group_by(name) %>% slice_max(year, n = 1, with_ties = FALSE) %>%
  ungroup()

message("  latest published year by return type:")
print(as.data.frame(fin_latest %>% count(source, year_label)), row.names = FALSE)

# ---- 2. Where each school has moved over the four years -----------
fin_change <- fin_panel %>%
  group_by(urn, name, type, post16) %>%
  summarise(from_year = year_label[which.min(year)],
            to_year   = year_label[which.max(year)],
            years     = n(),
            roll_from = roll[which.min(year)],
            roll_to   = roll[which.max(year)],
            res_from  = reserve[which.min(year)],
            res_to    = reserve[which.max(year)],
            res_pct_from = reserve_pct[which.min(year)],
            res_pct_to   = reserve_pct[which.max(year)],
            .groups = "drop") %>%
  mutate(roll_change = roll_to - roll_from,
         res_change  = res_to - res_from,
         # The average annual movement in the reserve. Negative is a
         # burn rate; positive is a school rebuilding one.
         res_per_year = res_change / (years - 1),
         # Years of reserve left at the recent rate, for the schools
         # that are both in credit and running the reserve down. Infinite
         # for a school rebuilding, undefined for one already overdrawn -
         # both are stated as such rather than printed as a number.
         years_left = case_when(
           res_to <= 0        ~ NA_real_,
           res_per_year >= 0  ~ Inf,
           TRUE               ~ res_to / -res_per_year))

# ---- 3. What the projections do to the roll ----------------------
# Configuration A of the open scenarios is the city as it stands:
# Longhill at Ovingdean on 210, the current catchments. Its modelled
# Year 7 intake is run at 2026 and at 2035, which is the span the
# reception-cohort projections reach.

CFG_A <- grep("^A\\.", unique(os$central$config), value = TRUE)
stopifnot(length(CFG_A) == 1)

proj_years <- range(os$central$entry_year)
intake <- os$central %>%
  filter(config == CFG_A, entry_year %in% proj_years) %>%
  select(name, entry_year, intake, pan) %>%
  tidyr::pivot_wider(id_cols = c(name, pan), names_from = entry_year,
                     values_from = intake,
                     names_prefix = "intake_")
names(intake) <- sub("intake_", "y", names(intake))
intake <- intake %>%
  rename(intake_first = !!paste0("y", proj_years[1]),
         intake_last  = !!paste0("y", proj_years[2])) %>%
  mutate(intake_change = intake_last - intake_first,
         intake_pct    = intake_change / intake_first)

# The panel and the model name schools slightly differently: the panel
# uses the full legal name from Get Information About Schools, the model
# the short one. Matched on URN, which neither of them can get wrong.
urn_of <- setNames(as.character(SCHOOLS_OPEN$urn), SCHOOLS_OPEN$name)
intake$urn <- unname(urn_of[intake$name])
stopifnot(!any(is.na(intake$urn[intake$name != "Peacehaven Community School"])))

# ---- 3b. The baseline has to be the intake that happened -----------
# The model's own 2026 level is NOT the intake. It places every child in
# the projected cohort somewhere in the city, and the schools that are
# full absorb only their admission number, so the surplus lands on the
# schools that are not full. Against the offers the council actually
# made for September 2026 it puts 183 children into Longhill, which
# admitted 81, and 197 into Cardinal Newman, which admitted 360.
#
# In aggregate it is close - the city total is about 2% out - so the
# error is in the DISTRIBUTION, and a financial table keyed on the
# modelled level would have overstated the income Longhill still has to
# lose by more than double.
#
# So the baseline is the published offer count, and the model supplies
# only the TRAJECTORY: the proportional change in a school's intake
# between the first projection year and the last, which is driven by the
# cohort in its catchment shrinking. That ratio is the part of the model
# this exercise can lean on.

BASE_YEAR <- min(proj_years)

offers <- readRDS(file.path(PUBLIC_OUT, "factsheet_panel.rds"))$factsheets %>%
  filter(year == BASE_YEAR) %>%
  select(name, offers = off_total)

intake <- intake %>%
  left_join(offers, by = "name") %>%
  mutate(trajectory   = intake_last / intake_first,
         intake_proj  = offers * trajectory,
         base_change  = intake_proj - offers)

message(sprintf("\n  %d offers published for %d entry; the model puts %d children in the same schools",
                round(sum(intake$offers, na.rm = TRUE)), BASE_YEAR,
                round(sum(intake$intake_first[!is.na(intake$offers)]))))
message("  where the model and the published offers disagree most:")
print(as.data.frame(intake %>% filter(!is.na(offers)) %>%
  mutate(gap = intake_first - offers) %>%
  arrange(desc(abs(gap))) %>% head(4) %>%
  transmute(School = substr(name, 1, 26), PAN = pan, Offers = offers,
            Modelled = round(intake_first), Gap = round(gap))),
  row.names = FALSE)

# ---- 4. Exposure: the two halves put together --------------------
# Each Year 7 place lost takes five pupils off the 11-16 roll once the
# smaller cohort has worked through. At the school's own income per
# pupil, that is the steady-state annual income change it is heading
# for, in today's money.
YEARS_IN_SCHOOL <- 5

exposure <- fin_latest %>%
  select(urn, name, type, post16, year_label, roll, income, income_pp,
         reserve, reserve_pct, balance_pct, staff_pct, source) %>%
  left_join(intake %>% select(urn, pan, offers, intake_first, intake_last,
                              intake_proj, base_change, trajectory,
                              intake_pct), by = "urn") %>%
  mutate(
    roll_change_ss = YEARS_IN_SCHOOL * base_change,
    income_change  = roll_change_ss * income_pp,
    income_pct     = income_change / income,
    faith          = name %in% c("Cardinal Newman Catholic School",
                                 "King's School"))

# Every city school must have a published offer count to be rebased on,
# or a row silently drops out of the arithmetic.
stopifnot(!any(is.na(exposure$offers)), !any(is.na(exposure$income_change)))

# The threshold is the DfE's own trigger rather than a number chosen
# here: a maintained school whose revenue reserve falls below zero is in
# a licensed deficit and must agree a recovery plan with the local
# authority. 5% of income is the level at which the sector generally
# treats a reserve as thin.
RESERVE_THIN <- 0.05

exposure <- exposure %>%
  mutate(position = case_when(
    reserve_pct < 0                              ~ "In deficit",
    reserve_pct < RESERVE_THIN                   ~ "Thin reserve",
    TRUE                                         ~ "In credit"),
    position = factor(position,
                      c("In deficit", "Thin reserve", "In credit")),
    exposed = reserve_pct < RESERVE_THIN & intake_pct < 0) %>%
  arrange(reserve_pct)

message("\n  Position at the latest published year:")
print(as.data.frame(exposure %>%
  transmute(School = substr(name, 1, 24), Year = year_label, Roll = roll,
            `Reserve %` = sprintf("%+.1f", 100 * reserve_pct),
            `In-year %` = sprintf("%+.1f", 100 * balance_pct),
            `Staff %` = sprintf("%.0f", 100 * staff_pct),
            `Offers 2026` = round(offers),
            `Modelled 2026` = round(intake_first),
            `Projected 2035` = round(intake_proj),
            `Income change` = sprintf("%+.0fk", income_change / 1e3),
            Position = as.character(position))), row.names = FALSE)

# ---- 5. Cross-checks against what section 6 already says ----------
# The Longhill row here is built from the same panel as os$lh_fin, so
# the two must agree. They have disagreed once already, when a filter
# on the panel silently dropped a year.
lh_here <- fin_panel %>% filter(urn == "114581") %>% arrange(year)
lh_there <- os$lh_fin %>% arrange(year_label)
stopifnot(nrow(lh_here) == nrow(lh_there),
          all(abs(lh_here$reserve - lh_there$reserve) < 1),
          all(abs(lh_here$income_pp - lh_there$income_pp) < 1))
message("\n  Longhill agrees with the table already in section 6.")

saveRDS(list(
  panel = fin_panel, latest = fin_latest, change = fin_change,
  intake = intake, exposure = exposure, offers = offers,
  proj_years = proj_years, base_year = BASE_YEAR, config = CFG_A,
  years_in_school = YEARS_IN_SCHOOL, reserve_thin = RESERVE_THIN,
  la = CITY_LA, run_at = Sys.time()),
  file.path(PUBLIC_OUT, "school_finance.rds"))

message("\nSaved output/school_finance.rds")
