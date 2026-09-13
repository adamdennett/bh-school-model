# 01a_factsheet_panel.R — The published allocation factsheets, 2010-2026
# ======================================================================
# Brighton & Hove publishes a Year 7 Allocation Factsheet every year,
# giving for every school the preferences RECEIVED and offers MADE, split
# by preference rank. Sixteen years are transcribed in
# BH_Schools_2/brighton_and_hove_allocations.qmd; this lifts them into a
# tidy panel and derives a multi-year attractiveness measure.
#
# This runs BEFORE 01_open_inputs.R, which uses the multi-year measure.
# The identification test that uses these data sits in 01c, after the
# model inputs exist.
#
# Output: public/output/factsheet_panel.rds, public/output/of_*.csv
# ======================================================================

source(here::here("public", "R", "00_open_core.R"))

ALLOCATIONS_QMD <- Sys.getenv("OPEN_ALLOCATIONS",
                              "E:/BH_Schools_2/brighton_and_hove_allocations.qmd")

message("\n=== Extracting the published factsheets ===")

txt <- paste(readLines(assert_open(ALLOCATIONS_QMD), warn = FALSE), collapse = "\n")

# Each year is a self-contained `yr_7_admissions_YYYY <- data.frame(...)`.
# Walk the parentheses to find the end of each call and evaluate just that,
# in a clean environment — the surrounding document is never run.
starts <- gregexpr("yr_7_admissions_[0-9]{4}\\s*<-\\s*data\\.frame\\(", txt)[[1]]
env <- new.env()
for (s in starts) {
  i <- s; depth <- 0L; started <- FALSE
  while (i <= nchar(txt)) {
    ch <- substr(txt, i, i)
    if (ch == "(") { depth <- depth + 1L; started <- TRUE }
    if (ch == ")") depth <- depth - 1L
    if (started && depth == 0L) break
    i <- i + 1L
  }
  try(eval(parse(text = substr(txt, s, i)), envir = env), silent = TRUE)
}
message("  Year frames recovered: ", length(ls(env)))

# Cells are "preferences (offers)"
split_nm <- function(v) {
  v <- as.character(v)
  list(pref = suppressWarnings(as.numeric(gsub(",", "", trimws(sub(" \\(.*$", "", v))))),
       off  = suppressWarnings(as.numeric(gsub("[),*]", "", sub("^.*\\(", "", v)))))
}

NAME_MAP <- c(
  "Blatchington Mill"                    = "Blatchington Mill School",
  "Brighton Aldridge Community Academy"  = "Brighton Aldridge Community Academy",
  "Cardinal Newman"                      = "Cardinal Newman Catholic School",
  "Dorothy Stringer"                     = "Dorothy Stringer School",
  "Hove Park"                            = "Hove Park School",
  "Kings School"                         = "King's School",
  "Longhill High"                        = "Longhill High School",
  "Longhill"                             = "Longhill High School",
  "Patcham High"                         = "Patcham High School",
  "Portslade Aldridge Community Academy" = "Portslade Aldridge Community Academy",
  "Portslade Community College"          = "Portslade Aldridge Community Academy",
  "Varndean"                             = "Varndean School"
)

factsheets <- purrr::map_dfr(sort(ls(env)), function(o) {
  d  <- get(o, envir = env)
  yr <- as.integer(sub(".*_", "", o))
  p1 <- split_nm(d$No_1st_pref); p2 <- split_nm(d$No_2nd_pref)
  p3 <- split_nm(d$No_3rd_pref); tt <- split_nm(d$Total)
  tibble(year = yr, school_raw = d$School,
         pref1 = p1$pref, off1 = p1$off, pref2 = p2$pref, off2 = p2$off,
         pref3 = p3$pref, off3 = p3$off,
         pref_total = tt$pref, off_total = tt$off)
}) %>%
  filter(school_raw != "Total") %>%
  mutate(name = unname(NAME_MAP[school_raw])) %>%
  filter(!is.na(name)) %>%
  select(-school_raw)

message("  Panel: ", nrow(factsheets), " school-years, ",
        n_distinct(factsheets$year), " years (",
        min(factsheets$year), "-", max(factsheets$year), ")")

# --- Do the offers square with the admission numbers? ----------------
# A school cannot offer many more places than its PAN. Where it looks
# like it has, either the over-offer is real - King's ran 15 above its
# 165 before its admission number became 180 for 2026 - or the PAN in
# 00_open_core.R is wrong.
#
# Varndean's 2026 number was wrong that way: recorded as 270 while the
# school offered 300. Nothing caught it, because a school offering over
# its admission number is not by itself absurd, and the figure sat in
# published tables for some time. This check cannot tell the two cases
# apart. It refuses to let either pass in silence.
PAN_OVER_ALLOWED <- c("King's School" = 15)

pan_check <- factsheets %>%
  filter(name != "Total", year %in% c(2024, 2026)) %>%
  inner_join(SCHOOLS_OPEN %>% select(name, pan2024, pan2026), by = "name") %>%
  mutate(pan     = if_else(year == 2026, pan2026, pan2024),
         over    = off_total - pan,
         allowed = coalesce(unname(PAN_OVER_ALLOWED[name]), 0)) %>%
  filter(over > 0)

if (nrow(pan_check)) {
  message("\n  Schools offering above their admission number:")
  print(as.data.frame(pan_check %>%
    transmute(School = name, Year = year, Offers = off_total, PAN = pan,
              Over = over, Allowed = allowed)), row.names = FALSE)
  bad <- pan_check %>% filter(over > allowed)
  if (nrow(bad))
    stop("offers exceed the admission number beyond the recorded allowance: ",
         paste(sprintf("%s %d (+%d)", bad$name, bad$year, bad$over),
               collapse = "; "),
         "\n  Either the PAN in 00_open_core.R is wrong, or the over-offer",
         " is real and belongs in PAN_OVER_ALLOWED.")
}

# --- Multi-year attractiveness ---------------------------------------
# A single admissions round is noisy. Averaging the recent rounds gives a
# far steadier measure, which matters because the sensitivity analysis
# found attractiveness, not distance decay, to be the dominant unknown.
#
# The window is five rounds. It used to be 2020 onwards, which was seven.
# Five is what the strategic view uses to weight attractiveness, and one
# window across both is worth more than either window on its own: the
# same quantity computed two ways in two documents is how the two came
# to disagree elsewhere. Five is also a compromise between two bad ends.
# A single round is the noisiest possible measure of exactly the schools
# in question - year-to-year variation in the weighted score is 25% at
# BACA and 20% at Longhill. The whole 2010-2026 series describes a city
# that no longer exists, and systematically flatters the schools whose
# demand has since fallen.
#
# TWO SPECIFICATIONS ARE BUILT, differing only in which ranks they count:
#
#   pref1            first preferences alone
#   pref_weighted    ranks 1-3, geometrically discounted
#
# The paired catchments are why the second exists. A family in the
# Stringer/Varndean or Hove Park/Blatchington Mill catchment must rank
# two local schools against each other, so the catchment's first
# preferences are split between them and a first-preference count reads
# a school families put second as one nobody wanted. The discount is
# geometric in the rank: a second choice counts WPREF_DECAY of a first,
# a third that squared.
#
# Both are averaged over the same window so that the difference between
# them is the ranks and nothing else.

PREF_YEARS  <- 5
WPREF_DECAY <- 0.5

pans        <- setNames(SCHOOLS_OPEN$pan2024, SCHOOLS_OPEN$name)
ATTRACT_FROM <- max(factsheets$year) - PREF_YEARS + 1

attract_panel <- factsheets %>%
  filter(year >= ATTRACT_FROM) %>%
  group_by(name) %>%
  summarise(pref1 = mean(pref1, na.rm = TRUE),
            pref2 = mean(pref2, na.rm = TRUE),
            pref3 = mean(pref3, na.rm = TRUE),
            off_total = mean(off_total, na.rm = TRUE),
            n_years = n(), .groups = "drop") %>%
  mutate(pan = unname(pans[name]),
         pref_weighted   = pref1 + WPREF_DECAY * pref2 + WPREF_DECAY^2 * pref3,
         prefs_per_place = pref1 / pan,
         wprefs_per_place = pref_weighted / pan,
         W_prefs_multi   = prefs_per_place / mean(prefs_per_place),
         W_wprefs_multi  = wprefs_per_place / mean(wprefs_per_place),
         offer_rate      = off_total / pref1)

stopifnot(all(attract_panel$n_years == PREF_YEARS))

message("\n  Multi-year attractiveness (", ATTRACT_FROM, "-",
        max(factsheets$year), ", ", PREF_YEARS, " rounds, decay ",
        WPREF_DECAY, "):")
print(as.data.frame(attract_panel %>%
  transmute(School = name, PAN = pan,
            `1st` = round(pref1), `2nd` = round(pref2), `3rd` = round(pref3),
            `Prefs/place` = round(prefs_per_place, 2),
            `Wtd/place` = round(wprefs_per_place, 2),
            W_prefs = round(W_prefs_multi, 2),
            W_wprefs = round(W_wprefs_multi, 2)) %>%
  arrange(desc(W_wprefs))), row.names = FALSE)

# Where the two specifications disagree is the interesting part, and it
# is not noise: it is the second and third preferences that a
# first-preference count throws away.
message("\n  Largest gaps between the two (W_wprefs - W_prefs):")
print(as.data.frame(attract_panel %>%
  transmute(School = name, gap = round(W_wprefs_multi - W_prefs_multi, 3)) %>%
  arrange(desc(abs(gap))) %>% head(4)), row.names = FALSE)

# --- Longhill's published decline ------------------------------------

lh_series <- factsheets %>%
  filter(name == "Longhill High School") %>%
  select(year, pref1, pref2, off_total) %>%
  arrange(year)

message("\n  Longhill, straight from the published factsheets:")
print(as.data.frame(lh_series %>% filter(year >= 2016)), row.names = FALSE)

saveRDS(list(factsheets = factsheets, attract_panel = attract_panel,
             lh_series = lh_series, attract_from = ATTRACT_FROM,
             pref_years = PREF_YEARS, wpref_decay = WPREF_DECAY,
             run_at = Sys.time()),
        file.path(PUBLIC_OUT, "factsheet_panel.rds"))

readr::write_csv(factsheets,    file.path(PUBLIC_OUT, "of_factsheet_panel.csv"))
readr::write_csv(attract_panel, file.path(PUBLIC_OUT, "of_attractiveness_multiyear.csv"))

message("\nSaved public/output/factsheet_panel.rds")
