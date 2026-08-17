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

# --- Multi-year attractiveness ---------------------------------------
# A single admissions round is noisy. Averaging the recent rounds gives a
# far steadier measure, which matters because the sensitivity analysis
# found attractiveness, not distance decay, to be the dominant unknown.

ATTRACT_FROM <- 2020
pans <- setNames(SCHOOLS_OPEN$pan2024, SCHOOLS_OPEN$name)

attract_panel <- factsheets %>%
  filter(year >= ATTRACT_FROM) %>%
  group_by(name) %>%
  summarise(pref1 = mean(pref1, na.rm = TRUE),
            pref2 = mean(pref2, na.rm = TRUE),
            off_total = mean(off_total, na.rm = TRUE),
            n_years = n(), .groups = "drop") %>%
  mutate(pan = unname(pans[name]),
         prefs_per_place = pref1 / pan,
         W_prefs_multi = prefs_per_place / mean(prefs_per_place),
         offer_rate = off_total / pref1)

message("\n  Multi-year attractiveness (", ATTRACT_FROM, "-", max(factsheets$year), "):")
print(as.data.frame(attract_panel %>%
  transmute(School = name, PAN = pan,
            `Mean 1st prefs` = round(pref1), `Mean 2nd prefs` = round(pref2),
            `Prefs/place` = round(prefs_per_place, 2),
            W = round(W_prefs_multi, 2))), row.names = FALSE)

# --- Longhill's published decline ------------------------------------

lh_series <- factsheets %>%
  filter(name == "Longhill High School") %>%
  select(year, pref1, pref2, off_total) %>%
  arrange(year)

message("\n  Longhill, straight from the published factsheets:")
print(as.data.frame(lh_series %>% filter(year >= 2016)), row.names = FALSE)

saveRDS(list(factsheets = factsheets, attract_panel = attract_panel,
             lh_series = lh_series, attract_from = ATTRACT_FROM,
             run_at = Sys.time()),
        file.path(PUBLIC_OUT, "factsheet_panel.rds"))

readr::write_csv(factsheets,    file.path(PUBLIC_OUT, "of_factsheet_panel.csv"))
readr::write_csv(attract_panel, file.path(PUBLIC_OUT, "of_attractiveness_multiyear.csv"))

message("\nSaved public/output/factsheet_panel.rds")
