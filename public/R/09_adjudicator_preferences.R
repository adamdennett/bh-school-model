# public/R/09_adjudicator_preferences.R — catchment-level preferences
# ======================================================================
# The council produced this to the Schools Adjudicator in the 2026/27
# case: for each catchment area and each of three admissions rounds, the
# number of children expressing a first, second and third preference for
# every secondary school, and the number allocated on national offer day.
#
# It is a catchment-level origin-destination matrix — the thing
# 02_sensitivity_envelope.R identifies as the largest single source of
# uncertainty in the open model. Every cell is an aggregate over a
# catchment; nothing in it identifies anyone.
#
# PROVENANCE: received as an objector to the case, not published. See
# "Where the data came from" in the report. It is used here because it is
# non-disclosive and because the analysis it supports is exactly what the
# report argues the public evidence base is missing.
#
# Output: public/output/adjudicator_preferences.rds
# ======================================================================

source(here::here("public", "R", "00_open_core.R"))

suppressPackageStartupMessages({ library(tidyverse) })

message("\n=== Catchment preferences, adjudicator evidence bundle ===")

DOCX <- Sys.getenv(
  "OPEN_JFIP_81",
  file.path(here::here(), "JFIP response BHCC", "JFIP response BHCC",
            "8 Preference and allocation data for catchment areas - COMPLETE",
            "8.1 preferences and allocation data.docx"))

if (!file.exists(DOCX)) {
  message("  ! 8.1 document not found; skipping. Set OPEN_JFIP_81 to enable.")
  quit(save = "no", status = 0)
}

td <- tempfile(); dir.create(td); utils::unzip(DOCX, exdir = td)
xml <- paste(readLines(file.path(td, "word", "document.xml"), warn = FALSE),
             collapse = "")

# The document alternates headings and tables. Walk it in order so each
# table inherits the year and catchment heading immediately above it.
blocks <- regmatches(xml, gregexpr("<w:tbl>.*?</w:tbl>|<w:p[ >].*?</w:p>", xml, perl = TRUE))[[1]]

strip <- function(s) trimws(gsub("<[^>]+>", "", s))

cells_of <- function(tbl) {
  rows <- regmatches(tbl, gregexpr("<w:tr[ >].*?</w:tr>", tbl, perl = TRUE))[[1]]
  purrr::map(rows, function(r) {
    tc <- regmatches(r, gregexpr("<w:tc>.*?</w:tc>", r, perl = TRUE))[[1]]
    strip(tc)
  })
}

year <- NA_character_; area <- NA_character_
out <- list()

for (b in blocks) {
  if (startsWith(b, "<w:tbl>")) {
    if (is.na(area)) next
    rows <- cells_of(b)
    hdr  <- rows[[1]]
    body <- rows[-1]
    d <- purrr::map_dfr(body, function(cs) {
      if (length(cs) < 2) return(NULL)
      num <- suppressWarnings(as.numeric(gsub("[^0-9]", "", cs[-1])))
      tibble(school = cs[1],
             pref1 = num[1], pref2 = num[2], pref3 = num[3], allocated = num[4])
    })
    out[[length(out) + 1]] <- d %>% mutate(round = year, area = area)
  } else {
    t <- strip(b)
    if (grepl("^20[0-9]{2}/[0-9]{2}", t)) year <- sub(":", "", t)
    if (grepl("^CA-[A-F]", t))            area <- sub("^(CA-[A-F]).*", "\\1", t)
  }
}

prefs <- bind_rows(out) %>%
  filter(nzchar(school), !grepl("^Schools$", school)) %>%
  mutate(across(c(pref1, pref2, pref3, allocated), ~ replace_na(.x, 0)))

message(sprintf("  %d rows across %d rounds and %d catchment areas",
                nrow(prefs), n_distinct(prefs$round), n_distinct(prefs$area)))

# ---- Map catchment areas to their schools ----------------------------
# CA-A..CA-F are the council's own labels for the six catchments. The
# mapping is recovered from the data itself: the school with the largest
# share of a catchment's allocations is that catchment's school, which
# avoids hard-coding a lookup that could go stale.

area_map <- prefs %>%
  group_by(area, school) %>%
  summarise(alloc = sum(allocated), .groups = "drop_last") %>%
  slice_max(alloc, n = 2, with_ties = FALSE) %>%
  ungroup()

message("\n=== Which schools each catchment area's children go to most ===")
print(as.data.frame(area_map), row.names = FALSE)

# ---- The question the open model cannot otherwise answer -------------
# For each catchment, what share of first preferences for the catchment's
# own school converted into an allocation? This is the closest published
# proxy for whether catchment priority does any work.

totals <- prefs %>%
  group_by(round, school) %>%
  summarise(pref1 = sum(pref1), pref2 = sum(pref2), pref3 = sum(pref3),
            allocated = sum(allocated), .groups = "drop") %>%
  mutate(conv1 = allocated / pmax(pref1, 1),
         prefs_total = pref1 + pref2 + pref3)

message("\n=== City totals by school, all rounds pooled ===")
print(as.data.frame(totals %>%
  group_by(school) %>%
  summarise(pref1 = sum(pref1), allocated = sum(allocated),
            `alloc per 1st pref` = round(sum(allocated) / pmax(sum(pref1), 1), 2),
            .groups = "drop") %>%
  arrange(desc(`alloc per 1st pref`))), row.names = FALSE)

# ---- Longhill specifically -------------------------------------------
LH <- "Longhill High School"
lh <- prefs %>% filter(school == LH) %>%
  group_by(round, area) %>%
  summarise(across(c(pref1, pref2, pref3, allocated), sum), .groups = "drop")

message("\n=== Longhill: preferences and allocations by catchment area ===")
print(as.data.frame(lh %>% arrange(round, desc(allocated))), row.names = FALSE)

lh_tot <- lh %>% group_by(round) %>%
  summarise(across(c(pref1, pref2, pref3, allocated), sum), .groups = "drop") %>%
  mutate(alloc_per_p1 = round(allocated / pmax(pref1, 1), 2))

message("\n=== Longhill, pooled by round ===")
print(as.data.frame(lh_tot), row.names = FALSE)
message("\n  Allocations exceeding first preferences means the school is")
message("  admitting on lower preferences - i.e. it is undersubscribed and")
message("  turning nobody away, which is the condition under which catchment")
message("  priority does no work.")

saveRDS(list(prefs = prefs, totals = totals, area_map = area_map,
             longhill = lh, longhill_pooled = lh_tot,
             rounds = sort(unique(prefs$round)),
             source = "BHCC evidence to the Schools Adjudicator, item 8.1",
             run_at = Sys.time()),
        file.path(PUBLIC_OUT, "adjudicator_preferences.rds"))

message("\nSaved public/output/adjudicator_preferences.rds")
