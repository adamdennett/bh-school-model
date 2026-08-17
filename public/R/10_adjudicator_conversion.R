# public/R/10_adjudicator_conversion.R — preference conversion by school
# ======================================================================
# Item 6 of the council's evidence to the Schools Adjudicator: for each
# school and each of three admissions rounds, the number of on-time
# preferences at each rank AND, in brackets, how many of those were
# offered a place. A final column gives the number actually admitted in
# September, from the October school census.
#
# The bracketed figure is what makes this valuable. It gives a direct
# first-preference success rate per school — the cleanest published
# measure of which schools ration places and which do not. That is the
# quantity the catchment-priority argument turns on, and no other open
# source carries it.
#
# PROVENANCE: as 09_adjudicator_preferences.R. Received as a party to the
# 2026/27 case; aggregate to school level; not published. The source
# documents are not committed to this repository.
#
# Output: public/output/adjudicator_conversion.rds
# ======================================================================

source(here::here("public", "R", "00_open_core.R"))

suppressPackageStartupMessages({ library(tidyverse) })

message("\n=== Preference conversion by school, adjudicator evidence ===")

DOCX <- Sys.getenv(
  "OPEN_JFIP_6",
  file.path(here::here(), "JFIP response BHCC", "JFIP response BHCC",
            "6 Preference and admission data for past 3 years - COMPLETE",
            "6 Preference data.docx"))

if (!file.exists(DOCX)) {
  message("  ! item 6 not found; skipping. Set OPEN_JFIP_6 to enable.")
  quit(save = "no", status = 0)
}

td <- tempfile(); dir.create(td); utils::unzip(DOCX, exdir = td)
xml <- paste(readLines(file.path(td, "word", "document.xml"), warn = FALSE),
             collapse = "")

blocks <- regmatches(xml, gregexpr("<w:tbl>.*?</w:tbl>|<w:p[ >].*?</w:p>",
                                   xml, perl = TRUE))[[1]]
strip <- function(s) trimws(gsub("<[^>]+>", "", s))

#' "300 (260)" -> c(named = 300, offered = 260)
split_np <- function(s) {
  named   <- suppressWarnings(as.numeric(gsub("[^0-9]", "",
                                              sub("\\s*\\(.*$", "", s))))
  offered <- suppressWarnings(as.numeric(gsub("[^0-9]", "",
                                              sub("^[^(]*\\(", "", s))))
  c(named = named, offered = offered)
}

year <- NA_character_; out <- list()

for (b in blocks) {
  if (startsWith(b, "<w:tbl>")) {
    if (is.na(year)) next
    rows <- regmatches(b, gregexpr("<w:tr[ >].*?</w:tr>", b, perl = TRUE))[[1]]
    d <- purrr::map_dfr(rows[-1], function(r) {
      cs <- strip(regmatches(r, gregexpr("<w:tc>.*?</w:tc>", r, perl = TRUE))[[1]])
      if (length(cs) < 5) return(NULL)
      p1 <- split_np(cs[2]); p2 <- split_np(cs[3]); p3 <- split_np(cs[4])
      tibble(school = cs[1],
             p1_named = p1[["named"]], p1_offered = p1[["offered"]],
             p2_named = p2[["named"]], p2_offered = p2[["offered"]],
             p3_named = p3[["named"]], p3_offered = p3[["offered"]],
             admitted_sept = suppressWarnings(as.numeric(gsub("[^0-9]", "",
                                                             cs[length(cs)]))))
    })
    out[[length(out) + 1]] <- d %>% mutate(round = year)
  } else {
    t <- strip(b)
    if (grepl("^20[0-9]{2}/[0-9]{2}", t)) year <- sub(":", "", t)
  }
}

conv <- bind_rows(out) %>%
  filter(nzchar(school), !grepl("^(School|Total)", school)) %>%
  # The factsheets name schools inconsistently across rounds - "Longhill"
  # and "Longhill High", with and without acronyms in brackets. Normalise
  # before pooling or each school appears twice.
  mutate(school = trimws(sub("\\s*\\(.*\\)$", "", school)),
         school = dplyr::case_when(
           grepl("^Longhill", school)            ~ "Longhill High School",
           grepl("^Brighton Aldridge", school)   ~ "Brighton Aldridge Community Academy",
           grepl("^Portslade Aldridge", school)  ~ "Portslade Aldridge Community Academy",
           grepl("^Blatchington", school)        ~ "Blatchington Mill School",
           grepl("^Dorothy Stringer", school)    ~ "Dorothy Stringer School",
           grepl("^Hove Park", school)           ~ "Hove Park School",
           grepl("^Patcham", school)             ~ "Patcham High School",
           grepl("^Varndean", school)            ~ "Varndean School",
           grepl("^Cardinal Newman", school)     ~ "Cardinal Newman Catholic School",
           grepl("^King", school)                ~ "King's School",
           TRUE ~ school)) %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0)),
         p1_rate  = p1_offered / pmax(p1_named, 1),
         rationing = 1 - p1_rate)

message(sprintf("  %d school-rounds across %s",
                nrow(conv), paste(sort(unique(conv$round)), collapse = ", ")))

# ---- The headline: who turns first preferences away -------------------

pooled <- conv %>%
  group_by(school) %>%
  summarise(across(c(p1_named, p1_offered, p2_named, p2_offered), sum),
            .groups = "drop") %>%
  mutate(p1_rate = p1_offered / pmax(p1_named, 1),
         turned_away = p1_named - p1_offered) %>%
  arrange(p1_rate)

message("\n=== First-preference success rate, three rounds pooled ===")
print(as.data.frame(pooled %>%
  transmute(School = school, `1st prefs` = p1_named, Offered = p1_offered,
            `Turned away` = turned_away,
            `Success rate` = sprintf("%.0f%%", 100 * p1_rate))),
  row.names = FALSE)

message("\n  A school offering a place to essentially every first-preference")
message("  applicant is not rationing, and cannot be conferring an advantage")
message("  through catchment priority. Priority only operates where a school")
message("  has to choose between applicants.")

by_round <- conv %>%
  transmute(round, school, p1_named, p1_offered, p1_rate,
            admitted_sept) %>%
  arrange(school, round)

saveRDS(list(conv = conv, pooled = pooled, by_round = by_round,
             rounds = sort(unique(conv$round)),
             source = "BHCC evidence to the Schools Adjudicator, item 6",
             run_at = Sys.time()),
        file.path(PUBLIC_OUT, "adjudicator_conversion.rds"))

message("\nSaved public/output/adjudicator_conversion.rds")
