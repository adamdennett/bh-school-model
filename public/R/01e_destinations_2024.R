# public/R/01e_destinations_2024.R — where each catchment's children were offered a place
# ======================================================================
# The council's answer to a Freedom of Information request, published on
# WhatDoTheyKnow, gives for the 2024 round (September 2024 entry) every
# school each home catchment's children were offered a place at on
# national offer day - including schools outside Brighton & Hove - and
# the school they were attending in the autumn. It was collated into a
# workbook, "Where everybody went to school in 2024".
#
# It is published and aggregated to catchment by school, with counts
# under 5 (and in two places under 10) suppressed by the council, so it
# can be used in the open bundle. It supplies what the adjudicator's
# Table 11 cannot: WHICH school outside the city those children went to.
# Longhill's catchment sent 38 to Priory School in Lewes, and fewer than
# five each to Peacehaven and Seahaven.
#
# Only the offer-day column is transcribed: offer day is what the model
# and every other comparison in the bundle is on. A suppressed count is
# kept as an interval, not a guess.
#
# Source: https://www.whatdotheyknow.com/request/schools_admissions_breakdowns_fo
#
# Output: public/output/destinations_2024.rds
# ======================================================================

source(here::here("public", "R", "00_open_core.R"))

suppressPackageStartupMessages({ library(tidyverse) })

message("\n=== Where each catchment's children were offered a place, 2024 ===")

SOURCE_2024 <- "Brighton & Hove City Council, FOI response published on WhatDoTheyKnow: https://www.whatdotheyknow.com/request/schools_admissions_breakdowns_fo"

# n is the published count; "<5" and "<10" are suppressed by the council.
offers <- tribble(
  ~catchment,    ~school,                                      ~n,
  "PACA",        "Brighton Aldridge Community Academy",        "<5",
  "PACA",        "Cardinal Newman Catholic School",            "24",
  "PACA",        "Davison CE High School for Girls, Worthing", "<5",
  "PACA",        "Hill Park School",                           "<5",
  "PACA",        "Hove Park School",                           "38",
  "PACA",        "King's School",                              "8",
  "PACA",        "Longhill High School",                       "<5",
  "PACA",        "Portslade Aldridge Community Academy",       "150",
  "Hove_Blatch", "ARK Globe Academy",                          "<5",
  "Hove_Blatch", "Blatchington Mill School",                   "326",
  "Hove_Blatch", "Brighton Aldridge Community Academy",        "11",
  "Hove_Blatch", "Cardinal Newman Catholic School",            "121",
  "Hove_Blatch", "Davison CE High School for Girls, Worthing", "<5",
  "Hove_Blatch", "Dorothy Stringer School",                    "<10",
  "Hove_Blatch", "Hill Park School",                           "<5",
  "Hove_Blatch", "Hove Park School",                           "114",
  "Hove_Blatch", "King's School",                              "134",
  "Hove_Blatch", "Patcham High School",                        "<5",
  "Hove_Blatch", "Portslade Aldridge Community Academy",       "34",
  "Hove_Blatch", "Priory School",                              "<5",
  "Hove_Blatch", "Shoreham Academy",                           "<5",
  "Hove_Blatch", "St Paul's Catholic College",                 "<5",
  "Hove_Blatch", "The Sir Robert Woodard Academy",             "<5",
  "DS_Varndean", "Blatchington Mill School",                   "<5",
  "DS_Varndean", "Brighton Aldridge Community Academy",        "35",
  "DS_Varndean", "Cardinal Newman Catholic School",            "59",
  "DS_Varndean", "Dorothy Stringer School",                    "288",
  "DS_Varndean", "Hill Park School",                           "9",
  "DS_Varndean", "Hove Park School",                           "7",
  "DS_Varndean", "King's School",                              "12",
  "DS_Varndean", "Longhill High School",                       "<5",
  "DS_Varndean", "Patcham High School",                        "<5",
  "DS_Varndean", "Portslade Aldridge Community Academy",       "7",
  "DS_Varndean", "Priory School",                              "<5",
  "DS_Varndean", "Shoreham Academy",                           "<5",
  "DS_Varndean", "St Paul's Catholic College",                 "<5",
  "DS_Varndean", "Sutton Grammar School",                      "<5",
  "DS_Varndean", "Varndean School",                            "289",
  "Longhill",    "Brighton Aldridge Community Academy",        "27",
  "Longhill",    "Cardinal Newman Catholic School",            "64",
  "Longhill",    "Dorothy Stringer School",                    "12",
  "Longhill",    "Hill Park School",                           "<5",
  "Longhill",    "Hove Park School",                           "8",
  "Longhill",    "Longhill High School",                       "98",
  "Longhill",    "Patcham High School",                        "<5",
  "Longhill",    "Peacehaven Community School",                "<5",
  "Longhill",    "Portslade Aldridge Community Academy",       "<5",
  "Longhill",    "Priory School",                              "38",
  "Longhill",    "Seahaven Academy",                           "<5",
  "Longhill",    "Varndean School",                            "<5",
  "BACA",        "Brighton Aldridge Community Academy",        "80",
  "BACA",        "Cardinal Newman Catholic School",            "21",
  "BACA",        "Dorothy Stringer School",                    "<10",
  "BACA",        "Hill Park School",                           "<5",
  "BACA",        "Hove Park School",                           "<5",
  "BACA",        "King's School",                              "<5",
  "BACA",        "Longhill High School",                       "<5",
  "BACA",        "Patcham High School",                        "<5",
  "BACA",        "Portslade Aldridge Community Academy",       "<5",
  "BACA",        "Priory School",                              "<5",
  "BACA",        "Varndean School",                            "<5",
  "Patcham",     "Brighton Aldridge Community Academy",        "16",
  "Patcham",     "Cardinal Newman Catholic School",            "25",
  "Patcham",     "Dorothy Stringer School",                    "11",
  "Patcham",     "Hill Park School",                           "<5",
  "Patcham",     "King's School",                              "<5",
  "Patcham",     "Patcham High School",                        "219",
  "Patcham",     "Portslade Aldridge Community Academy",       "<5",
  "Patcham",     "Varndean School",                            "6",
  "Patcham",     "Wallington County Grammar School",           "<5"
) %>%
  mutate(suppressed = startsWith(n, "<"),
         lo = if_else(suppressed, 1, suppressWarnings(as.numeric(n))),
         hi = case_when(n == "<5" ~ 4, n == "<10" ~ 9,
                        TRUE ~ suppressWarnings(as.numeric(n))))
stopifnot(!any(is.na(offers$lo)), !any(is.na(offers$hi)))

# The council's own totals of children offered a place, per catchment.
totals <- tribble(
  ~catchment,    ~offered,
  "PACA",        229,
  "Hove_Blatch", 755,
  "DS_Varndean", 705,
  "Longhill",    263,
  "BACA",        133,
  "Patcham",     285)

# Schools outside Brighton & Hove, and the area they are in. Hill Park and
# Downs View are Brighton & Hove special schools; they are not destinations
# the mainstream model has, and are not out of the city.
OUT_OF_CITY_2024 <- tribble(
  ~school,                                      ~area,
  "Priory School",                              "East Sussex",
  "Peacehaven Community School",                "East Sussex",
  "Seahaven Academy",                           "East Sussex",
  "Seaford Head School",                        "East Sussex",
  "Shoreham Academy",                           "West Sussex",
  "St Paul's Catholic College",                 "West Sussex",
  "The Sir Robert Woodard Academy",             "West Sussex",
  "Davison CE High School for Girls, Worthing", "West Sussex",
  "ARK Globe Academy",                          "London",
  "Sutton Grammar School",                      "London",
  "Wallington County Grammar School",           "London")

outside_2024 <- offers %>% inner_join(OUT_OF_CITY_2024, by = "school")

message("\n  Offers outside Brighton & Hove, 2024 round, by home catchment:")
print(as.data.frame(outside_2024 %>% select(catchment, school, area, n)), row.names = FALSE)
lh <- outside_2024 %>% filter(catchment == "Longhill")
message(sprintf("\n  Longhill's catchment: %s to Priory School, Lewes, of %d offered a place.",
                lh$n[lh$school == "Priory School"], totals$offered[totals$catchment == "Longhill"]))

saveRDS(list(offers = offers, totals = totals, out_of_city = OUT_OF_CITY_2024,
             outside = outside_2024, round = "2024 (September 2024 entry), national offer day",
             source = SOURCE_2024, run_at = Sys.time()),
        file.path(PUBLIC_OUT, "destinations_2024.rds"))
message("\nSaved public/output/destinations_2024.rds")
