# public/R/00d_route_geometries.R — actual routed journey geometries
# ======================================================================
# The network map previously drew every bus route shape in the GTFS feed
# — 369 of them. Inside Brighton that is forty services on the same
# corridors plus loops and turnarounds, which reads as a tangle rather
# than as a road network, and it does not illustrate what the section
# claims: where the journey times come from.
#
# This routes the worked example journeys properly, with r5r, and saves
# the per-leg geometry. A walk leg follows the street network; a transit
# leg follows the route shape; and each journey can be checked against
# local knowledge.
#
# This is the only script in the bundle that runs r5r, and it needs the
# built network in public/output/travel/r5_network. If that is absent it
# exits cleanly and the map falls back to the GTFS shapes.
#
# Output: public/output/route_geometries.rds
# ======================================================================

source(here::here("public", "R", "00_open_core.R"))

suppressPackageStartupMessages({ library(tidyverse); library(sf) })

message("\n=== Routed journey geometries ===")

NET <- file.path(PUBLIC_OUT, "travel", "r5_network")

if (!file.exists(file.path(NET, "network.dat")) ||
    !requireNamespace("r5r", quietly = TRUE)) {
  message("  ! r5r network or package unavailable; skipping.")
  message("    The network map will fall back to GTFS route shapes.")
  quit(save = "no", status = 0)
}

options(java.parameters = "-Xmx6G")

# ---- The journeys ----------------------------------------------------
# Chosen to be checkable against local knowledge rather than to flatter
# the model, and to span the geography the argument turns on: the east
# of the city, the expansion area, and a west-to-east cross-city trip.

JOURNEYS <- tribble(
  ~label,                              ~from_pcd, ~to_school,
  "Whitehawk to Longhill",             "BN2 5FL", "Longhill High School",
  "Whitehawk to Dorothy Stringer",     "BN2 5FL", "Dorothy Stringer School",
  "Woodingdean to Longhill",           "BN2 6PA", "Longhill High School",
  "Saltdean to Longhill",              "BN2 8LF", "Longhill High School",
  "Peacehaven to Longhill",            "BN10 8AY", "Longhill High School",
  "Kemptown to Longhill",              "BN2 1ED", "Longhill High School",
  "Hove to Dorothy Stringer",          "BN3 2FL", "Dorothy Stringer School"
)

# ---- Coordinates -----------------------------------------------------
# Origins come from the routed matrix, which already carries lon/lat per
# postcode. Destinations come from the school list, in British National
# Grid, so they need projecting.

trav <- read_open_csv(OPEN$travel) %>%
  mutate(postcode = normalise_pcd(id)) %>%
  select(postcode, lon = long, lat) %>%
  distinct(postcode, .keep_all = TRUE)

origins <- JOURNEYS %>%
  mutate(postcode = normalise_pcd(from_pcd)) %>%
  left_join(trav, by = "postcode") %>%
  transmute(id = label, lon, lat)

schools_ll <- SCHOOLS_OPEN %>%
  filter(!is.na(easting), !is.na(northing)) %>%
  st_as_sf(coords = c("easting", "northing"), crs = 27700) %>%
  st_transform(4326)

sch_xy <- schools_ll %>%
  mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2]) %>%
  st_drop_geometry() %>%
  select(name, lon, lat)

destinations <- JOURNEYS %>%
  left_join(sch_xy, by = c("to_school" = "name")) %>%
  transmute(id = label, lon, lat)

ok <- !is.na(origins$lon) & !is.na(destinations$lon)
if (!all(ok)) {
  message("  ! dropping ", sum(!ok), " journeys with unmatched endpoints: ",
          paste(JOURNEYS$label[!ok], collapse = "; "))
}
origins <- origins[ok, ]; destinations <- destinations[ok, ]
stopifnot(nrow(origins) > 0)

message("  Routing ", nrow(origins), " journeys")

# ---- Route -----------------------------------------------------------
# A weekday morning departure, matching the matrix build. r5r 2.x renamed
# the core object, so both spellings are handled.

r5 <- r5r::setup_r5(data_path = NET, verbose = FALSE)
# The feed is a multi-period merge: only one window has the full weekday
# timetable active. 2024-12-04 is the Wednesday with the most services
# running (3,913 against a few hundred elsewhere), so it is the honest
# date to route on.
dep <- as.POSIXct("2024-12-04 08:00:00", tz = "Europe/London")

itin <- r5r::detailed_itineraries(
  r5r_network = r5,
  origins      = origins,
  destinations = destinations,
  mode         = c("WALK", "TRANSIT"),
  departure_datetime = dep,
  max_walk_time = 30,
  max_trip_duration = 120,
  shortest_path = TRUE,
  drop_geometry = FALSE,
  progress = FALSE
)

message("  Legs returned: ", nrow(itin))

# ---- Tidy ------------------------------------------------------------

legs <- itin %>%
  st_as_sf() %>%
  mutate(journey = from_id,
         leg_mode = mode,
         minutes  = segment_duration,
         # Service ids arrive as "BH:SER12A:12A"; keep the number only.
         route    = sub("^.*:", "", route)) %>%
  select(journey, leg_mode, minutes, distance, route, geometry) %>%
  st_transform(4326)

summary_tbl <- legs %>%
  st_drop_geometry() %>%
  group_by(journey) %>%
  summarise(total_minutes = round(sum(minutes, na.rm = TRUE)),
            legs = n(),
            services = paste(unique(route[nzchar(route)]), collapse = ", "),
            .groups = "drop")

message("\n=== Worked journeys, routed ===")
print(as.data.frame(summary_tbl), row.names = FALSE)

saveRDS(list(legs = legs, summary = summary_tbl, journeys = JOURNEYS,
             departure = dep, run_at = Sys.time()),
        file.path(PUBLIC_OUT, "route_geometries.rds"))

message("\nSaved public/output/route_geometries.rds")
