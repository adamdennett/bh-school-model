# public/R/00c_network_maps.R — the network the travel times come from
# ======================================================================
# Every journey time in this project is produced by r5r routing over an
# OpenStreetMap street network and a GTFS bus timetable. Those are the
# two inputs that do most of the work, and neither has been shown.
#
# This builds the descriptive material: what the bus network actually
# covers, how journey time varies across the city to each site, and a
# handful of worked journeys between real places so the numbers can be
# sanity-checked against local knowledge.
#
# It reads the GTFS feed and the routed matrix. It does NOT re-run r5r,
# so it needs no Java and takes seconds rather than an hour.
#
# Output: public/output/network_maps.rds
# ======================================================================

source(here::here("public", "R", "00_open_core.R"))

suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
})

message("\n=== The routing network ===")

TRAVEL_DIR <- file.path(PUBLIC_OUT, "travel")
GTFS  <- file.path(TRAVEL_DIR, "r5_network", "gtfs.zip")
META  <- file.path(TRAVEL_DIR, "build_metadata.rds")

meta <- if (file.exists(META)) readRDS(META) else NULL
if (!is.null(meta)) {
  message("  Network built ", format(meta$built_at, "%d %B %Y"),
          " from ", meta$pbf, " and ", meta$gtfs)
  message("  Origins routed: ", format(meta$n_origins, big.mark = ","),
          " postcodes across ", paste(meta$outcodes, collapse = ", "))
}

# ---- The bus network, from the GTFS feed ----------------------------

read_gtfs_table <- function(zip, name) {
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp), add = TRUE)
  files <- utils::unzip(zip, list = TRUE)$Name
  hit <- files[basename(files) == name]
  if (!length(hit)) return(NULL)
  con <- unz(zip, hit[1])
  out <- try(readr::read_csv(con, show_col_types = FALSE, progress = FALSE),
             silent = TRUE)
  if (inherits(out, "try-error")) NULL else out
}

stops  <- read_gtfs_table(GTFS, "stops.txt")
routes <- read_gtfs_table(GTFS, "routes.txt")
shapes <- read_gtfs_table(GTFS, "shapes.txt")
trips  <- read_gtfs_table(GTFS, "trips.txt")

message("\n  GTFS feed: ", nrow(stops), " stops, ", nrow(routes), " routes")

stops_sf <- stops %>%
  filter(!is.na(stop_lat), !is.na(stop_lon)) %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)

# Route geometries. Each shape is a polyline; one per shape_id is plenty
# for a map, and joining to routes gives the service number.
route_lines <- NULL
if (!is.null(shapes) && !is.null(trips)) {
  shape_route <- trips %>%
    distinct(shape_id, route_id) %>%
    left_join(routes %>% select(route_id, route_short_name), by = "route_id") %>%
    distinct(shape_id, .keep_all = TRUE)

  route_lines <- shapes %>%
    filter(!is.na(shape_pt_lat), !is.na(shape_pt_lon)) %>%
    arrange(shape_id, shape_pt_sequence) %>%
    group_by(shape_id) %>%
    filter(n() > 1) %>%
    summarise(geometry = st_sfc(st_linestring(
      cbind(shape_pt_lon, shape_pt_lat)), crs = 4326), .groups = "drop") %>%
    st_as_sf() %>%
    left_join(shape_route, by = "shape_id")

  message("  Route shapes: ", nrow(route_lines))
}

# Which services actually reach the eastern towns? This is the thing the
# rebuild was for, so it is worth showing explicitly.
east_stops <- stops_sf %>%
  filter(grepl("Peacehaven|Telscombe|Saltdean|Newhaven|Seaford|Rottingdean|Woodingdean",
               stop_name, ignore.case = TRUE))

message("  Stops in the eastern corridor: ", nrow(east_stops))


# ---- Journey time surfaces, from the routed matrix -------------------

inp <- readRDS(file.path(PUBLIC_OUT, "open_inputs.rds"))
LH <- "Longhill High School"

surface <- inp$zones %>%
  select(zone, lsoa, zone_e, zone_n, Oi, area) %>%
  left_join(inp$costs_now %>% filter(name == LH) %>%
              select(zone, ovingdean = cij), by = "zone") %>%
  left_join(inp$costs_elm %>% filter(name == LH) %>%
              select(zone, elm_grove = cij), by = "zone") %>%
  left_join(inp$costs_now %>% filter(name == "Dorothy Stringer School") %>%
              select(zone, stringer = cij), by = "zone") %>%
  mutate(saving = ovingdean - elm_grove) %>%
  st_as_sf(coords = c("zone_e", "zone_n"), crs = 27700, remove = FALSE) %>%
  st_transform(4326)

message(sprintf("\n  Journey to Longhill at Ovingdean: median %.0f min, worst %.0f",
                median(surface$ovingdean, na.rm = TRUE),
                max(surface$ovingdean, na.rm = TRUE)))
message(sprintf("  Journey to the Elm Grove site:     median %.0f min, worst %.0f",
                median(surface$elm_grove, na.rm = TRUE),
                max(surface$elm_grove, na.rm = TRUE)))
message(sprintf("  Relocation saves a median of %.0f minutes; it costs time in %.0f%% of zones.",
                median(surface$saving, na.rm = TRUE),
                100 * mean(surface$saving < 0, na.rm = TRUE)))


# ---- Worked journeys between real places ----------------------------
# Chosen to be checkable against local knowledge rather than to flatter
# the model: each is a journey a Brighton family would recognise.

places <- tribble(
  ~place,                    ~pcd,
  "Whitehawk (Whitehawk Rd)", "BN2 5FL",
  "Woodingdean (centre)",     "BN2 6PA",
  "Kemptown (St George's Rd)","BN2 1ED",
  "Rottingdean (High St)",    "BN2 7HE",
  "Saltdean (Longridge Ave)", "BN2 8LF",
  "Hove (Church Rd)",         "BN3 2FL",
  "Peacehaven (Roderick Ave)","BN10 8AY"
)

travel_raw <- read_open_csv(OPEN$travel) %>%
  mutate(postcode = normalise_pcd(id))

journeys <- places %>%
  mutate(postcode = normalise_pcd(pcd)) %>%
  left_join(travel_raw, by = "postcode") %>%
  select(place, pcd, any_of(c("time_longhill", "time_elm_grove", "time_ds",
                              "time_varndean", "time_cardinal_n",
                              "time_priory_lewes", "time_peacehaven"))) %>%
  rename_with(~ sub("^time_", "", .x))

message("\n=== Worked journeys (walk and bus, minutes) ===")
print(as.data.frame(journeys %>%
  mutate(across(where(is.numeric), ~ round(.x)))), row.names = FALSE)

found <- sum(!is.na(journeys$longhill))
message(sprintf("\n  %d of %d sample postcodes matched the routed matrix.",
                found, nrow(journeys)))


# ---- Save -----------------------------------------------------------

saveRDS(list(
  meta        = meta,
  stops       = stops_sf,
  routes      = route_lines,
  east_stops  = east_stops,
  n_stops     = nrow(stops_sf),
  n_routes    = nrow(routes),
  surface     = surface,
  journeys    = journeys,
  gtfs_coverage = if (!is.null(meta)) meta$gtfs_coverage else NULL,
  run_at      = Sys.time()
), file.path(PUBLIC_OUT, "network_maps.rds"))

message("\nSaved public/output/network_maps.rds")
