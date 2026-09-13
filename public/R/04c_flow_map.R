# ======================================================================
# public/R/04c_flow_map.R — modelled flows drawn on the network
# ----------------------------------------------------------------------
# The tables in 04b say how many children each school draws. They do not
# say where from, and for Longhill that is the whole question: a school
# at the eastern edge of the city either recruits from its own corner or
# it does not recruit.
#
# This routes every zone-to-school pair that carries modelled flow, with
# r5r, over the same network the journey times came from, then sums the
# flow onto shared segments with stplanr::overline(). Corridors used by
# many neighbourhoods come out thick and thin out towards the edges,
# which is what makes the picture readable: it is the modelled flow, not
# a set of straight desire lines pretending to be journeys.
#
# The legs are real. A walk leg follows the street network and a bus leg
# follows the service's own shape, so the trunk running west out of
# Ovingdean is the road and the buses that are actually on it.
#
# WHAT THIS IS NOT: these are MODELLED flows, from an open-data model
# with no admissions records behind it. Nobody's journey is drawn here.
# The map shows where the model sends children, which is exactly the
# thing the rest of section 7 is testing against published offers.
#
# Needs the r5r network in public/output/travel/r5_network and a JVM. If
# either is missing it exits cleanly and the document falls back to a
# choropleth of the same flows, so the build never depends on it.
#
# Inputs : public/output/model_terms.rds, open_inputs.rds
# Output : public/output/flow_map.rds
# ======================================================================

source(here::here("public", "R", "00_open_core.R"))

suppressPackageStartupMessages({ library(tidyverse); library(sf) })

message("\n=== Modelled flows on the network ===")

NET <- file.path(PUBLIC_OUT, "travel", "r5_network")
OUT <- file.path(PUBLIC_OUT, "flow_map.rds")

# rJava reads JAVA_HOME from the registry and on this machine that
# sometimes comes back empty even though a JVM is installed. Rather than
# fail, look in the usual places first. Anything found here is only used
# if JAVA_HOME is not already set.
if (!nzchar(Sys.getenv("JAVA_HOME"))) {
  cand <- c(
    Sys.glob("C:/Program Files/Java/*"),
    Sys.glob("C:/Program Files/Eclipse Adoptium/*"),
    Sys.glob(file.path(Sys.getenv("LOCALAPPDATA"), "Programs/*/jbr")),
    Sys.glob("/usr/lib/jvm/*"))
  cand <- cand[file.exists(file.path(cand, "bin",
                                     if (.Platform$OS.type == "windows")
                                       "java.exe" else "java"))]
  if (length(cand)) {
    Sys.setenv(JAVA_HOME = cand[1])
    message("  JAVA_HOME was unset; using ", cand[1])
  }
}

ok <- file.exists(file.path(NET, "network.dat")) &&
  requireNamespace("r5r", quietly = TRUE) &&
  requireNamespace("stplanr", quietly = TRUE) &&
  nzchar(Sys.getenv("JAVA_HOME"))

if (!ok) {
  message("  ! r5r network, stplanr or a JVM unavailable; skipping.")
  message("    Section 7.2's flow map falls back to a choropleth.")
  quit(save = "no", status = 0)
}

options(java.parameters = "-Xmx6G")

inp <- readRDS(file.path(PUBLIC_OUT, "open_inputs.rds"))
mt  <- readRDS(file.path(PUBLIC_OUT, "model_terms.rds"))

# Every school is drawn under the full model, which is the one section
# 7.4 lands on. Longhill is also drawn under the two earlier rungs,
# because the question there is how much of the city the model thinks it
# reaches, and that answer changes a great deal between them: Brightopia
# gives it most of Brighton, the full model gives it Ovingdean and
# Woodingdean. Only Longhill gets the extra models - doing it for all
# ten would treble the file for a comparison nine of them do not need.
MODEL   <- mt$best %||% "M4"
LH      <- "Longhill High School"
LH_ALSO <- c("M0", "M1")

flows <- bind_rows(
  mt$od_flows %>% filter(model == MODEL),
  mt$od_flows %>% filter(model %in% LH_ALSO, name == LH))

message("  Full model: ", MODEL, "; ", format(nrow(flows), big.mark = ","),
        " zone-school-model rows carrying flow")

# ---- Coordinates -----------------------------------------------------

zones_city <- inp$zones %>% filter(area != "Expansion area")

z_pts <- zones_city %>%
  st_as_sf(coords = c("zone_e", "zone_n"), crs = 27700) %>%
  st_transform(4326)
z_xy <- st_coordinates(z_pts)
ORIGINS <- tibble(id = z_pts$zone, lon = z_xy[, 1], lat = z_xy[, 2])

s_pts <- inp$schools %>%
  filter(name %in% unique(flows$name)) %>%
  st_as_sf(coords = c("easting", "northing"), crs = 27700) %>%
  st_transform(4326)
s_xy <- st_coordinates(s_pts)
DESTS <- tibble(id = s_pts$name, lon = s_xy[, 1], lat = s_xy[, 2])

# Only pairs worth drawing. Below a fifth of a child the line would be
# invisible and the routing is wasted.
MIN_FLOW <- 0.2
pairs <- flows %>% filter(flow >= MIN_FLOW)
message(sprintf("  Routing %s pairs (flow >= %.1f) across %d schools",
                format(nrow(pairs), big.mark = ","), MIN_FLOW,
                n_distinct(pairs$name)))

# ---- Route -----------------------------------------------------------
# The same weekday morning as 00d: the feed is a multi-period merge and
# 2024-12-04 is the Wednesday with the full timetable running. Routing
# on any other date silently returns almost nothing.

r5  <- r5r::setup_r5(data_path = NET, verbose = FALSE)
dep <- as.POSIXct("2024-12-04 08:00:00", tz = "Europe/London")

# The geometry of a journey does not depend on which model sent the
# children along it, so each zone-school pair is routed once and the
# flows from every model are joined onto it afterwards.
route_school <- function(sch) {
  p <- pairs %>% filter(name == sch) %>% distinct(zone)
  o <- ORIGINS %>% filter(id %in% p$zone)
  d <- DESTS   %>% filter(id == sch)
  itin <- try(r5r::detailed_itineraries(
    r5r_network = r5, origins = o,
    destinations = d[rep(1, nrow(o)), ],
    mode = c("WALK", "TRANSIT"), departure_datetime = dep,
    max_walk_time = 30, max_trip_duration = 120,
    shortest_path = TRUE, drop_geometry = FALSE, progress = FALSE),
    silent = TRUE)
  if (inherits(itin, "try-error") || !nrow(itin)) return(NULL)
  itin %>%
    st_as_sf() %>%
    transmute(zone = from_id, name = sch,
              leg_mode = if_else(mode == "WALK", "Walk", "Bus"),
              route = sub("^.*:", "", route),
              geometry)
}

geom <- purrr::map(sort(unique(pairs$name)), function(s) {
  x <- route_school(s)
  message(sprintf("    %-38s %s legs", s,
                  if (is.null(x)) "no" else format(nrow(x), big.mark = ",")))
  x
})
r5r::stop_r5(r5)
geom <- bind_rows(geom)

# A leg carries the whole journey's flow: 12 children travelling from
# this neighbourhood use every leg of the route, not a share of it.
legs <- geom %>%
  inner_join(pairs %>% select(model, zone, name, flow),
             by = c("zone", "name"), relationship = "many-to-many")

stopifnot(nrow(legs) > 0)

# ---- Sum onto shared segments ---------------------------------------
# overline() splits the lines where they meet and adds the flow on each
# resulting segment, which is what turns a bundle of separate routes
# into a network with thick trunks. Walk and bus are kept apart: they
# are different things and drawing a shared corridor as one number would
# merge a footpath with a bus lane.

message("\n  Aggregating onto shared segments...")

overline_one <- function(d) {
  d <- st_transform(d, 27700)
  out <- try(stplanr::overline(d, attrib = "flow", ncores = 1,
                               quiet = TRUE), silent = TRUE)
  if (inherits(out, "try-error")) return(NULL)
  st_transform(out, 4326)
}

combos <- legs %>% st_drop_geometry() %>% distinct(model, name, leg_mode)

net <- purrr::pmap_dfr(combos, function(model, name, leg_mode) {
  d <- legs %>% filter(model == !!model, name == !!name,
                       leg_mode == !!leg_mode)
  if (!nrow(d)) return(NULL)
  o <- overline_one(d)
  if (is.null(o)) return(NULL)
  o %>% mutate(model = !!model, name = !!name, leg_mode = !!leg_mode)
})

# Segments carrying a fraction of a child add nothing but file size.
MIN_SEG <- 0.5
net <- net %>% filter(flow >= MIN_SEG) %>% st_sf()

# Bus route shapes carry far more vertices than a web map can show. A
# 10 m tolerance is well below what is visible at any zoom the map
# offers and cuts the payload by about two thirds.
net <- net %>%
  st_transform(27700) %>%
  st_simplify(dTolerance = 10) %>%
  st_transform(4326) %>%
  filter(!st_is_empty(.))

message(sprintf("  %s segments kept (flow >= %.1f), %.1f MB",
                format(nrow(net), big.mark = ","), MIN_SEG,
                as.numeric(object.size(net)) / 1e6))

# ---- Where each school's intake comes from --------------------------

catch_of <- zones_city %>% select(zone, catchment)

by_zone <- flows %>%
  left_join(catch_of, by = "zone") %>%
  group_by(model, name) %>%
  mutate(share = flow / sum(flow)) %>%
  ungroup()

by_catch <- by_zone %>%
  group_by(model, name, catchment) %>%
  summarise(flow = sum(flow), .groups = "drop_last") %>%
  mutate(share = flow / sum(flow)) %>%
  ungroup()

message(sprintf("\n=== Where the model draws %s's intake from ===", LH))
print(as.data.frame(by_catch %>% filter(name == LH) %>%
  select(model, catchment, flow) %>%
  mutate(flow = round(flow)) %>%
  pivot_wider(names_from = model, values_from = flow, values_fill = 0) %>%
  arrange(desc(.data[[MODEL]]))), row.names = FALSE)

own <- by_catch %>% filter(name == LH, catchment == "Longhill")
for (m in unique(own$model))
  message(sprintf("  %s: %.0f of %.0f children from its own catchment (%.0f%%)",
                  m, own$flow[own$model == m],
                  sum(by_catch$flow[by_catch$name == LH & by_catch$model == m]),
                  100 * own$share[own$model == m]))

saveRDS(list(net = net, legs_n = nrow(legs), by_zone = by_zone,
             by_catch = by_catch, model = MODEL, departure = dep,
             min_flow = MIN_FLOW, min_seg = MIN_SEG,
             run_at = Sys.time()), OUT)

message("\nSaved public/output/flow_map.rds")
