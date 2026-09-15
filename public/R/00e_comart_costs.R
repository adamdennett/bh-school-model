# ======================================================================
# public/R/00e_comart_costs.R — routed journeys to CoMArt's site
# ----------------------------------------------------------------------
# CoMArt, in East Brighton, closed in 2005; its closure is why the city
# moved to catchments with a lottery tie-break. The strategic view and
# the simulator ask what the system would look like with it open again,
# as a small school sharing Longhill's catchment. That needs what every
# other school has: routed walk and bus time from every postcode.
#
# This routes the one extra destination on the network 00b built, with
# the same settings - walk and transit, a representative school-day
# departure at 08:00, the median over the window - and the same way of
# picking the date. Brighton Aldridge is routed alongside it and compared
# with its column in the existing matrix, so a drift in network, date or
# settings shows up as a number rather than as a quietly different model.
#
# Aggregated to zones exactly as 01_open_inputs does: the mean over each
# zone's postcodes, capped at the time it would take to walk.
#
# Needs the r5r network in public/output/travel/r5_network and a JVM.
# Output: public/output/comart_costs.rds
# ======================================================================

source(here::here("public", "R", "00_open_core.R"))

suppressPackageStartupMessages({ library(tidyverse) })

message("\n=== CoMArt: routed journeys to the site of the school closed in 2005 ===")

NET <- file.path(PUBLIC_OUT, "travel", "r5_network")
OUT <- file.path(PUBLIC_OUT, "comart_costs.rds")

# The site, as marked on the strategic view's orientation map.
COMART_SITE <- tibble(id = "comart", name = "CoMArt", lon = -0.099449, lat = 50.823301)
xy <- sf::st_coordinates(sf::st_transform(
  sf::st_as_sf(COMART_SITE, coords = c("lon", "lat"), crs = 4326), 27700))
COMART_SITE$easting <- xy[, 1]; COMART_SITE$northing <- xy[, 2]

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
options(java.parameters = paste0("-Xmx", Sys.getenv("R5R_HEAP", "8G")))
stopifnot(file.exists(file.path(NET, "network.dat")),
          requireNamespace("r5r", quietly = TRUE))

meta <- read_open_rds(file.path(PUBLIC_OUT, "travel", "build_metadata.rds"))
PARAMS <- meta$params

# ---- The departure date, chosen as 00b chooses it ----------------------
service_counts <- function(gtfs_path) {
  td <- tempfile(); dir.create(td)
  fl <- utils::unzip(gtfs_path, list = TRUE)$Name
  utils::unzip(gtfs_path,
               files = intersect(c("calendar.txt", "calendar_dates.txt", "trips.txt"), fl),
               exdir = td)
  cal <- if ("calendar.txt" %in% fl)
    suppressWarnings(readr::read_csv(file.path(td, "calendar.txt"), show_col_types = FALSE))
  cd <- if ("calendar_dates.txt" %in% fl)
    suppressWarnings(readr::read_csv(file.path(td, "calendar_dates.txt"), show_col_types = FALSE))
  trips <- suppressWarnings(readr::read_csv(file.path(td, "trips.txt"), show_col_types = FALSE))
  ymd <- function(x) as.Date(as.character(x), format = "%Y%m%d")
  lo <- if (!is.null(cal)) min(ymd(cal$start_date)) else min(ymd(cd$date))
  hi <- if (!is.null(cal)) max(ymd(cal$end_date))   else max(ymd(cd$date))
  cand <- seq(lo, hi, by = "day")
  cand <- cand[format(cand, "%u") %in% c("2", "3", "4")]
  wdcol <- c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")
  n_on <- vapply(cand, function(d) {
    base <- character(0)
    if (!is.null(cal)) {
      wd <- wdcol[as.integer(format(d, "%u"))]
      if (wd %in% names(cal))
        base <- cal$service_id[cal[[wd]] == 1 &
                                 ymd(cal$start_date) <= d & ymd(cal$end_date) >= d]
    }
    if (!is.null(cd)) {
      dd <- cd[ymd(cd$date) == d, ]
      base <- setdiff(base, dd$service_id[dd$exception_type == 2])
      base <- union(base,  dd$service_id[dd$exception_type == 1])
    }
    sum(trips$service_id %in% base)
  }, integer(1))
  tibble(date = cand, n_services = n_on)
}
DEPARTURE_DATE <- Sys.getenv("DEPARTURE_DATE", "")
sc <- service_counts(file.path(NET, "gtfs.zip"))
dep_date <- if (nzchar(DEPARTURE_DATE)) as.Date(DEPARTURE_DATE) else
  sc %>% slice_max(n_services, n = 1, with_ties = FALSE) %>% pull(date)
message(sprintf("  Departure %s %s", dep_date, PARAMS$default_time))

# ---- Route -------------------------------------------------------------------
exports <- getNamespaceExports("r5r")
builder <- if ("build_network" %in% exports) r5r::build_network else r5r::setup_r5
b_args <- list(data_path = NET)
if ("verbose" %in% names(formals(builder))) b_args$verbose <- FALSE
net <- do.call(builder, b_args)
stopifnot(!is.null(net))
NET_ARG <- if ("r5r_network" %in% names(formals(r5r::travel_time_matrix))) "r5r_network" else "r5r_core"

travel <- read_open_csv(OPEN$travel)
origins <- travel %>%
  transmute(id = normalise_pcd(id), lon = long, lat = lat) %>%
  filter(is.finite(lon), is.finite(lat)) %>%
  distinct(id, .keep_all = TRUE)

# Brighton Aldridge's site, from the open school table, as the check.
baca_xy <- SCHOOLS_OPEN %>% filter(short == "baca") %>%
  sf::st_as_sf(coords = c("easting", "northing"), crs = 27700) %>% sf::st_transform(4326) %>%
  sf::st_coordinates()
dests <- bind_rows(COMART_SITE %>% select(id, lon, lat),
                   tibble(id = "baca", lon = baca_xy[, 1], lat = baca_xy[, 2]))

route_to <- function(dest) {
  dep <- as.POSIXct(paste(dep_date, PARAMS$default_time), tz = "GMT")
  args <- list(origins = origins, destinations = dest, mode = PARAMS$mode,
               departure_datetime = dep, max_walk_time = PARAMS$max_walk_time,
               max_trip_duration = PARAMS$max_trip_duration,
               time_window = PARAMS$time_window, percentiles = PARAMS$percentiles,
               progress = FALSE)
  args[[NET_ARG]] <- net
  ttm <- do.call(r5r::travel_time_matrix, args)
  tcol <- grep("^travel_time", names(ttm), value = TRUE)[1]
  tibble(postcode = ttm$from_id, t = ttm[[tcol]])
}
t_comart <- route_to(dests[1, ])
t_baca   <- route_to(dests[2, ])
try(r5r::stop_r5(net), silent = TRUE)
message(sprintf("  routed %s postcodes to CoMArt's site", format(nrow(t_comart), big.mark = ",")))

# ---- The check: Brighton Aldridge against the existing matrix ----------
chk <- t_baca %>%
  inner_join(travel %>% transmute(postcode = normalise_pcd(id), existing = time_baca), by = "postcode") %>%
  filter(is.finite(t), is.finite(existing))
check <- list(n = nrow(chk), median_abs_diff = median(abs(chk$t - chk$existing)),
              share_within_2min = mean(abs(chk$t - chk$existing) <= 2),
              correlation = cor(chk$t, chk$existing))
message(sprintf("  check, Brighton Aldridge: %d postcodes, median |difference| %.1f min, %.0f%% within 2 min, r = %.3f",
                check$n, check$median_abs_diff, 100 * check$share_within_2min, check$correlation))
if (check$median_abs_diff > 2)
  warning("CoMArt was routed on a network or date that does not reproduce the existing matrix", call. = FALSE)

# ---- Zones, as 01_open_inputs does it ------------------------------------------
inp <- read_open_rds(file.path(PUBLIC_OUT, "open_inputs.rds"))
WALK_KMH <- 1.030 / (14.22 / 60); WALK_CIRCUITY <- 1.3
zones <- inp$zones %>% filter(area != "Expansion area")
costs <- zones %>% select(zone, zone_e, zone_n) %>%
  left_join(inp$pcd_regime %>% select(postcode, zone) %>%
              inner_join(t_comart, by = "postcode") %>%
              group_by(zone) %>% summarise(routed_min = mean(t, na.rm = TRUE), .groups = "drop"),
            by = "zone") %>%
  mutate(km = pmax(euclid_m(zone_e, zone_n, COMART_SITE$easting, COMART_SITE$northing) / 1000, 0.1),
         walk_min = km * WALK_CIRCUITY / WALK_KMH * 60,
         routed = is.finite(routed_min),
         cij = pmin(if_else(routed, routed_min, walk_min), walk_min)) %>%
  select(zone, cij, km, routed)
stopifnot(nrow(costs) == nrow(zones), all(is.finite(costs$cij)))
message(sprintf("  %d zones, %.0f%% routed; median journey %.0f min, %.0f%% within 30 min",
                nrow(costs), 100 * mean(costs$routed), median(costs$cij), 100 * mean(costs$cij <= 30)))

saveRDS(list(site = COMART_SITE, costs = costs, check = check, departure = dep_date,
             params = PARAMS, built_at = Sys.time()), OUT)
message("Saved public/output/comart_costs.rds")
