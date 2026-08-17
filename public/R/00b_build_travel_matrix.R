# 00b_build_travel_matrix.R — Rebuild the travel-time matrix, extended east
# =========================================================================
# Produces a postcode-to-school walk/bus travel time matrix covering the
# expanded authority — Brighton & Hove plus Peacehaven, Telscombe and East
# Saltdean — and the East Sussex schools those families realistically
# choose between.
#
# It replaces the distance-based approximation used east of Saltdean in
# the current models, which is their single biggest methodological weakness.
#
# ------------------------------------------------------------------------
# WHY THIS IS SIMPLER THAN EXPECTED: UK2GTFS IS NOT NEEDED
# ------------------------------------------------------------------------
# The existing GTFS feed already reaches Seaford. Checking the feeds in
# BH_Schools_2/data and BH_Schools_Consultation/data:
#
#   stops within 2 km of Peacehaven ......... 55
#   stops within 2 km of Newhaven ........... 35
#   stops within 2 km of Seaford ............ 40
#   routes 12 / 12A / 12X (the coastal corridor) ... present
#   easternmost stop ........................ lon 0.31 (past Eastbourne)
#
# The bus data was never the constraint. The constraint is the ROAD
# NETWORK: BrighonSchoolsSIM.qmd clips OSM to Brighton's output areas plus
# a 500 m buffer, so r5r has no street network east of the city boundary
# and cannot route to or from Peacehaven even though the stops exist.
#
# So the fix is to stop clipping. This script builds over the whole of
# East and West Sussex. If you still want UK2GTFS — for a multi-operator
# feed from BODS rather than the Brighton & Hove Buses one — set
# USE_UK2GTFS = TRUE and follow the notes in section 3.
#
# ------------------------------------------------------------------------
# REQUIREMENTS
# ------------------------------------------------------------------------
#   * Java 21 (r5r 2.x). Check with rJava::.jinit(); rJava::.jcall(
#       "java.lang.System", "S", "getProperty", "java.version")
#   * install.packages(c("r5r", "osmextract"))
#   * ~8 GB of heap. Set BEFORE rJava loads — see JAVA_HEAP below.
#
# Output: public/output/travel/bn_pcds_sch_travel_extended.csv
#         public/output/travel/build_metadata.rds
# =========================================================================

# ---------------------------------------------------------------------
# THE HEAP OPTION MUST COME FIRST
# ---------------------------------------------------------------------
# options(java.parameters) is only read when the JVM starts, and the JVM
# starts the moment anything loads rJava — which library(r5r) does. Set it
# before any library() call or r5r will silently run on the default heap
# (usually 512 MB) and fail on a network this size.
JAVA_HEAP <- Sys.getenv("R5R_HEAP", "8G")
options(java.parameters = paste0("-Xmx", JAVA_HEAP))

# Java 21 is required by r5r 2.x. rJavaEnv can install it if needed:
#   install.packages("rJavaEnv")
#   rJavaEnv::java_check_version_rjava()
#   rJavaEnv::java_quick_install(version = 21)
if (requireNamespace("rJavaEnv", quietly = TRUE)) {
  try(rJavaEnv::java_check_version_rjava(quiet = TRUE), silent = TRUE)
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
  library(here)
  library(r5r)
})

source(here::here("public", "R", "00_open_core.R"))

TRAVEL_DIR <- file.path(PUBLIC_OUT, "travel")
R5_DIR     <- file.path(TRAVEL_DIR, "r5_network")
dir.create(R5_DIR, showWarnings = FALSE, recursive = TRUE)

# ---- Options --------------------------------------------------------
USE_UK2GTFS <- FALSE          # see section 3
REBUILD_OSM <- TRUE           # FALSE to reuse an existing download

# Two feeds sit in the sibling projects under the same filename and they
# are NOT the same. Checked calendars:
#
#   BH_Schools_Consultation/data/gtfs.zip  3.9 MB  2024-05-07 to 2024-05-24
#                                                  3,564 services/weekday
#   BH_Schools_2/data/gtfs.zip             4.8 MB  2023-10-29 to 2025-11-07
#                                                  median 179/weekday,
#                                                  peaking 3,908 in Dec 2024
#
# The second is the fuller feed and is the default. The first is the
# May-2024 snapshot used by BrighonSchoolsSIM.qmd, valid for a fortnight.
GTFS_SOURCE <- Sys.getenv("OPEN_GTFS", "E:/BH_Schools_2/data/gtfs.zip")

# Routing parameters, from BH_Schools_Consultation/travel.qmd: walk+transit,
# 60 minute walk limit, 120 minute trip limit. Longhill departs 07:30
# because its buses run earlier; every other school is 08:00. That
# asymmetry is deliberate and is preserved.
#
# THE DATE CANNOT MATCH THE PUBLISHED MATRIX. That was built on a 2022 feed
# which is not in this repository and is no longer published, so exact
# reproduction is impossible. DEPARTURE_DATE is chosen automatically as the
# date with the most services running in whichever feed is supplied; set it
# explicitly to override.
DEPARTURE_DATE <- Sys.getenv("R5R_DATE", "")   # "" = pick automatically

R5_PARAMS <- list(
  mode              = c("WALK", "TRANSIT"),
  max_walk_time     = 60,
  max_trip_duration = 120,
  time_window       = 1,
  percentiles       = 50,
  default_time      = "08:00:00",
  longhill_time     = "07:30:00"
)


# ====================================================================
# 1. Destinations
# ====================================================================

message("\n=== 1. Destinations ===")

# The eleven schools in the expanded authority, plus the East Sussex
# schools that Peacehaven families actually choose between. Leaving those
# out would leave the model nowhere to send eastern children except
# Brighton, which mechanically overstates the inflow.
DESTINATIONS <- tibble::tribble(
  ~short,          ~name,                                   ~urn,   ~easting, ~northing, ~in_scope,
  "varndean",      "Varndean School",                        114579,   531232,    107295, "city",
  "ds",            "Dorothy Stringer School",                114580,   530794,    107134, "city",
  "longhill",      "Longhill High School",                   114581,   536239,    103942, "city",
  "blatchington",  "Blatchington Mill School",               114606,   528137,    106649, "city",
  "hove_park",     "Hove Park School",                       114607,   528269,    106172, "city",
  "patcham",       "Patcham High School",                    114608,   530717,    108617, "city",
  "cardinal_n",    "Cardinal Newman Catholic School",        114611,   529812,    105778, "city",
  "baca",          "Brighton Aldridge Community Academy",    136164,   534234,    108203, "city",
  "paca",          "Portslade Aldridge Community Academy",   137063,   524992,    107180, "city",
  "kings",         "King's School",                          139409,   527377,    107625, "city",
  "peacehaven",    "Peacehaven Community School",            144661,   541262,    101530, "joining_2028",
  "seahaven",      "Seahaven Academy",                       140679,   543952,    100605, "east_sussex",
  "priory_lewes",  "Priory School",                          114598,   541971,    109666, "east_sussex",
  "seaford_head",  "Seaford Head School",                    138473,   549440,     98936, "east_sussex",
  # Hypothetical relocation site — no URN
  "elm_grove",     "Longhill (Elm Grove site)",              NA,       533118,    105231, "hypothetical"
)

message("  ", nrow(DESTINATIONS), " destinations (",
        sum(DESTINATIONS$in_scope == "east_sussex"), " East Sussex, 1 hypothetical)")


# ====================================================================
# 2. Origins — every live postcode in the study area
# ====================================================================

message("\n=== 2. Origins ===")

# Postcode districts to cover. BN1/2/3/41 are the current matrix; BN10 is
# the gap that motivates this rebuild; BN9/BN25/BN7 give eastern families
# somewhere to go that is not Brighton.
OUTCODES <- c("BN1", "BN2", "BN3", "BN41", "BN42", "BN45",
              "BN10",              # Peacehaven, Telscombe Cliffs
              "BN9",               # Newhaven
              "BN25",              # Seaford
              "BN7")               # Lewes

# The published matrix has 10,478 rows across BN1/2/3/4/6/41/42/45/50/51/
# 52/88. Most of the excess over the count below is TERMINATED postcodes
# and large-user codes (BN50, BN51, BN52, BN88 are PO-box style, not
# residential). Filtering to live geographic postcodes gives about 7,900
# origins across a wider area — fewer rows but every one of them somewhere
# a child could actually live.
#
# Set LIVE_ONLY = FALSE to reproduce the old row set for a like-for-like
# comparison against the published file.
LIVE_ONLY <- TRUE

onspd <- read_open_csv(
  OPEN$onspd,
  col_types = readr::cols_only(pcds = "c", doterm = "c", oslaua = "c",
                               osward = "c", lsoa21 = "c",
                               oseast1m = "d", osnrth1m = "d",
                               lat = "d", long = "d")
) %>%
  { if (LIVE_ONLY) filter(., is.na(doterm)) else . } %>%
  mutate(postcode = normalise_pcd(pcds),
         outcode  = sub(" .*$", "", postcode)) %>%
  filter(outcode %in% OUTCODES, !is.na(lat), !is.na(long))

message("  Postcodes (", if (LIVE_ONLY) "live only" else "live + terminated",
        "): ", format(nrow(onspd), big.mark = ","))
print(as.data.frame(onspd %>% count(outcode, name = "postcodes") %>%
                      arrange(desc(postcodes))), row.names = FALSE)

origins <- onspd %>%
  transmute(id = postcode, lon = long, lat = lat) %>%
  distinct(id, .keep_all = TRUE)


# ====================================================================
# 3. GTFS
# ====================================================================

message("\n=== 3. GTFS ===")

# ---------------------------------------------------------------------
# GETTING A FRESHER FEED
# ---------------------------------------------------------------------
# UK2GTFS converts TransXChange to GTFS; it does NOT download from the Bus
# Open Data Service. Its own documentation now advises against using it for
# this purpose:
#
#   "The Open Bus Data Service now offers a national GTFS download option
#    based on ITO World's TransXchange to GTFS converter" — and recommends
#   non-expert users download those files directly.
#
# So the shortest route to a current feed is to take GTFS straight from
# BODS rather than convert anything:
#
#   1. Go to the Bus Open Data Service downloads page
#      (https://data.bus-data.dft.gov.uk/downloads/) and take the GTFS
#      Schedule download — nationally or for the South East region.
#   2. Point GTFS_SOURCE at that file.
#
# Nothing else needs to change: the service-date picker below reads
# whatever calendar the feed carries and selects a date it can serve, so a
# newer feed simply drops in.
#
# UK2GTFS is still the right tool if you need a HISTORICAL feed — say, to
# reproduce the 2022 conditions behind the published matrix — since BODS
# only publishes current data. In that case:
#
#   remotes::install_github("ITSLeeds/UK2GTFS")
#   UK2GTFS::transxchange2gtfs(path_in = "<TransXChange zip>",
#                              ncores = 4, try_mode = TRUE)
#
# with TransXChange from Traveline (TNDS) for the date in question.

if (USE_UK2GTFS) {
  stop("USE_UK2GTFS is TRUE but no converted feed was supplied. ",
       "Convert your TransXChange archive, point GTFS_SOURCE at the result, ",
       "then set USE_UK2GTFS = FALSE.")
}

gtfs_path <- assert_open(GTFS_SOURCE)

# Confirm the feed really does reach the east before building anything
stops_tmp <- tempfile(); dir.create(stops_tmp)
utils::unzip(gtfs_path, files = "stops.txt", exdir = stops_tmp)
stops <- readr::read_csv(file.path(stops_tmp, "stops.txt"), show_col_types = FALSE) %>%
  filter(!is.na(stop_lat), !is.na(stop_lon))

near_km <- function(lon, lat, r = 2) {
  sum(sqrt(((stops$stop_lon - lon) * 71.5)^2 + ((stops$stop_lat - lat) * 111.2)^2) < r)
}
coverage <- tibble(
  place = c("Brighton", "Saltdean", "Peacehaven", "Newhaven", "Seaford"),
  lon   = c(-0.1372, -0.0400, -0.0060, 0.0560, 0.1010),
  lat   = c(50.8225, 50.8020, 50.7920, 50.7930, 50.7710)
) %>%
  mutate(stops_within_2km = purrr::map2_int(lon, lat, ~ near_km(.x, .y)))

message("  Feed: ", basename(gtfs_path), " — ", format(nrow(stops), big.mark = ","), " stops")
print(as.data.frame(coverage %>% select(place, stops_within_2km)), row.names = FALSE)

if (any(coverage$stops_within_2km[coverage$place %in% c("Peacehaven", "Newhaven")] < 5))
  warning("The GTFS feed has few stops in the expansion area. Check the feed ",
          "before trusting eastern travel times.")

file.copy(gtfs_path, file.path(R5_DIR, "gtfs.zip"), overwrite = TRUE)

# --- Pick a departure date the feed can actually serve ---------------
# r5r errors outright if no service runs on the chosen date. Feeds vary
# wildly in which dates they cover, so count active services per candidate
# weekday and take the best rather than guessing.

# Counts TRIPS, not service_ids. r5r's own availability check is about
# whether any trip runs, and a service_id can carry many trips or none, so
# counting ids can say "3,908 services" for a date with no service at all.
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
  trips <- suppressWarnings(readr::read_csv(file.path(td, "trips.txt"),
                                            show_col_types = FALSE))

  ymd <- function(x) as.Date(as.character(x), format = "%Y%m%d")
  lo <- if (!is.null(cal)) min(ymd(cal$start_date)) else min(ymd(cd$date))
  hi <- if (!is.null(cal)) max(ymd(cal$end_date))   else max(ymd(cd$date))

  # Tuesday to Thursday only: a representative school day, avoiding the
  # lighter Monday and Friday timetables.
  cand <- seq(lo, hi, by = "day")
  cand <- cand[format(cand, "%u") %in% c("2", "3", "4")]
  wdcol <- c("monday","tuesday","wednesday","thursday","friday","saturday","sunday")

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

sc <- service_counts(gtfs_path)

if (nzchar(DEPARTURE_DATE)) {
  dep_date <- as.Date(DEPARTURE_DATE)
  n_svc <- sc$n_services[sc$date == dep_date]
  if (length(n_svc) == 0 || n_svc == 0)
    stop("No services run on ", DEPARTURE_DATE, " in ", basename(gtfs_path),
         ". Feed covers ", min(sc$date), " to ", max(sc$date), ".")
} else {
  bestrow <- sc %>% slice_max(n_services, n = 1, with_ties = FALSE)
  dep_date <- bestrow$date
  n_svc <- bestrow$n_services
}

message(sprintf("  Departure date: %s (%s), %s services running",
                dep_date, weekdays(dep_date), format(n_svc, big.mark = ",")))
message("  Feed covers ", min(sc$date), " to ", max(sc$date),
        "; median ", median(sc$n_services), " services on a candidate weekday")

if (n_svc < 0.5 * max(sc$n_services))
  warning("The chosen date has well below the feed's peak service level. ",
          "Check it is not a school holiday.")


# ====================================================================
# 4. Road network — the piece that was actually missing
# ====================================================================

message("\n=== 4. Road network ===")
message("  The previous build clipped OSM to Brighton's output areas plus")
message("  500 m, which is why nothing east of the city could be routed.")
message("  This build covers East and West Sussex whole.")

# ---------------------------------------------------------------------
# r5r NEEDS EXACTLY ONE .pbf, AND THE STUDY AREA SPANS TWO
# ---------------------------------------------------------------------
# Geofabrik splits along the county boundary, and it splits straight
# through this study area. Measured by counting OSM line features around
# each school:
#
#                                    east-sussex   west-sussex
#   Portslade, Hove, Brighton, Varndean, Longhill        0            yes
#   Peacehaven, Newhaven, Seaford                      yes              0
#
# Brighton & Hove is in the WEST extract; the 2028 expansion area is in
# the EAST one. Both are needed.
#
# Putting two .pbf files in the folder does not work: r5r builds from one
# and returns NULL, with no error that names the cause. The smallest
# single Geofabrik region containing both is England at 1.4 GB, which is
# impractical to build. So the two extracts are merged into one file.
#
# Merging needs a command-line tool — see
# https://gis.stackexchange.com/questions/242704/ and
# https://osmcode.org/osmium-tool/. Any one of these will do:
#
#   osmium merge west.pbf east.pbf -o sussex.pbf     (best; conda or WSL)
#   osmosis --rb west.pbf --rb east.pbf --merge --wb sussex.pbf   (pure Java)
#   osmconvert west.pbf east.pbf -o=sussex.pbf       (single Windows .exe)
#
# On Windows with Java already installed, osmosis or osmconvert are the
# least friction; osmium is the better tool if you have conda or WSL.

OSM_SRC   <- file.path(R5_DIR, "osm_src")     # downloads live here...
MERGED_PBF <- file.path(R5_DIR, "sussex-merged.osm.pbf")   # ...only this in R5_DIR
dir.create(OSM_SRC, showWarnings = FALSE, recursive = TRUE)

# Point this at the merge tool if it is not on the PATH. On Windows osmosis
# ships as a .bat, which Sys.which() will not find.
OSM_MERGE_TOOL <- Sys.getenv("OSM_MERGE_TOOL", "E:/osmosis-0.49.2/bin/osmosis.bat")

#' Locate a merge tool: the configured path first, then the PATH
find_merge_tool <- function() {
  if (nzchar(OSM_MERGE_TOOL) && file.exists(OSM_MERGE_TOOL)) {
    nm <- tolower(tools::file_path_sans_ext(basename(OSM_MERGE_TOOL)))
    if (nm %in% c("osmium", "osmosis", "osmconvert"))
      return(c(name = nm, path = normalizePath(OSM_MERGE_TOOL, winslash = "/")))
  }
  for (nm in c("osmium", "osmosis", "osmconvert")) {
    p <- unname(Sys.which(nm))
    if (nzchar(p)) return(c(name = nm, path = p))
  }
  NULL
}

#' osmosis is a Java program launched by a shell script, so it needs a java
#' executable of its own — the JVM already running inside R is no use to it.
#' rJavaEnv installs into a cache directory that is not on the PATH, so find
#' it and hand it over as JAVA_HOME.
java_home_for_subprocess <- function() {
  if (nzchar(Sys.getenv("JAVA_HOME"))) return(Sys.getenv("JAVA_HOME"))
  if (nzchar(Sys.which("java"))) return("")          # already on the PATH
  cands <- c(
    file.path(Sys.getenv("LOCALAPPDATA"), "R", "cache", "R", "rJavaEnv",
              "installed", "windows", "x64", "21"),
    list.dirs(file.path(Sys.getenv("LOCALAPPDATA"), "R", "cache", "R",
                        "rJavaEnv", "installed"), recursive = TRUE)
  )
  for (cd in cands)
    if (nzchar(cd) && file.exists(file.path(cd, "bin", "java.exe")))
      return(normalizePath(cd, winslash = "/"))
  ""
}

merge_pbf <- function(inputs, output) {
  tool <- find_merge_tool()

  if (is.null(tool))
    stop("No OSM merge tool found.\n",
         "The study area spans two Geofabrik extracts and r5r needs one file.\n",
         "Install ONE of these and set OSM_MERGE_TOOL to its path:\n",
         "  osmium      https://osmcode.org/osmium-tool/ (conda install -c conda-forge osmium-tool)\n",
         "  osmosis     https://github.com/openstreetmap/osmosis/releases (pure Java)\n",
         "  osmconvert  https://wiki.openstreetmap.org/wiki/Osmconvert (single .exe)\n\n",
         "Or merge by hand and save the result as:\n  ", output,
         call. = FALSE)

  inputs <- normalizePath(inputs, winslash = "/", mustWork = TRUE)
  outp   <- normalizePath(output, winslash = "/", mustWork = FALSE)

  # Set JAVA_HOME in this process rather than passing system2(env = ...).
  # On Windows that argument is prepended to the COMMAND LINE rather than
  # applied to the environment, so osmosis receives "JAVA_HOME=C:/..." as
  # its first argument and reports:
  #   "Expected argument 1 to be an option or task name."
  jh <- java_home_for_subprocess()
  if (nzchar(jh)) {
    old_jh <- Sys.getenv("JAVA_HOME", unset = NA_character_)
    Sys.setenv(JAVA_HOME = jh)
    on.exit({
      if (is.na(old_jh)) Sys.unsetenv("JAVA_HOME") else Sys.setenv(JAVA_HOME = old_jh)
    }, add = TRUE)
    message("  JAVA_HOME for the merge: ", jh)
  }

  message("  Merging with ", tool[["name"]], " (", tool[["path"]], ")")

  args <- switch(
    tool[["name"]],
    osmium     = c("merge", inputs, "-o", outp, "--overwrite"),
    # Osmosis takes named parameters; the short forms are --rb / --wb
    osmosis    = c(as.vector(rbind("--read-pbf", paste0("file=", inputs))),
                   "--merge", "--write-pbf", paste0("file=", outp)),
    osmconvert = c(inputs, paste0("-o=", outp))
  )

  status <- system2(tool[["path"]], args)

  if (status != 0 || !file.exists(outp))
    stop("The merge failed (exit status ", status, ").\n",
         "If osmosis reported a Java problem, set JAVA_HOME explicitly.",
         call. = FALSE)
  outp
}

if (REBUILD_OSM || !file.exists(MERGED_PBF)) {
  if (!requireNamespace("osmextract", quietly = TRUE))
    stop("install.packages('osmextract')")

  # Download into a subfolder, never into R5_DIR itself — a stray second
  # .pbf beside the merged one reproduces the original failure.
  src <- vapply(c("West Sussex", "East Sussex"), function(region) {
    osmextract::oe_download(
      file_url = osmextract::oe_match(region)$url,
      download_directory = OSM_SRC,
      quiet = FALSE
    )
  }, character(1))

  message("  Source extracts: ", paste(basename(src), collapse = ", "))
  merge_pbf(src, MERGED_PBF)
  message(sprintf("  Merged -> %s (%.0f MB)", basename(MERGED_PBF),
                  file.size(MERGED_PBF) / 1024^2))
} else {
  message("  Reusing merged extract: ", basename(MERGED_PBF))
}

# Whatever happened above, R5_DIR must end up holding exactly one .pbf.
stray <- setdiff(list.files(R5_DIR, pattern = "\\.pbf$", full.names = TRUE),
                 normalizePath(MERGED_PBF, winslash = "/", mustWork = FALSE))
if (length(stray)) {
  message("  Moving ", length(stray), " stray .pbf out of the network folder:")
  for (s in stray) {
    message("    ", basename(s))
    file.rename(s, file.path(OSM_SRC, basename(s)))
  }
}

# Orphaned index sidecars also have to go. r5r writes <name>.pbf.mapdb next
# to each extract; if the extract moves and its indexes stay behind, they
# describe a network that is no longer there.
keep_prefix <- basename(MERGED_PBF)
orphans <- setdiff(
  list.files(R5_DIR, pattern = "\\.mapdb(\\.p)?$", full.names = TRUE),
  list.files(R5_DIR, pattern = paste0("^", keep_prefix, "\\.mapdb"), full.names = TRUE)
)
if (length(orphans)) {
  message("  Removing ", length(orphans), " orphaned index file(s) from a previous extract")
  file.remove(orphans)
}

pbf_paths <- list.files(R5_DIR, pattern = "\\.pbf$", full.names = TRUE)
if (length(pbf_paths) != 1)
  stop("R5_DIR must contain exactly one .pbf; it has ", length(pbf_paths), ".")
message("  Network folder holds one extract: ", basename(pbf_paths))


# ====================================================================
# 5. Build the network and compute the matrix
# ====================================================================

message("\n=== 5. r5r ===")

if (!requireNamespace("r5r", quietly = TRUE))
  stop("install.packages('r5r'). r5r 2.x needs Java 21.")

# ---------------------------------------------------------------------
# THE CACHE TRAP
# ---------------------------------------------------------------------
# setup_r5() reuses an existing network.dat rather than rebuilding. If the
# GTFS or OSM has changed since that file was written, r5r silently keeps
# routing on the OLD data — and the symptom is baffling: it reports no
# transit services on a date you have just verified has thousands.
#
# So: if network.dat is older than any input in the folder, delete it.
# Rebuilding takes about a minute, which is far cheaper than the
# confusion.

net_dat <- file.path(R5_DIR, "network.dat")
if (file.exists(net_dat)) {
  inputs <- list.files(R5_DIR, pattern = "\\.(pbf|zip)$", full.names = TRUE)
  newest_input <- max(file.mtime(inputs))
  if (newest_input > file.mtime(net_dat)) {
    message("  network.dat (", format(file.mtime(net_dat), "%H:%M"),
            ") is older than its inputs (", format(newest_input, "%H:%M"),
            ") — deleting so it rebuilds")
    file.remove(net_dat)
    sj <- file.path(R5_DIR, "network_settings.json")
    if (file.exists(sj)) file.remove(sj)
  } else {
    message("  Reusing cached network.dat (inputs unchanged)")
  }
}

# r5r 2.3.0 renamed setup_r5() to build_network(), and the first argument of
# travel_time_matrix() from `r5r_core` to `r5r_network`. Support both.
r5r_exports <- getNamespaceExports("r5r")
net_builder <- if ("build_network" %in% r5r_exports) r5r::build_network else r5r::setup_r5

# Only pass arguments the installed version actually declares — `verbose`
# has moved in and out of these signatures across releases.
build_args <- list(data_path = R5_DIR)
if ("verbose" %in% names(formals(net_builder))) build_args$verbose <- FALSE

r5r_net <- do.call(net_builder, build_args)

# If build_network() gives nothing back, try the older entry point before
# giving up — some releases keep the network in the deprecated one.
if (is.null(r5r_net) && "setup_r5" %in% r5r_exports) {
  message("  build_network() returned NULL; retrying via setup_r5()")
  s_args <- list(data_path = R5_DIR)
  if ("verbose" %in% names(formals(r5r::setup_r5))) s_args$verbose <- FALSE
  r5r_net <- try(do.call(r5r::setup_r5, s_args), silent = TRUE)
  if (inherits(r5r_net, "try-error")) r5r_net <- NULL
}

# Check we actually got a network back. This matters more than it looks:
# assigning NULL into a list REMOVES that element rather than setting it, so
# a NULL network would silently drop the argument from the call and surface
# as "argument 'r5r_network' is missing, with no default" — an error that
# points at the wrong thing entirely.
if (is.null(r5r_net)) {
  message("\n--- diagnostics ---")
  message("r5r version : ", as.character(utils::packageVersion("r5r")))
  message("builder used: ",
          if ("build_network" %in% r5r_exports) "build_network" else "setup_r5")
  message("its formals : ", paste(names(formals(net_builder)), collapse = ", "))
  jv <- try({
    rJava::.jinit()
    rJava::.jcall("java.lang.System", "S", "getProperty", "java.version")
  }, silent = TRUE)
  message("java version: ", if (inherits(jv, "try-error")) "UNAVAILABLE" else jv)
  message("heap set to : ", getOption("java.parameters"))
  message("files in the network folder:")
  fi <- file.info(list.files(R5_DIR, full.names = TRUE))
  for (i in seq_len(nrow(fi)))
    message(sprintf("  %-46s %8.1f MB  %s", basename(rownames(fi)[i]),
                    fi$size[i] / 1024^2, format(fi$mtime[i], "%H:%M")))
  message("-------------------\n")
  stop("No routable network was returned. The diagnostics above should say why: ",
       "a Java version below 21, a missing .pbf or gtfs.zip, or too little heap ",
       "are the usual causes.")
}

# NOTE: do NOT register on.exit(stop_r5(r5r_net)) here.
#
# At top level in a sourced script, on.exit() attaches to the eval() frame
# for that single expression, so it fires the moment the line completes
# rather than at the end of the run. stop_r5() then shuts the network down
# AND removes the object from the calling environment, so the very next
# line fails with "object 'r5r_net' not found".
#
# The network is stopped explicitly at the end of section 6 instead.

NET_ARG <- if ("r5r_network" %in% names(formals(r5r::travel_time_matrix)))
  "r5r_network" else "r5r_core"

message("  Expecting ", format(n_svc, big.mark = ","), " trips on ", dep_date,
        " from ", basename(gtfs_path),
        " — if r5r reports none, the cache trap has bitten again")
message("  r5r ", as.character(utils::packageVersion("r5r")),
        " | builder: ", if ("build_network" %in% r5r_exports) "build_network" else "setup_r5",
        " | network: ", paste(class(r5r_net), collapse = "/"),
        " | argument: ", NET_ARG)

dest_ll <- DESTINATIONS %>%
  sf::st_as_sf(coords = c("easting", "northing"), crs = 27700) %>%
  sf::st_transform(4326) %>%
  mutate(lon = sf::st_coordinates(.)[, 1], lat = sf::st_coordinates(.)[, 2]) %>%
  sf::st_drop_geometry() %>%
  transmute(id = short, lon, lat)

#' One school at a time, so each can keep its own departure time
#'
#' The network is passed by an explicit branch rather than by building an
#' argument list and assigning into it by name. `args[[nm]] <- value` looks
#' tidier, but if `value` is ever NULL it deletes the element instead of
#' setting it, and the resulting "argument is missing" error names the
#' argument rather than the NULL that caused it.
one_school <- function(short_name) {
  clock <- if (short_name %in% c("longhill", "elm_grove"))
    R5_PARAMS$longhill_time else R5_PARAMS$default_time

  dep <- as.POSIXct(paste(dep_date, clock), tz = "GMT")
  dest_one <- dest_ll %>% filter(id == short_name)
  stopifnot(nrow(dest_one) == 1)

  ttm <- if (NET_ARG == "r5r_network") {
    r5r::travel_time_matrix(
      r5r_network        = r5r_net,
      origins            = origins,
      destinations       = dest_one,
      mode               = R5_PARAMS$mode,
      departure_datetime = dep,
      max_walk_time      = R5_PARAMS$max_walk_time,
      max_trip_duration  = R5_PARAMS$max_trip_duration,
      time_window        = R5_PARAMS$time_window,
      percentiles        = R5_PARAMS$percentiles,
      progress           = FALSE
    )
  } else {
    r5r::travel_time_matrix(
      r5r_core           = r5r_net,
      origins            = origins,
      destinations       = dest_one,
      mode               = R5_PARAMS$mode,
      departure_datetime = dep,
      max_walk_time      = R5_PARAMS$max_walk_time,
      max_trip_duration  = R5_PARAMS$max_trip_duration,
      time_window        = R5_PARAMS$time_window,
      percentiles        = R5_PARAMS$percentiles,
      progress           = FALSE
    )
  }

  tcol <- grep("travel_time", names(ttm), value = TRUE)[1]
  ttm %>%
    transmute(id = as.character(from_id),
              !!paste0("time_", short_name) := as.numeric(.data[[tcol]]))
}

message("  Routing ", nrow(dest_ll), " destinations x ",
        format(nrow(origins), big.mark = ","), " origins ...")

t0 <- Sys.time()
mats <- purrr::map(dest_ll$id, function(s) {
  message("    ", s)
  one_school(s)
})
message(sprintf("  done in %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins"))))

travel <- purrr::reduce(mats, full_join, by = "id")


# ====================================================================
# 6. Derived columns, matching the published file's shape
# ====================================================================

message("\n=== 6. Assembling ===")

# Closest three among the REAL schools only — the hypothetical Elm Grove
# site must not contaminate a "nearest school" column.
real_cols <- paste0("time_", DESTINATIONS$short[DESTINATIONS$in_scope != "hypothetical"])

long <- travel %>%
  select(id, all_of(real_cols)) %>%
  tidyr::pivot_longer(-id, names_to = "school", values_to = "t",
                      names_prefix = "time_") %>%
  filter(is.finite(t)) %>%
  group_by(id) %>%
  arrange(t, .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  ungroup()

closest <- long %>%
  filter(rank <= 3) %>%
  select(id, rank, school, t) %>%
  tidyr::pivot_wider(names_from = rank, values_from = c(school, t)) %>%
  rename(first_closest_travel = school_1, second_closest_travel = school_2,
         third_closest_travel = school_3,
         first_closest_time = t_1, second_closest_time = t_2, third_closest_time = t_3)

stats <- long %>%
  group_by(id) %>%
  summarise(max_travel = max(t), min_travel = min(t),
            avg_travel = round(mean(t), 1), .groups = "drop")

out <- travel %>%
  left_join(closest, by = "id") %>%
  left_join(stats, by = "id") %>%
  left_join(onspd %>% transmute(id = postcode, long, lat, lsoa21, oslaua, osward),
            by = "id")

message("  Rows: ", format(nrow(out), big.mark = ","))
message("  Unreachable pairs: ",
        sum(!is.finite(as.matrix(out[, grep("^time_", names(out))]))))

readr::write_csv(out, file.path(TRAVEL_DIR, "bn_pcds_sch_travel_extended.csv"))

saveRDS(list(destinations = DESTINATIONS, outcodes = OUTCODES,
             params = R5_PARAMS, gtfs = basename(gtfs_path),
             gtfs_coverage = coverage, pbf = basename(pbf_paths),
             n_origins = nrow(origins), built_at = Sys.time()),
        file.path(TRAVEL_DIR, "build_metadata.rds"))

message("\nSaved public/output/travel/bn_pcds_sch_travel_extended.csv")

# Shut the network down now that everything is written. Doing it here
# rather than via on.exit() is deliberate — see the note in section 5.
try(r5r::stop_r5(r5r_net), silent = TRUE)
message("
-----------------------------------------------------------------------
NEXT
-----------------------------------------------------------------------
* Compare against the published matrix before adopting it. The overlap is
  BN1/2/3/41; times there should be close but will not be identical, since
  the road network is no longer clipped and a few routes will now be found
  that previously were not. A systematic shift in the overlap means
  something changed that should be understood, not absorbed.
* Then point OPEN_TRAVEL at the new file and re-run the pipelines. The
  distance-to-time approximation in 01_open_inputs.R and 04_lgr_expansion.R
  becomes unnecessary for everything it now covers.
-----------------------------------------------------------------------
")
