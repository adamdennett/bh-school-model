# 00_open_core.R — Open-data core: paths, schools, and the SIM functions
# ======================================================================
# This folder is a SELF-CONTAINED, PUBLISHABLE version of the Brighton &
# Hove school demand model. It is deliberately duplicated rather than
# sourced from the parent project so that the whole `public/` directory
# can be lifted into a public repository without dragging anything else
# with it.
#
# ----------------------------------------------------------------------
# THE RULE THIS FILE ENFORCES
# ----------------------------------------------------------------------
# Nothing here reads pupil-level admissions records, and nothing here
# reads a model object fitted on them. Every input is either published by
# a public body or reproducible from published sources with the code in
# this folder. `assert_open()` below refuses to open a path inside the
# restricted directories, so a careless edit fails loudly instead of
# quietly contaminating a public artefact.
#
# ----------------------------------------------------------------------
# DATA SOURCES — all open
# ----------------------------------------------------------------------
#  Travel times   r5r routing over OpenStreetMap + Brighton & Hove GTFS
#                 (May 2024). Derived, contains no personal data: it is a
#                 postcode-to-school time matrix, and postcodes with
#                 coordinates are published by ONS in the ONSPD.
#  Admissions     Brighton & Hove City Council published allocation
#                 factsheets, 2013-2026 (offers, preferences, PAN).
#  Schools        DfE Get Information About Schools (GIAS).
#  Attainment     DfE performance tables via the school_attainment_tool
#                 pipeline (Attainment 8, Ofsted, workforce, finance).
#  Population     ONS small area population estimates, mid-2022, and
#                 LSOA child projections.
#  Boundaries     ONS Open Geography Portal; council catchment map.
#  Reorganisation MHCLG decision letters, 16 July 2026.
# ======================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
  library(here)
})

# ---- Paths ----------------------------------------------------------
# Override any of these with an environment variable of the same name.

OPEN <- list(
  # The routed matrix rebuilt by 00b_build_travel_matrix.R: 7,896
  # postcodes across BN1/2/3/7/9/10/25/41/42/45, 15 destinations, and a
  # real routed column for the hypothetical Elm Grove site. Set
  # OPEN_TRAVEL to the older published file to fall back — but note that
  # the older file has no Elm Grove column, so every relocation result
  # silently collapses to zero if it is used.
  travel     = Sys.getenv("OPEN_TRAVEL",
                          here::here("public", "output", "travel",
                                     "bn_pcds_sch_travel_extended.csv")),
  admissions = Sys.getenv("OPEN_ADMISSIONS",
                          "E:/BH_Schools_2/data/Yr7_admissions.csv"),
  # The catchment map in force from September 2026 entry. See
  # CATCHMENT_REGIMES below for the pre-2024 map this replaced.
  catchments = Sys.getenv("OPEN_CATCHMENTS",
                          "E:/school_attainment_tool/data/optionZ_Mar25.geojson"),
  onspd      = Sys.getenv("OPEN_ONSPD",
                          "E:/BH_Schools_2/data/ONSPD_FEB_2024_UK_BN.csv"),
  # ONS small-area population by single year of age. Used to split each
  # ward's children between its LSOAs by their real age-11 distribution
  # rather than by postcode count, which assumes a uniform child density.
  lsoa_age11 = Sys.getenv("OPEN_LSOA_AGE11",
                          "E:/BH_Schools_Consultation/data/lsoa_age_11_props_totals.xlsx"),
  sape       = Sys.getenv("OPEN_SAPE",
                          "E:/BH_Reoganisation/data/sapewardstablefinal.xlsx"),
  wards      = Sys.getenv("OPEN_WARDS",
                          "E:/BH_Reoganisation/data/WD_MAY_2025_UK_BFC_865555711586926236.gpkg"),
  panel      = Sys.getenv("OPEN_PANEL",
                          "E:/school_attainment_tool/data/panel_data.rds"),
  gias       = Sys.getenv("OPEN_GIAS",
                          "E:/BH_Schools_2/data/edubasealldata20241003.xlsx")
)

PUBLIC_OUT <- here::here("public", "output")
dir.create(PUBLIC_OUT, showWarnings = FALSE, recursive = TRUE)


# ---- Catchment regimes ----------------------------------------------
# Brighton & Hove consulted on secondary catchment boundaries in 2024 and
# determined a new scheme in March 2025. Under the School Admissions Code
# an arrangement determined in February/March of year Y first applies to
# entry in September Y+1, so the new map governs entry from 2026.
#
# The two maps are near-identical outside east Brighton. The redraw swaps
# Kemptown into the Longhill catchment and moves part of Whitehawk &
# Marina out of it into Stringer/Varndean; 8.4% of the city's live
# postcodes change catchment.
#
# Both files are published boundary sets and carry no personal data.

CATCHMENT_REGIMES <- list(
  pre2024 = list(
    label = "Pre-2024 catchments",
    note  = "The map in force for entry up to and including 2025.",
    path  = "E:/BH_Schools_2/data/BrightonSecondaryCatchments.geojson",
    field = "AreaName"),
  optionZ = list(
    label = "Option Z (in force from 2026)",
    note  = "Determined March 2025 following the 2024 consultation.",
    path  = "E:/school_attainment_tool/data/optionZ_Mar25.geojson",
    field = "catchment")
)

# First entry year governed by the new map.
OPTIONZ_FROM <- as.integer(Sys.getenv("OPTIONZ_FROM", "2026"))

#' Read a catchment map and normalise its area names
#'
#' The two published files label the same six areas differently. This
#' returns an sf with a single `catchment` column using the internal
#' names (PACA, Hove_Blatch, Patcham, DS_Varndean, BACA, Longhill).
#'
#' @param regime name in CATCHMENT_REGIMES, or a path
#' @param field  column holding the area name, if `regime` is a path
read_catchments <- function(regime = "optionZ", field = NULL) {
  if (regime %in% names(CATCHMENT_REGIMES)) {
    spec  <- CATCHMENT_REGIMES[[regime]]
    path  <- spec$path
    field <- spec$field
  } else {
    path <- regime
    if (is.null(field)) stop("field must be given when regime is a path")
  }
  if (!file.exists(path)) stop("catchment map not found: ", path)
  # assert_open() is defined below; resolved at call time, not at definition.
  path <- assert_open(path)

  sf::st_read(path, quiet = TRUE) |>
    sf::st_transform(27700) |>
    sf::st_make_valid() |>
    dplyr::mutate(catchment = dplyr::recode(as.character(.data[[field]]),
      # pre-2024 file
      "Patcham HighSchool" = "Patcham", "StringerVarndean" = "DS_Varndean",
      "BrightonAldridge"   = "BACA",    "BlatchingtonHove" = "Hove_Blatch",
      "Portslade"          = "PACA",
      # option Z file
      "VarndeanStringer"   = "DS_Varndean", "HoveBlatchington" = "Hove_Blatch",
      # common to both
      "Longhill" = "Longhill", "BACA" = "BACA", "PACA" = "PACA",
      "Patcham"  = "Patcham")) |>
    dplyr::select(catchment)
}


# Directories this bundle must never read from
RESTRICTED <- c(
  normalizePath(here::here("data"),   mustWork = FALSE),   # pupil records
  normalizePath(here::here("output"), mustWork = FALSE)    # fits on those records
)

#' Refuse to read anything from the restricted directories
#'
#' Called by every reader in this folder. If a path resolves inside the
#' pupil-data directory or the restricted model outputs, stop.
assert_open <- function(path) {
  p <- normalizePath(path, mustWork = FALSE)
  for (r in RESTRICTED) {
    if (startsWith(p, r)) {
      stop("BLOCKED: '", path, "' is inside a restricted directory (", r, ").\n",
           "The public bundle must not read pupil-level records or any model ",
           "fitted on them.", call. = FALSE)
    }
  }
  if (!file.exists(p))
    stop("Open input not found: ", path,
         "\nSet the corresponding OPEN_* environment variable.", call. = FALSE)
  p
}

read_open_csv <- function(path, ...) readr::read_csv(assert_open(path), show_col_types = FALSE, ...)
read_open_rds <- function(path)      readRDS(assert_open(path))


# ---- Schools --------------------------------------------------------
# Everything here is published: locations and capacity from GIAS,
# admission numbers from the council's own allocation factsheets.

SCHOOLS_OPEN <- tibble::tribble(
  ~urn,   ~short,         ~name,                                   ~easting, ~northing, ~catchment,    ~faith, ~pan2024, ~pan2026, ~pan2030,
  114579, "varndean",     "Varndean School",                          531232,    107295, "DS_Varndean", FALSE,      300,      270,      240,
  # 2026/27 numbers follow the adjudicator's determination of 20 October
  # 2025 (ADA4423, ADA4452-4454, ADA4456, ADA4458), which is binding: the
  # council's proposed reductions at Blatchington Mill and Dorothy
  # Stringer (330 -> 300) were overturned and both stay at 330. Longhill's
  # reduction from 270 to 210 was unopposed and stands.
  114580, "ds",           "Dorothy Stringer School",                  530794,    107134, "DS_Varndean", FALSE,      330,      330,      270,
  114581, "longhill",     "Longhill High School",                     536239,    103942, "Longhill",    FALSE,      270,      210,      180,
  114606, "blatchington", "Blatchington Mill School",                 528137,    106649, "Hove_Blatch", FALSE,      330,      330,      240,
  114607, "hove_park",    "Hove Park School",                         528269,    106172, "Hove_Blatch", FALSE,      180,      180,      180,
  114608, "patcham",      "Patcham High School",                      530717,    108617, "Patcham",     FALSE,      225,      225,      180,
  114611, "cardinal_n",   "Cardinal Newman Catholic School",          529812,    105778, NA_character_, TRUE,       360,      360,      360,
  136164, "baca",         "Brighton Aldridge Community Academy",      534234,    108203, "BACA",        FALSE,      180,      180,      180,
  137063, "paca",         "Portslade Aldridge Community Academy",     524992,    107180, "PACA",        FALSE,      220,      220,      220,
  139409, "kings",        "King's School",                            527377,    107625, NA_character_, TRUE,       165,      165,      165,
  # Joins Brighton & Hove on 1 April 2028 under the boundary change.
  # PAN is GIAS capacity spread over five year groups.
  144661, "peacehaven",   "Peacehaven Community School",              541262,    101530, "Peacehaven",  FALSE,      180,      180,      180
)

CATCHMENT_SCHOOLS_OPEN <- list(
  PACA        = "Portslade Aldridge Community Academy",
  Hove_Blatch = c("Hove Park School", "Blatchington Mill School"),
  Patcham     = "Patcham High School",
  DS_Varndean = c("Dorothy Stringer School", "Varndean School"),
  BACA        = "Brighton Aldridge Community Academy",
  Longhill    = "Longhill High School",
  Peacehaven  = "Peacehaven Community School"
)

# Wards joining Brighton & Hove in 2028
LGR_WARDS_OPEN <- c(
  "E05011585" = "East Saltdean & Telscombe Cliffs",
  "E05011594" = "Peacehaven East",
  "E05011595" = "Peacehaven North",
  "E05011596" = "Peacehaven West"
)

ELM_GROVE <- list(easting = 533118, northing = 105231,
                  label = "Top of Elm Grove (Race Hill ridge)")


# ---- Model functions ------------------------------------------------
# Production-constrained spatial interaction model,
#   T_ij = A_i O_i W_j^alpha d_ij^-beta,  A_i = 1 / sum_j W_j^alpha d_ij^-beta
# after Dennett, "Idiots' Guide to Spatial Interaction Modelling", Part 2
# (https://rpubs.com/adam_dennett/259068).

euclid_m <- function(e1, n1, e2, n2) sqrt((e1 - e2)^2 + (n1 - n2)^2)

CalcRSquared <- function(observed, estimated) cor(observed, estimated, use = "complete.obs")^2
CalcRMSE     <- function(observed, estimated) round(sqrt(mean((observed - estimated)^2, na.rm = TRUE)), 3)

prod_constrained_sim <- function(df, o_col = "Oi", w_col = "Wj", c_col = "cij",
                                 orig_col = "orig", dest_col = "dest",
                                 alpha = 1, beta = 1.5, extra_utility = 0) {
  Oi  <- df[[o_col]]
  Wj  <- pmax(df[[w_col]], 1e-6)
  cij <- pmax(df[[c_col]], 1e-6)

  df$dest_term <- (Wj^alpha) * (cij^(-beta)) * exp(extra_utility)

  Ai <- df %>%
    group_by(.data[[orig_col]]) %>%
    summarise(A_i = 1 / sum(dest_term, na.rm = TRUE), .groups = "drop")

  df <- df %>% left_join(Ai, by = orig_col)
  df$sim_flow <- df$A_i * Oi * df$dest_term
  df
}

# ---- Faith schools and the restricted choice set --------------------
# Cardinal Newman and King's admit on religious criteria, so they are not
# in every family's choice set. Ignoring that makes the model badly
# over-predict second preferences to them — 447 against 212 observed at a
# plausible decay parameter. Splitting demand into a share for whom the
# faith schools are available and a share for whom they are not lifts the
# fit of the second-preference profile from R2 0.60 to 0.90. The share is
# estimated in 01c_identification_test.R.

FAITH_SCHOOLS <- c("Cardinal Newman Catholic School", "King's School")
FAITH_ELIGIBLE_SHARE <- 0.5

#' Production-constrained SIM with a restricted choice set for faith schools
#'
#' Runs the model twice — once with every school available, once with the
#' faith schools removed — and combines by the eligible share. Because the
#' model is origin-constrained, dropping the faith schools automatically
#' redistributes those families across the remaining options.
sim_faith_split <- function(df, o_col = "Oi", w_col = "Wj", c_col = "cij",
                            orig_col = "orig", dest_col = "dest",
                            alpha = 1, beta = 1.5, extra_utility = 0,
                            phi = FAITH_ELIGIBLE_SHARE) {

  if (phi >= 1) {
    return(prod_constrained_sim(df, o_col, w_col, c_col, orig_col, dest_col,
                                alpha, beta, extra_utility))
  }

  elig <- prod_constrained_sim(df, o_col, w_col, c_col, orig_col, dest_col,
                               alpha, beta, extra_utility)

  df_no <- df
  is_faith <- df_no[[dest_col]] %in% FAITH_SCHOOLS
  df_no[[w_col]][is_faith] <- 1e-12
  noel <- prod_constrained_sim(df_no, o_col, w_col, c_col, orig_col, dest_col,
                               alpha, beta, extra_utility)

  df$sim_flow <- phi * elig$sim_flow + (1 - phi) * noel$sim_flow
  df$A_i <- elig$A_i
  df
}

#' Constrain modelled flows to capacity, without inflating undersubscribed
#' schools. Origins stay exactly constrained; destinations are capped.
ipf_capacity <- function(df, cap, orig_col = "orig", dest_col = "dest",
                         flow_col = "sim_flow", o_col = "Oi",
                         max_iter = 200, tol = 1e-4) {

  flows <- as.numeric(df[[flow_col]])
  orig  <- as.character(df[[orig_col]])
  dest  <- as.character(df[[dest_col]])

  gsum <- function(x, g) { s <- tapply(x, g, sum, na.rm = TRUE); setNames(as.numeric(s), names(s)) }

  o_target <- df %>%
    group_by(.data[[orig_col]]) %>%
    summarise(target = first(.data[[o_col]]), .groups = "drop")
  o_vec <- setNames(as.numeric(o_target$target), as.character(o_target[[orig_col]]))

  for (it in seq_len(max_iter)) {
    o_now <- gsum(flows, orig)
    o_fac <- o_vec[names(o_now)] / o_now
    names(o_fac) <- names(o_now); o_fac[!is.finite(o_fac)] <- 1
    flows <- flows * as.numeric(o_fac[orig])

    d_now <- gsum(flows, dest)
    # Named vector first: pmin(1, x) copies attributes from the scalar and
    # silently drops the names.
    d_fac <- pmin(cap[names(d_now)] / d_now, 1)
    names(d_fac) <- names(d_now); d_fac[!is.finite(d_fac)] <- 1
    flows <- flows * as.numeric(d_fac[dest])

    d_chk <- gsum(flows, dest); o_chk <- gsum(flows, orig)
    if (max(d_chk - cap[names(d_chk)]) <= tol &&
        max(abs(o_chk - o_vec[names(o_chk)])) <= tol) break
  }

  df$sim_flow_capped <- flows
  df
}

normalise_pcd <- function(x) {
  x <- gsub("[^A-Z0-9]", "", toupper(trimws(as.character(x))))
  ifelse(nchar(x) >= 5,
         paste0(substr(x, 1, nchar(x) - 3), " ", substr(x, nchar(x) - 2, nchar(x))),
         NA_character_)
}

message("public/00_open_core.R loaded — ", nrow(SCHOOLS_OPEN),
        " schools; restricted paths blocked.")
