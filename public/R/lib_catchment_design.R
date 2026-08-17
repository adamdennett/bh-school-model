# public/R/lib_catchment_design.R — capacitated catchment design
# ======================================================================
# The power-diagram catchment designer, as a pure algorithm. It touches
# no data of its own: everything arrives through arguments, so the same
# code runs over the private pupil-record costs and over the published
# open inputs, and identical inputs must give identical geometry.
#
# Sourced by R/06_catchment_design.R and by
# public/R/08_catchment_design_open.R.
# ======================================================================

# ====================================================================
# 2. Capacitated catchment design
# ====================================================================

#' Design catchments as a capacitated power diagram
#'
#' @param costs   zone x school cost table
#' @param pans    named vector of PANs
#' @param exclude schools with no geographic catchment (faith schools)
#' @param tol     acceptable |assigned - target| / target
#' @return tibble(zone, catchment) plus diagnostics
design_catchments <- function(costs, pans, zones, tol = 0.03, min_abs = 15,
                              max_iter = 3000, eta0 = 0.8,
                              areas,
                              n_candidates = 3, cap_target = TRUE) {

  areas <- areas[!names(areas) %in% "___none___"]

  # Cost to a catchment area = time to the nearest of its schools
  area_cost <- purrr::imap_dfr(areas, function(members, aname) {
    costs %>%
      filter(pref_name %in% members) %>%
      group_by(zone) %>%
      summarise(cost = min(cij), .groups = "drop") %>%
      mutate(catchment = aname)
  })

  cap <- purrr::map_dbl(areas, ~ sum(pans[.x], na.rm = TRUE))

  # Targets: proportional to capacity, scaled to the children available.
  # The scale factor is what absorbs faith-school, independent and
  # out-of-city leakage.
  total_children <- sum(zones$Oi)
  target <- cap / sum(cap) * total_children

  # A catchment cannot sensibly be asked to hold more children than the
  # school has places. Without this, the scaling pushed Patcham's target
  # to 275 against a PAN of 225, and the price adjustment then had to
  # reach a long way to find the extra children. Capping leaves some
  # children unassigned to any geographic catchment, which is the correct
  # behaviour: about a fifth of the city's children attend the two
  # city-wide faith schools or leave the system altogether, and they were
  # never the geography's to place.
  if (cap_target) target <- pmin(target, cap)

  cmat <- area_cost %>%
    tidyr::pivot_wider(names_from = catchment, values_from = cost) %>%
    arrange(match(zone, zones$zone))
  zid  <- cmat$zone
  C    <- as.matrix(cmat[, names(areas), drop = FALSE])
  Oi   <- zones$Oi[match(zid, zones$zone)]

  # A catchment's target is met to within whichever is the more forgiving of
  # a relative and an absolute tolerance. Exact convergence is not always
  # attainable: zones are indivisible, and for a small catchment a single
  # LSOA can be a tenth of its target, so the assignment can only ever step
  # past the target rather than land on it.
  band <- pmax(tol * target[colnames(C)], min_abs)

  # Geographic sanity. The price adjustment minimises (travel + price),
  # and a large enough price can send a zone somewhere absurd: earlier
  # versions put two Peacehaven LSOAs in the Stringer and Varndean
  # catchment while giving two central Brighton LSOAs to Peacehaven, a
  # straight swap across the whole city. Restricting each zone to its
  # nearest few catchments by journey time keeps the optimisation honest
  # without dictating the answer.
  if (!is.na(n_candidates) && n_candidates < ncol(C)) {
    keep <- t(apply(C, 1, function(r) rank(r, ties.method = "first") <= n_candidates))
    C[!keep] <- Inf
    message(sprintf("  Each zone may be assigned to its %d nearest catchments only",
                    n_candidates))
  }

  price <- rep(0, ncol(C)); names(price) <- colnames(C)
  scale <- mean(C[is.finite(C)], na.rm = TRUE) / 10

  best <- list(score = Inf)
  converged <- FALSE

  for (it in seq_len(max_iter)) {
    eta <- eta0 / (1 + it / 200)          # decay, so late steps stop overshooting
    assign_idx <- max.col(-sweep(C, 2, price, "+"), ties.method = "first")
    assigned   <- tapply(Oi, factor(colnames(C)[assign_idx], levels = colnames(C)),
                         sum, default = 0)
    excess <- assigned - target[colnames(C)]
    score  <- max(abs(excess) / band)

    if (score < best$score)
      best <- list(score = score, idx = assign_idx, price = price,
                   assigned = assigned, excess = excess, iter = it)

    if (score <= 1) { converged <- TRUE; break }
    price <- price + eta * (excess / target[colnames(C)]) * scale
  }

  assign_idx <- best$idx
  out <- tibble(zone = zid, catchment = colnames(C)[assign_idx])

  list(
    assignment = out,
    price      = best$price,
    target     = target,
    assigned   = best$assigned,
    excess     = best$excess,
    band       = band,
    worst_ratio = best$score,
    iterations = best$iter,
    converged  = converged,
    total_minutes = sum(Oi * C[cbind(seq_along(assign_idx), assign_idx)]),
    mean_minutes  = weighted.mean(C[cbind(seq_along(assign_idx), assign_idx)], Oi)
  )
}


#' Are the designed catchments spatially contiguous?
#'
#' Adjacency comes from a Delaunay triangulation of the zone centroids,
#' which is a reasonable stand-in for polygon adjacency and needs no
#' boundary file. A catchment is contiguous if its zones form one connected
#' component of that graph.
#' Adjacency between zones, from a Delaunay triangulation of their centroids
#'
#' Long edges are dropped. A raw triangulation connects points around the
#' convex hull that are nowhere near each other — in this data it links
#' north Patcham to the south-east coast — and treating those as
#' neighbourhood adjacencies makes the repair step reassign whole districts
#' across the city.
zone_adjacency <- function(assignment, zones, max_km = 2.5) {
  xy <- zones %>%
    filter(zone %in% assignment$zone) %>%
    arrange(match(zone, assignment$zone))
  dl <- deldir::deldir(xy$zone_e, xy$zone_n, suppressMsge = TRUE)
  e <- dl$delsgs[, c("ind1", "ind2")]
  len <- euclid_m(xy$zone_e[e$ind1], xy$zone_n[e$ind1],
                  xy$zone_e[e$ind2], xy$zone_n[e$ind2]) / 1000
  # Keep a generous cap so genuinely sparse areas stay connected
  keep <- len <= max(max_km, quantile(len, 0.75))
  e[keep, , drop = FALSE]
}

#' Connected components of one catchment, as a list of index vectors
components_of <- function(idx, edges) {
  e <- edges[edges$ind1 %in% idx & edges$ind2 %in% idx, , drop = FALSE]
  adj <- split(c(e$ind2, e$ind1), c(e$ind1, e$ind2))
  seen <- integer(0); comps <- list()
  for (s in idx) {
    if (s %in% seen) next
    queue <- s; this <- integer(0)
    while (length(queue)) {
      v <- queue[1]; queue <- queue[-1]
      if (v %in% seen) next
      seen <- c(seen, v); this <- c(this, v)
      queue <- c(queue, setdiff(as.integer(adj[[as.character(v)]]), seen))
    }
    comps[[length(comps) + 1L]] <- this
  }
  comps
}

check_contiguity <- function(assignment, zones, edges = NULL) {
  if (is.null(edges)) edges <- zone_adjacency(assignment, zones)
  purrr::map_dfr(unique(assignment$catchment), function(cc) {
    idx <- which(assignment$catchment == cc)
    comps <- components_of(idx, edges)
    tibble(catchment = cc, n_zones = length(idx), components = length(comps),
           largest_component = max(lengths(comps)),
           contiguous = length(comps) <= 1L)
  })
}

#' Absorb detached fragments into the cheapest adjacent catchment
#'
#' A power diagram is contiguous in continuous space, but this one is built
#' on predicted travel times rather than straight-line distance and evaluated
#' at discrete centroids, so small detached pieces can survive. Each fragment
#' that is not its catchment's main body is handed to whichever neighbouring
#' catchment costs its children least. Capacity is then re-reported, because
#' the repair trades a little capacity accuracy for a usable map.
repair_fragments <- function(design, costs, areas, zones,
                             n_candidates = 3) {

  assignment <- design$assignment
  edges <- zone_adjacency(assignment, zones)

  area_names <- names(areas)
  area_cost <- purrr::imap_dfr(areas, function(members, aname) {
    costs %>% filter(pref_name %in% members) %>%
      group_by(zone) %>% summarise(cost = min(cij), .groups = "drop") %>%
      mutate(catchment = aname)
  }) %>%
    tidyr::pivot_wider(names_from = catchment, values_from = cost) %>%
    arrange(match(zone, assignment$zone))

  C <- as.matrix(area_cost[, area_names, drop = FALSE])

  # The same geographic constraint the design step applies. Without it the
  # rebalancing was free to swap zones across the whole city: it was the
  # repair, not the design, that put two Peacehaven LSOAs into the
  # Stringer and Varndean catchment and two central Brighton LSOAs into
  # Peacehaven. The design had them right.
  if (!is.na(n_candidates) && n_candidates < ncol(C)) {
    keep <- t(apply(C, 1, function(r) rank(r, ties.method = "first") <= n_candidates))
    C[!keep] <- Inf
  }
  moved <- 0L

  for (cc in area_names) {
    idx <- which(assignment$catchment == cc)
    if (length(idx) < 2) next
    comps <- components_of(idx, edges)
    if (length(comps) <= 1) next
    keep <- which.max(lengths(comps))
    for (k in setdiff(seq_along(comps), keep)) {
      frag <- comps[[k]]
      # Only absorb genuinely small detached pieces. A "fragment" holding a
      # quarter of the catchment is not a stray edge case, it is a sign the
      # design itself is split, and silently moving it would wreck the
      # capacity balance the design exists to achieve.
      if (length(frag) > max(2L, floor(0.2 * length(idx)))) next
      nb <- unique(assignment$catchment[c(
        edges$ind2[edges$ind1 %in% frag], edges$ind1[edges$ind2 %in% frag])])
      nb <- setdiff(nb, cc)
      if (length(nb) == 0) next
      # Candidate costs are Inf where the geographic constraint forbids the
      # move. which.min would still return one of them, which is how two
      # Peacehaven LSOAs ended up in the Stringer and Varndean catchment.
      cand <- colMeans(C[frag, nb, drop = FALSE])
      if (!any(is.finite(cand))) next
      best_nb <- nb[which.min(replace(cand, !is.finite(cand), Inf))]
      assignment$catchment[frag] <- best_nb
      moved <- moved + length(frag)
    }
  }

  # --- Rebalance -----------------------------------------------------
  # Absorbing fragments knocks catchments off their targets, so trade zones
  # back across boundaries until capacity is restored. Only boundary zones
  # move, and only if the move leaves both catchments in one piece, so the
  # map stays a map.
  Oi <- zones$Oi[match(assignment$zone, zones$zone)]
  target <- design$target[area_names]
  band   <- design$band[area_names]

  keeps_contiguous <- function(a, cc) {
    idx <- which(a == cc)
    if (length(idx) == 0) return(TRUE)
    length(components_of(idx, edges)) <= 1L
  }

  for (step in seq_len(400)) {
    assigned <- tapply(Oi, factor(assignment$catchment, levels = area_names), sum, default = 0)
    excess <- (assigned - target) / band
    if (max(abs(excess)) <= 1) break

    from <- area_names[which.max(excess)]
    to_candidates <- area_names[excess < -0.2]
    if (length(to_candidates) == 0) break

    # Boundary zones of `from` that touch an under-target catchment
    idx_from <- which(assignment$catchment == from)
    touching <- purrr::map_dfr(idx_from, function(i) {
      nb <- unique(assignment$catchment[c(
        edges$ind2[edges$ind1 == i], edges$ind1[edges$ind2 == i])])
      nb <- intersect(setdiff(nb, from), to_candidates)
      if (length(nb) == 0) return(NULL)
      pen <- C[i, nb] - C[i, from]
      keep <- is.finite(pen)
      if (!any(keep)) return(NULL)
      tibble(i = i, to = nb[keep], penalty = pen[keep])
    })
    if (nrow(touching) == 0) break

    moved_one <- FALSE
    for (r in order(touching$penalty)) {
      i <- touching$i[r]; to <- touching$to[r]
      trial <- assignment$catchment
      trial[i] <- to
      if (keeps_contiguous(trial, from) && keeps_contiguous(trial, to)) {
        assignment$catchment <- trial
        moved_one <- TRUE
        break
      }
    }
    if (!moved_one) break
  }

  assigned <- tapply(Oi, factor(assignment$catchment, levels = area_names), sum, default = 0)

  design$assignment_repaired <- assignment
  design$repaired_zones <- moved
  design$rebalance_steps <- step
  design$repaired_assigned <- assigned
  design$repaired_excess <- assigned - design$target[area_names]
  design
}


