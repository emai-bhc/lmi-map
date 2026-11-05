# Pure logic: point-in-polygon lookup and LMI classification.
suppressPackageStartupMessages(library(sf))

LMI_THRESHOLD <- 51

# TRUE if pct >= 51; NA if pct is missing
is_lmi <- function(pct, threshold = LMI_THRESHOLD) {
  pct <- suppressWarnings(as.numeric(pct))
  ifelse(is.na(pct), NA, pct >= threshold)
}

# Look up a single point (lng/lat, WGS84) in a polygon layer.
# Returns list(found, pct, lmi, geoid)
lookup_point <- function(lng, lat, layer) {
  none <- list(found = FALSE, pct = NA_real_, lmi = NA, geoid = NA_character_)
  if (is.null(layer) || anyNA(c(lng, lat))) return(none)
  old <- sf_use_s2()
  on.exit(suppressMessages(sf_use_s2(old)), add = TRUE)
  suppressMessages(sf_use_s2(FALSE))
  pt <- st_sfc(st_point(c(lng, lat)), crs = 4326)
  pt <- st_transform(pt, st_crs(layer))
  hit <- suppressMessages(suppressWarnings(st_intersects(pt, layer, sparse = TRUE)))[[1]]
  if (length(hit) == 0) return(none)
  row <- layer[hit[1], ]
  pct <- as.numeric(row$LOWMODPCT)
  list(found = TRUE, pct = pct, lmi = is_lmi(pct),
       geoid = as.character(row$GEOID))
}

# Rough Virginia bounding box sanity check (lng/lat)
in_virginia_bbox <- function(lng, lat) {
  !anyNA(c(lng, lat)) && lng >= -83.7 && lng <= -75.2 && lat >= 36.5 && lat <= 39.5
}

# Combine block group and tract results into display text
describe_result <- function(bg, tract) {
  fmt <- function(r, label) {
    if (!isTRUE(r$found)) return(sprintf("%s: no match found", label))
    if (is.na(r$pct)) return(sprintf("%s %s: no LMI data available", label, r$geoid))
    sprintf("%s %s: %.1f%% low/moderate income (%s)", label, r$geoid, r$pct,
            if (isTRUE(r$lmi)) "meets 51% threshold" else "below 51% threshold")
  }
  any_lmi <- isTRUE(bg$lmi) || isTRUE(tract$lmi)
  none_found <- !isTRUE(bg$found) && !isTRUE(tract$found)
  headline <- if (none_found) {
    "This location is not within any HUD block group or tract in the Virginia data."
  } else if (isTRUE(bg$lmi) && isTRUE(tract$lmi)) {
    "This address is located in a low to moderate income neighborhood (block group and tract)."
  } else if (isTRUE(bg$lmi)) {
    "This address is located in a low to moderate income neighborhood (by block group)."
  } else if (isTRUE(tract$lmi)) {
    "This address is located in a low to moderate income neighborhood (by tract)."
  } else {
    "This address is NOT located in a low to moderate income neighborhood."
  }
  list(headline = headline, lmi = any_lmi, none_found = none_found,
       bg_line = fmt(bg, "Block group"), tract_line = fmt(tract, "Tract"))
}
