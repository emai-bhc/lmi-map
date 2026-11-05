# Data prep: download HUD LMI layers for Virginia (STATE = '51') and cache as RDS.
# Run: Rscript R/fetch_data.R
suppressPackageStartupMessages({library(httr2); library(sf)})

BG_URL    <- "https://services.arcgis.com/VTyQ9soqVukalItT/arcgis/rest/services/LOW_MOD_INCOME_BY_BG/FeatureServer/0/query"
TRACT_URL <- "https://services.arcgis.com/VTyQ9soqVukalItT/arcgis/rest/services/Low_to_Moderate_Income_Population_by_Tract/FeatureServer/4/query"

fetch_layer <- function(url, where = "STATE='51'", page_size = 1000,
                        out_fields = "*") {
  pages <- list(); offset <- 0
  repeat {
    resp <- request(url) |>
      req_url_query(outFields = out_fields, where = where, f = "geojson",
                    resultOffset = offset, resultRecordCount = page_size,
                    outSR = 4326) |>
      req_retry(max_tries = 4) |> req_timeout(180) |> req_perform()
    txt <- resp_body_string(resp)
    sfp <- sf::read_sf(txt, quiet = TRUE)
    n <- nrow(sfp)
    message(sprintf("  offset %d: %d features", offset, n))
    if (n > 0) pages[[length(pages) + 1]] <- sfp
    exceeded <- grepl('"exceededTransferLimit": *true', txt)
    if (n == 0 || !exceeded) break
    offset <- offset + n
  }
  out <- do.call(rbind, pages)
  # The block group layer names its column "State"; tract layer uses "STATE".
  geom <- attr(out, "sf_column")
  names(out)[names(out) != geom] <- toupper(names(out)[names(out) != geom])
  # Defensive client-side filter as well
  out <- out[as.character(out$STATE) == "51", ]
  out <- st_make_valid(st_transform(out, 4326))
  out
}

if (sys.nframe() == 0) {
  dir.create("data", showWarnings = FALSE)
  message("Fetching block groups...")
  bg <- fetch_layer(BG_URL)
  message("Fetching tracts...")
  tr <- fetch_layer(TRACT_URL)
  # BG layer stores the percentage as a fraction (0.171) in "Lowmod_pct";
  # tract layer stores LOWMODPCT as a percent (47.82). Normalize both to percent.
  bg$LOWMODPCT <- round(as.numeric(bg$LOWMOD_PCT) * 100, 2)
  tr$LOWMODPCT <- as.numeric(tr$LOWMODPCT)
  keep_bg <- c("GEOID","GEONAME","COUNTYNAME","STATE","LOWMOD","LOWMODUNIV","LOWMODPCT")
  keep_tr <- c("GEOID","STATE","COUNTY","LOWMOD","LOWMODUNIV","LOWMODPCT")
  bg <- bg[, intersect(keep_bg, names(bg))]
  tr <- tr[, intersect(keep_tr, names(tr))]
  print(summary(bg$LOWMODPCT)); print(summary(tr$LOWMODPCT))
  saveRDS(bg, "data/lmi_bg_va.rds")
  saveRDS(tr, "data/lmi_tract_va.rds")
  message(sprintf("Saved %d block groups, %d tracts", nrow(bg), nrow(tr)))
}
