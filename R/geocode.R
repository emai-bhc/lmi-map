# US Census Bureau geocoder (no API key).
# Returns list(ok, lng, lat, matched, error)
suppressPackageStartupMessages(library(httr2))

geocode_address <- function(address) {
  address <- trimws(address)
  if (!nzchar(address)) return(list(ok = FALSE, error = "Please enter an address."))
  res <- tryCatch({
    resp <- request("https://geocoding.geo.census.gov/geocoder/locations/onelineaddress") |>
      req_url_query(address = address, benchmark = "Public_AR_Current", format = "json") |>
      req_retry(max_tries = 3) |> req_timeout(30) |> req_perform()
    resp_body_json(resp)
  }, error = function(e) e)
  if (inherits(res, "error"))
    return(list(ok = FALSE, error = "The geocoding service could not be reached. Please try again."))
  m <- res$result$addressMatches
  if (length(m) == 0)
    return(list(ok = FALSE, error = "No match found for that address. Include street, city and VA."))
  m <- m[[1]]
  list(ok = TRUE, lng = m$coordinates$x, lat = m$coordinates$y, matched = m$matchedAddress)
}
