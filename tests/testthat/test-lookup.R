root <- normalizePath(file.path("..", ".."))
source(file.path(root, "R", "lookup.R"))
source(file.path(root, "R", "geocode.R"))

mk_layer <- function(pcts) {
  polys <- lapply(seq_along(pcts), function(i) {
    x0 <- i - 1
    st_polygon(list(rbind(c(x0, 0), c(x0 + 1, 0), c(x0 + 1, 1), c(x0, 1), c(x0, 0))))
  })
  st_sf(GEOID = paste0("G", seq_along(pcts)), LOWMODPCT = pcts,
        geometry = st_sfc(polys, crs = 4326))
}

test_that("is_lmi uses a >= 51 threshold", {
  expect_true(is_lmi(51)); expect_true(is_lmi(80.2))
  expect_false(is_lmi(50.99)); expect_false(is_lmi(0))
  expect_true(is.na(is_lmi(NA)))
})

test_that("lookup_point finds the containing polygon", {
  lay <- mk_layer(c(60, 30))
  a <- lookup_point(0.5, 0.5, lay)
  expect_true(a$found); expect_true(a$lmi); expect_equal(a$geoid, "G1")
  b <- lookup_point(1.5, 0.5, lay)
  expect_true(b$found); expect_false(b$lmi)
  c0 <- lookup_point(5, 5, lay)
  expect_false(c0$found); expect_true(is.na(c0$lmi))
})

test_that("describe_result handles all combinations", {
  yes <- list(found = TRUE, pct = 60, lmi = TRUE, geoid = "A")
  no  <- list(found = TRUE, pct = 20, lmi = FALSE, geoid = "B")
  nf  <- list(found = FALSE, pct = NA_real_, lmi = NA, geoid = NA_character_)
  expect_true(describe_result(yes, no)$lmi)
  expect_true(describe_result(no, yes)$lmi)
  expect_false(describe_result(no, no)$lmi)
  expect_match(describe_result(no, no)$headline, "NOT located")
  expect_true(describe_result(nf, nf)$none_found)
})

test_that("Virginia bbox check", {
  expect_true(in_virginia_bbox(-77.43, 37.54))
  expect_false(in_virginia_bbox(-122.4, 37.8))
  expect_false(in_virginia_bbox(NA, 37))
})

test_that("cached data has expected shape (Virginia only, LOWMODPCT percent)", {
  skip_if_not(file.exists(file.path(root, "data", "lmi_bg_va.rds")))
  bg <- readRDS(file.path(root, "data", "lmi_bg_va.rds"))
  tr <- readRDS(file.path(root, "data", "lmi_tract_va.rds"))
  expect_true(all(bg$STATE == "51")); expect_true(all(tr$STATE == "51"))
  expect_true(all(startsWith(bg$GEOID, "51"))); expect_true(all(startsWith(tr$GEOID, "51")))
  expect_gt(max(bg$LOWMODPCT, na.rm = TRUE), 1)  # percent, not fraction
  # Richmond City Hall should land in both layers
  b <- lookup_point(-77.4335, 37.5407, bg); t <- lookup_point(-77.4335, 37.5407, tr)
  expect_true(b$found); expect_true(t$found)
  expect_true(startsWith(b$geoid, "51760"))
})

test_that("geocoder handles empty and live input", {
  expect_false(geocode_address("")$ok)
  skip_if_offline("geocoding.geo.census.gov")
  g <- geocode_address("1111 E Main St, Richmond, VA 23219")
  expect_true(g$ok)
  expect_true(in_virginia_bbox(g$lng, g$lat))
  expect_false(geocode_address("zzzz nowhere 00000 qqqq")$ok)
})
