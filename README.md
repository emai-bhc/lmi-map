# Virginia LMI Address Lookup

R Shiny app (built with mapgl) that reports whether a Virginia address, or a point clicked on the map, is in a low-to-moderate income (LMI) area. It uses two HUD layers (block groups and tracts) filtered to Virginia (STATE = "51"). An area is LMI when LOWMODPCT >= 51. Results for both layers appear in a side panel and in the marker popup.

## Data note
The block group layer has no LOWMODPCT column. It stores `Lowmod_pct` as a fraction (0.171) and names the state column `State`. `R/fetch_data.R` normalizes it to a 0-100 `LOWMODPCT` so both layers share one scale.

## Files
- `app.R`: Shiny app
- `R/lookup.R`: point-in-polygon and LMI classification
- `R/geocode.R`: US Census geocoder (no API key)
- `R/fetch_data.R`: downloads and caches HUD data to `data/*.rds` (`Rscript R/fetch_data.R`)
- `R/make_manifest.R`: writes `manifest.json` for Posit Connect
- `tests/`: testthat tests (`NOT_CRAN=true Rscript -e "testthat::test_dir(\"tests/testthat\")"`)

## Deploy to Posit Connect
1. `Rscript R/fetch_data.R` (optional, refreshes data)
2. `Rscript R/make_manifest.R` (re-run after any change to app files or packages)
3. Deploy via git-backed deployment or `rsconnect::deployApp()`.

## Address autocomplete
The address box suggests Virginia street addresses as you type, using the free Photon service (OpenStreetMap data, no API key), limited to the Virginia bounding box and filtered to results in Virginia. Photon is called from the user's browser and is subject to its fair-use limits; for heavy traffic, swap in a keyed provider in `photon_js` in app.R. A typed address that is not in the list is searched with the Census geocoder when you press Enter.

## Branding
Styled to the Better Housing Coalition Brand Identity Guide: BHC orange (#E86C1F) with the guide's supporting palette, Lato headings, Lora body text, the reversed (white) horizontal logo on the orange header, and the tagline in the header and footer. Logos live in `www/img/` and styles in `www/bhc.css`.
