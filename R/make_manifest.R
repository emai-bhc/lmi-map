# Generate manifest.json for Posit Connect. Run: Rscript R/make_manifest.R
# Requires data/*.rds to exist (run R/fetch_data.R first).
stopifnot(file.exists("data/lmi_bg_va.rds"), file.exists("data/lmi_tract_va.rds"))
rsconnect::writeManifest(
  appDir = ".",
  appPrimaryDoc = "app.R",
  appFiles = c("app.R", "R/lookup.R", "R/geocode.R",
               "data/lmi_bg_va.rds", "data/lmi_tract_va.rds",
               "www/bhc.css", list.files("www/img", full.names = TRUE))
)
