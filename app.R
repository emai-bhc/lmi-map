library(shiny)
library(mapgl)
library(sf)
source("R/lookup.R")
source("R/geocode.R")

# ---- Data (cached by R/fetch_data.R) ---------------------------------------
bg    <- readRDS("data/lmi_bg_va.rds")
tract <- readRDS("data/lmi_tract_va.rds")

# Simplified copies for drawing only (lookups use full-resolution geometry)
simplify_display <- function(x, tol = 0.0004) {
  old <- sf_use_s2()
  suppressMessages(sf_use_s2(FALSE))
  on.exit(suppressMessages(sf_use_s2(old)))
  x <- suppressWarnings(st_simplify(x, preserveTopology = TRUE, dTolerance = tol))
  x <- x[!st_is_empty(x), c("GEOID", "LOWMODPCT")]
  x$status <- ifelse(is.na(x$LOWMODPCT), "No data",
                     ifelse(x$LOWMODPCT >= LMI_THRESHOLD, "LMI (>= 51%)", "Not LMI"))
  x$popup <- sprintf("<b>%s</b><br>Low/mod income: %s%%", x$GEOID, x$LOWMODPCT)
  x
}
bg_disp    <- simplify_display(bg)
tract_disp <- simplify_display(tract, 0.001)

status_vals   <- c("LMI (>= 51%)", "Not LMI", "No data")
status_colors <- c("#E86C1F", "#ADE0ED", "#CCCCCC")  # BHC orange / light blue / grey
fill_expr <- match_expr("status", values = status_vals, stops = status_colors,
                        default = "#cccccc")

# ---- Address autocomplete (Photon, OSM-based, no API key) -------------------
# Runs in the browser. Restricted to the Virginia bounding box and filtered to
# results whose state is Virginia. Option value is "label|lng|lat".
photon_js <- "function(query, callback) {
  if (!query || query.length < 4) return callback();
  var url = 'https://photon.komoot.io/api/?lang=en&limit=10&q=' +
    encodeURIComponent(query) + '&bbox=-83.68,36.54,-75.17,39.47' +
    '&lat=37.5&lon=-78.5';
  fetch(url).then(function(r) { return r.json(); }).then(function(j) {
    var seen = {}, out = [];
    (j.features || []).forEach(function(f) {
      var p = f.properties || {};
      if (p.countrycode !== 'US' || p.state !== 'Virginia') return;
      if (!p.street && !p.housenumber) return;
      var street = ((p.housenumber || '') + ' ' + (p.street || p.name || '')).trim();
      var label = [street, p.city || p.county, 'VA', p.postcode].filter(Boolean).join(', ');
      if (seen[label]) return; seen[label] = true;
      var c = f.geometry.coordinates;
      out.push({label: label, value: label + '|' + c[0] + '|' + c[1]});
    });
    callback(out);
  }).catch(function() { callback(); });
}"

# ---- UI ---------------------------------------------------------------------
ui <- fluidPage(
  title = "LMI Address Lookup | Better Housing Coalition",
  tags$head(
    tags$link(rel = "icon", type = "image/png", href = "img/bhc-logo-image-only-rgb.png"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet",
              href = paste0("https://fonts.googleapis.com/css2?family=Lato:wght@400;700;900",
                            "&family=Lora:wght@400;700&display=swap")),
    tags$link(rel = "stylesheet", href = "bhc.css")
  ),
  # Header: reversed (white) BHC logo on brand orange
  div(class = "bhc-header",
      tags$img(class = "bhc-logo", src = "img/bhc-logo-horiz-white.png",
               alt = "Better Housing Coalition"),
      div(h1(class = "bhc-title", "Low-to-Moderate Income Address Lookup"),
          p(class = "bhc-tagline", "Virginia | Better Homes. Better Communities. Better Lives."))),
  div(class = "bhc-body", sidebarLayout(
    sidebarPanel(width = 4,
      selectizeInput("address", "Virginia address", choices = NULL, multiple = FALSE,
                     options = list(
                       placeholder = "Start typing, e.g. 1111 E Main St, Richmond",
                       valueField = "value", labelField = "label",
                       searchField = "label", maxItems = 1,
                       create = TRUE, createOnBlur = FALSE, persist = FALSE,
                       loadThrottle = 350, openOnFocus = FALSE,
                       # Suggestions are never re-filtered client side
                       score = I("function(search) { return function() { return 1; }; }"),
                       render = I("{ option_create: function(data, escape) {
                         return '<div class=\"create\">Search for <strong>' +
                           escape(data.input) + '</strong></div>'; } }"),
                       load = I(photon_js)
                     )),
      helpText("Pick a suggestion, or press Enter to search what you typed. ",
               "You can also click anywhere on the map to drop a marker."),
      hr(),
      uiOutput("result")
    ),
    mainPanel(width = 8, maplibreOutput("map", height = "78vh"))
  )),
  div(class = "bhc-footer",
      p(class = "bhc-footer-tag", "Better Homes. Better Communities. Better Lives."),
      p("Data: HUD low-to-moderate income block group and tract layers for Virginia (STATE = 51). ",
        "An area is LMI when LOWMODPCT is 51 or above."),
      p("Better Housing Coalition changes lives and transforms communities through ",
        "high-quality, affordable housing."))
)

# ---- Server -----------------------------------------------------------------
server <- function(input, output, session) {
  result <- reactiveVal(NULL)

  output$map <- renderMaplibre({
    maplibre(style = carto_style("positron"), center = c(-79.4, 37.9), zoom = 6.3) |>
      add_fill_layer(id = "tracts", source = tract_disp, fill_color = fill_expr,
                     fill_opacity = 0.45, popup = "popup", max_zoom = 9) |>
      add_fill_layer(id = "bgs", source = bg_disp, fill_color = fill_expr,
                     fill_opacity = 0.45, popup = "popup", min_zoom = 9) |>
      add_categorical_legend("LMI status (LOWMODPCT)",
                             values = status_vals, colors = status_colors)
  })

  place_and_check <- function(lng, lat, label = NULL, fly = TRUE) {
    proxy <- maplibre_proxy("map")
    if (!in_virginia_bbox(lng, lat)) {
      result(list(error = "That location appears to be outside Virginia."))
      clear_markers(proxy)
      return(invisible())
    }
    b <- lookup_point(lng, lat, bg)
    t <- lookup_point(lng, lat, tract)
    d <- describe_result(b, t)
    result(c(d, list(label = label, lng = lng, lat = lat)))
    popup_html <- sprintf("<b>%s</b><br>%s<br>%s<br>%s",
                          if (is.null(label)) sprintf("%.5f, %.5f", lat, lng) else label,
                          d$headline, d$bg_line, d$tract_line)
    p <- proxy |> clear_markers() |>
      add_markers(data = c(lng, lat), popup = popup_html,
                  color = "#00617F")  # BHC teal: stands out against orange/blue fills
    if (fly) fly_to(p, center = c(lng, lat), zoom = 12)
    invisible()
  }

  observeEvent(input$address, {
    req(nzchar(input$address))
    parts <- strsplit(input$address, "|", fixed = TRUE)[[1]]
    if (length(parts) == 3) {
      # Picked a suggestion: refine with the Census geocoder, fall back to
      # the suggestion's own coordinates.
      g <- geocode_address(parts[1])
      if (isTRUE(g$ok) && in_virginia_bbox(g$lng, g$lat)) {
        place_and_check(g$lng, g$lat, label = g$matched)
      } else {
        place_and_check(as.numeric(parts[2]), as.numeric(parts[3]), label = parts[1])
      }
    } else {
      g <- geocode_address(input$address)
      if (!isTRUE(g$ok)) { result(list(error = g$error)); return() }
      place_and_check(g$lng, g$lat, label = g$matched)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$map_click, {
    cl <- input$map_click
    place_and_check(cl$lng, cl$lat, label = NULL, fly = FALSE)
  })

  output$result <- renderUI({
    r <- result()
    if (is.null(r)) return(helpText("Enter an address or click the map."))
    if (!is.null(r$error)) return(div(class = "bhc-card error", r$error))
    cls <- if (r$none_found) "none" else if (r$lmi) "lmi" else "notlmi"
    div(class = paste("bhc-card", cls),
        if (!is.null(r$label)) tags$p(tags$b(r$label)),
        tags$p(class = "bhc-headline", r$headline), tags$p(r$bg_line), tags$p(r$tract_line))
  })
}

shinyApp(ui, server)
