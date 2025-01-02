library(shiny)
library(bslib)
library(mapgl)

pmtiles <- "https://data.source.coop/cboettig/us-boundaries/mappinginequality.pmtiles"

# Define the UI
ui <- page_sidebar(

  titlePanel("Demo App"),
  sidebar = sidebar(
    input_switch("redlines", "Redlined Areas"),
    input_switch("svi", "Social Vulnerability", value = TRUE),
    input_switch("richness", "Biodiversity Richness"),
    input_switch("rsr", "Biodiversity Range Size Rarity"),

    ),
    # Create a main panel
  card(full_screen = TRUE,
      maplibreOutput("map")

    )
  )

# Define the server
server <- function(input, output, session) {
  output$map <- renderMaplibre({

    m <- maplibre(center=c(-92.9, 41.3), zoom=4)

    if (input$redlines) {
      m <- m |>
        add_fill_layer(
          id = "redlines",
          source = list(type = "vector",
                        url = paste0("pmtiles://", pmtiles)),
          source_layer = "mappinginequality",
          fill_color = list("get", "fill")
        )
    }
    if (input$richness) {
      m <- m |>
        add_raster_source(id = "richness",
                          tiles = "https://data.source.coop/cboettig/mobi/tiles/red/species-richness-all/{z}/{x}/{y}.png",
                          maxzoom = 11
                          ) |>
        add_raster_layer(id = "richness-layer", 
                         source = "richness")

    }

     if (input$rsr) {
      m <- m |>
        add_raster_source(id = "rsr",
                          tiles = "https://data.source.coop/cboettig/mobi/tiles/green/range-size-rarity-all/{z}/{x}/{y}.png",
                          maxzoom = 11
                          ) |>
        add_raster_layer(id = "richness-layer",
                         source = "rsr")

    }
    if (input$svi) {
      m <- m |>
        add_fill_layer(
          id = "redlines",
          source = list(type = "vector",
                        url = paste0("pmtiles://", "https://data.source.coop/cboettig/social-vulnerability/svi2020_us_tract.pmtiles")),
          source_layer = "SVI2000_US_tract",
          fill_opacity = 0.5,
          fill_color = interpolate(column = "RPL_THEMES",
                                  values = c(0, 1),
                                  stops = c("lightblue", "darkblue"),
                                  na_color = "lightgrey")
        )
    }





    m
  })


}

# Run the app
shinyApp(ui = ui, server = server)