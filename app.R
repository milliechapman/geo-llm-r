library(shiny)
library(bslib)
library(mapgl)

pmtiles <- "https://data.source.coop/cboettig/us-boundaries/mappinginequality.pmtiles"
ui <- page_sidebar(
  title = "mapgl with Shiny",
  sidebar = sidebar(),
  card(
    full_screen = TRUE,
    maplibreOutput("map")
  )
)

server <- function(input, output, session) {
  output$map <- renderMaplibre({

    maplibre(center=c(-72.9, 41.3), zoom=10) |>
      add_fill_layer(
        id = "redlines",
        source = list(type = "vector",
                      url = paste0("pmtiles://", pmtiles)),
        source_layer = "mappinginequality",
        fill_color = list("get", "fill")
      )
  })


}

shinyApp(ui, server)
