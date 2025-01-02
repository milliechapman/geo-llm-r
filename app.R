library(shiny)
library(bslib)
library(markdown)
library(shinychat)
library(mapgl)
library(tidyverse)
library(duckdbfs)

pmtiles <- "https://data.source.coop/cboettig/us-boundaries/mappinginequality.pmtiles"

# Define the UI
ui <- page_sidebar(

  sidebar = sidebar(

    textAreaInput("chat", 
      "Ask me a question!", 
      value = "Which state has the highest average social vulnerability?",
      width = "100%",
      height = 100
    ),

    verbatimTextOutput("sql_code"),
    textOutput("explanation"),

    input_switch("redlines", "Redlined Areas"),
    input_switch("svi", "Social Vulnerability", value = TRUE),
    input_switch("richness", "Biodiversity Richness"),
    input_switch("rsr", "Biodiversity Range Size Rarity"),
  width = 300,
  ),
  titlePanel("Demo App"),

  layout_columns(
    card(maplibreOutput("map")),
    card(includeMarkdown("## Plot"),
         plotOutput("chart1"),
         plotOutput("chart2"),
         ),

    col_widths = c(8,4)
  ),

  tableOutput("table")

)

svi <- "https://data.source.coop/cboettig/social-vulnerability/svi2020_us_tract.parquet" |>
      open_dataset(tblname = "svi")

con <- duckdbfs::cached_connection()
schema <- DBI::dbGetQuery(con, "PRAGMA table_info(svi)")
#schema <- svi |> head() |> collect() |> str()

system_prompt = glue::glue('
You are a helpful agent who always replies strictly in JSON-formatted text.
Your task is to translate the users question into a SQL query that will be run
against the "svi" table in a duckdb database. The duckdb database has a
spatial extension which understands PostGIS operations as well.

The table schema is <schema>

The column called "RPL_THEMES" corresponds to the overall "Social vulnerability index" number. 

Format your answer as follows:

{
"query": "your raw SQL response goes here",
"explanation": "your explanation of the query"
}
', .open = "<", .close = ">")

# Define the server
server <- function(input, output, session) {

  chat <- ellmer::chat_vllm(
    base_url = "https://llm.nrp-nautilus.io/",
    model = "llama3",
    api_key = Sys.getenv("NRP_API_KEY"),
    system_prompt = system_prompt
  )

  observeEvent(input$chat, {
    stream <- chat$chat(input$chat)

    chat_append("chat", stream)
    response <- jsonlite::fromJSON(stream)

    output$sql_code <- renderText({response$query})
    output$explanation <- renderText({response$explanation})

    df <- DBI::dbGetQuery(con, response$query)
    output$table <- renderTable(df, striped = TRUE)

  })

  output$map <- renderMaplibre({
    m <- maplibre(center=c(-92.9, 41.3), zoom=3)

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
  m})


    chart1 <-
      svi |>
      filter(RPL_THEMES > 0) |>
      group_by(COUNTY) |>
      summarise(mean_svi = mean(RPL_THEMES)) |>
      collect() |>
      ggplot(aes(mean_svi)) + geom_density()

  output$chart1 <- renderPlot(chart1)
  output$chart2 <- renderPlot(chart1)

}

# Run the app
shinyApp(ui = ui, server = server)