library(shiny)
library(bslib)
library(markdown)
library(shinychat)
library(mapgl)
library(tidyverse)
library(duckdbfs)
library(fontawesome)
library(bsicons)
library(gt)
library(htmltools)

duckdbfs::load_spatial()

css <- HTML("<link rel='stylesheet' type='text/css' href='https://demos.creative-tim.com/material-dashboard/assets/css/material-dashboard.min.css?v=3.2.0'>")

pmtiles <- "https://data.source.coop/cboettig/us-boundaries/mappinginequality.pmtiles"

# Define the UI
ui <- page_sidebar(
  tags$head(css),
  titlePanel("Demo App"),
  card(
    layout_columns(
    textInput("chat",
      label = NULL,
      "Which county has the highest average social vulnerability?",
      width = "100%"),
    div(
    actionButton("user_msg", "", icon = icon("paper-plane"),  class = "btn-primary btn-sm align-bottom"),
    class = "align-text-bottom"),
    col_widths = c(11, 1)),
    fill = FALSE
  ),
  layout_columns(
    card(maplibreOutput("map")),
    card(includeMarkdown("## Plot"),
         plotOutput("chart1"),
         plotOutput("chart2"),
         ),

    col_widths = c(8, 4)
  ),

  gt_output("table"),


  card(fill = FALSE,
    layout_columns(
    br(),
    accordion(
      open = FALSE,
      accordion_panel("generated SQL Code",
        verbatimTextOutput("sql_code"),
      ),
      accordion_panel("Explanation",
        textOutput("explanation"),
      )
    ), 
    br(),
    col_widths = c(2, 8, 2)
    )
  ),

  sidebar = sidebar(

    input_switch("redlines", "Redlined Areas", value = FALSE),
    input_switch("svi", "Social Vulnerability", value = FALSE),
    input_switch("richness", "Biodiversity Richness", value = FALSE),
    input_switch("rsr", "Biodiversity Range Size Rarity", value = FALSE),
#  width = 350,
  ),
  theme = bs_theme(version = "5")
)

svi <- "https://data.source.coop/cboettig/social-vulnerability/svi2020_us_tract.parquet" |>
      open_dataset(tblname = "svi")

con <- duckdbfs::cached_connection()
schema <- DBI::dbGetQuery(con, "PRAGMA table_info(svi)")

system_prompt = glue::glue('
You are a helpful agent who always replies strictly in JSON-formatted text.
Your task is to translate the users question into a SQL query that will be run
against the "svi" table in a duckdb database. The duckdb database has a
spatial extension which understands PostGIS operations as well. 


Be careful to limit any return to no more than 50 rows. 

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

  observeEvent(input$user_msg, {
    stream <- chat$chat(input$chat)

    chat_append("chat", stream)
    response <- jsonlite::fromJSON(stream)

    output$sql_code <- renderText({stringr::str_wrap(response$query, width = 60)})
    output$explanation <- renderText(response$explanation)

    df <- DBI::dbGetQuery(con, response$query)

    df <- df |> select(-any_of("Shape"))
    output$table <- render_gt(df, height = 300)

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