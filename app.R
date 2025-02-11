library(shiny)
library(bslib)
library(htmltools)
library(markdown)
library(fontawesome)
library(bsicons)
library(gt)
library(glue)
library(ggplot2)

library(mapgl)
library(dplyr)
library(duckdbfs)

duckdbfs::load_spatial()

css <- HTML("<link rel='stylesheet' type='text/css' href='https://demos.creative-tim.com/material-dashboard/assets/css/material-dashboard.min.css?v=3.2.0'>")


# Define the UI
ui <- page_sidebar(
  fillable = FALSE, # do not squeeze to vertical screen space
  tags$head(css),
  titlePanel("Demo App"),

"This is a proof-of-principle for a simple chat-driven interface to dynamically explore geospatial data. 
 ",


  card(
    layout_columns(
    textInput("chat",
      label = NULL,
      "Which counties in California have the highest average social vulnerability?",
      width = "100%"),
    div(
    actionButton("user_msg", "", icon = icon("paper-plane"),
                 class = "btn-primary btn-sm align-bottom"),
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
    col_widths = c(8, 4),
    row_heights = c("600px"),
    max_height = "700px"
  ),


  gt_output("table"),

  card(fill = TRUE,
    card_header(fa("robot")),
 
    accordion(
      open = FALSE,
      accordion_panel(
        title = "show sql",
        icon = fa("terminal"),
        verbatimTextOutput("sql_code"),
      ),
      accordion_panel(
        title = "explain",
        icon = fa("user", prefer_type="solid"),
        textOutput("explanation"),
      )
    ),

    card(
      card_header("Errata"),
      markdown(
"
#### Credits

Developed by Carl Boettiger, UC Berkeley, 2025.  BSD License.

Data from the US Census and CDC's [Social Vulnerability Index](https://www.atsdr.cdc.gov/place-health/php/svi/index.html)

#### Technical details

The app is written entirely in R using shiny. The app will translate natural language queries in SQL code using
a small open-weights language model. The SQL code is executed using the duckdb backend against cloud-native
geoparquet snapshot of the Social Vulnerability Index hosted on Source Cooperative. Summary chart data are also
computed in duckdb by streaming, providing responsive updates while needing minimal RAM or disk storage despite
the large size of the data sources. 

The map is rendered and updated using MapLibre with PMTiles, which provides responsive rendering for large feature sets.
The PMTiles layer is also hosted on Source cooperative where it can be streamed efficiently.
")
    )

  ),

  sidebar = sidebar(

    input_switch("redlines", "Redlined Areas", value = FALSE),
    input_switch("svi", "Social Vulnerability", value = TRUE),
    input_switch("richness", "Biodiversity Richness", value = FALSE),
    input_switch("rsr", "Biodiversity Range Size Rarity", value = FALSE),

    card(
      card_header(bs_icon("github"), "Source code:"),
      a(href = "https://github.com/boettiger-lab/geo-llm-r",
        "https://github.com/boettiger-lab/geo-llm-r"))
  ),

  theme = bs_theme(version = "5")
)




repo <- "https://data.source.coop/cboettig/social-vulnerability"
pmtiles <- glue("{repo}/svi2020_us_tract.pmtiles")
parquet <- glue("{repo}/svi2020_us_tract.parquet")
svi <- open_dataset(parquet, tblname = "svi") |>
  filter(RPL_THEMES > 0)


con <- duckdbfs::cached_connection()
schema <- DBI::dbGetQuery(con, "PRAGMA table_info(svi)")

system_prompt = glue::glue('
You are a helpful agent who always replies strictly in JSON-formatted text.
Your task is to translate the users question into a SQL query that will be run
against the "svi" table in a duckdb database. The duckdb database has a
spatial extension which understands PostGIS operations as well. 
Include semantically meaningful columns like COUNTY and STATE name.

In the data, each row represents an individual census tract. If asked for 
county or state level statistics, be sure to aggregate across all the tracts
in that county or state. 

The table schema is <schema>

The column called "RPL_THEMES" corresponds to the overall "Social vulnerability index" number. 

Format your answer as follows:

{
"query": "your raw SQL response goes here",
"explanation": "your explanation of the query"
}
', .open = "<", .close = ">")

chat <- ellmer::chat_vllm(
  base_url = "https://llm.nrp-nautilus.io/",
  model = "llama3",
  api_key = Sys.getenv("NRP_API_KEY"),
  system_prompt = system_prompt,
  api_args = list(temperature = 0)
)

# helper utilities
# faster/more scalable to pass maplibre the ids to refilter pmtiles,
# than to pass it the full geospatial/sf object
filter_column <- function(full_data, filtered_data, id_col = "FIPS") {
  if (nrow(filtered_data) < 1) return(NULL)
  values <- full_data |>
    inner_join(filtered_data, copy = TRUE) |>
    pull(id_col)
  # maplibre syntax for the filter of PMTiles  
  list("in", list("get", id_col), list("literal", values))
}

chart1_data <- svi |>
  group_by(COUNTY) |>
  summarise(mean_svi = mean(RPL_THEMES)) |>
  collect()

chart1 <- chart1_data |>
  ggplot(aes(mean_svi)) + geom_density(fill="darkred") +
  ggtitle("County-level vulnerability nation-wide")


# Define the server
server <- function(input, output, session) {
  data <- reactiveValues(df = tibble())
  output$chart1 <- renderPlot(chart1)

  observeEvent(input$user_msg, {
    stream <- chat$chat(input$chat)

    # optional, remember previous discussion
    #chat_append("chat", stream)

    # Parse response
    response <- jsonlite::fromJSON(stream)
    output$sql_code <- renderText(stringr::str_wrap(response$query, width = 60))
    output$explanation <- renderText(response$explanation)

    # Actually execute the SQL query generated:
    df <- DBI::dbGetQuery(con, response$query)

    # don't display shape column in render
    df <- df |> select(-any_of("Shape"))
    output$table <- render_gt(df, height = 300)


    y_axis <- colnames(df)[!colnames(df) %in% colnames(svi)] 
    chart2 <- df |> 
      rename(social_vulnerability = y_axis) |>
      ggplot(aes(social_vulnerability)) +
      geom_density(fill = "darkred")  +
      xlim(c(0, 1)) +
      ggtitle("Vulnerability of selected areas")

    output$chart2 <- renderPlot(chart2)

    # We need to somehow trigger this df to update the map.
    data$df <- df

  })



  output$map <- renderMaplibre({

    m <- maplibre(center = c(-92.9, 41.3), zoom = 3, height = "400")
    if (input$redlines) {
      m <- m |>
        add_fill_layer(
          id = "redlines",
          source = list(type = "vector",
                        url = paste0("pmtiles://", "https://data.source.coop/cboettig/us-boundaries/mappinginequality.pmtiles")),
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
          id = "svi_layer",
          source = list(type = "vector",
                        url = paste0("pmtiles://", pmtiles)),
          source_layer = "SVI2000_US_tract",
          filter = filter_column(svi, data$df, "FIPS"),
          fill_opacity = 0.5,
          fill_color = interpolate(column = "RPL_THEMES",
                                  values = c(0, 1),
                                  stops = c("lightpink", "darkred"),
                                  na_color = "lightgrey")
        )
    }
  m})



}

# Run the app
shinyApp(ui = ui, server = server)