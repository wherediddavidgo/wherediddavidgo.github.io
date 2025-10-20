# app.R
library(shiny)
library(leaflet)
library(dplyr)
library(htmltools)

# ---- Example data (replace with your own) ----
places <- tibble::tribble(
  ~id, ~name,                ~lat,     ~lon,      ~zoom, ~info_html,
  "a",  "University of North Carolina",    35.907716,  -79.052342,     12,  "BS in Geological Sciences, 2024.",
  "b",  "Virginia Tech",  37.229181, -80.425414,     11,  "2nd year PhD Student in Department of Geosciences.",
  "c",  "UNC Institute of Marine Sciences",  34.723495, -76.751800,     12,  "I spent summer 2022 here working as an 
  undergraduate lab tech with Dr. Tony Rodriguez and PhD Student Josh Himmelstein. I helped to build and deploy OpenOBS turbidity loggers on salt marshes to understand 
  how sediment transport across marshes is affected by land use change and sea level rise.",
  "d",  "Washington Canyon", 37.449106, -74.486553,     8,  "In August 2022 I joined the research groups of Drs. Chris Martens (UNC) and Karen LLoyd (UTK) on a 10-day research cruise
  to characterize the fate of methane in the ocean at cold seeps. We deployed benthic landers with experimental incubators and methane concentration loggers for several multi-day experimental runs
  and collected seafloor sediment and pore water samples for genomic analysis of the microbial communities surrounding cold seeps.",
  "e",  "Northern New Mexico",  35.502912, -106.841215,     8,  "I completed a 4-week geology field camp through NC State in summer 2023. While in New Mexico, we mapped geologic structures and river terraces 
  and made stratigraphic columns to interpret depositional history. We also learned how to use ground conductivity to image the subsurface, and our results from the project have been 
  used by local tribes to inform conservation efforts.",
  "f",  "Jordan Lake", 35.744023,  -79.008481,     12,  "For my senior thesis, I designed and built an inexpensive, open-source sonde to measure 
  conductivity, temperature, depth, turbidity, and dissolved oxygen to reduce financial barriers to oceanography and environmental science. I conducted 2 field tests in Jordan Lake. Read more 
  at this link: https://github.com/wherediddavidgo/ctdTURBO.",
  "g",  "Owens River Valley",  37.360912,  -118.326598,     12,  "In Fall 2023, I served as the undergraduate research mentor for the first-year seminar EMES 72H: Field Geology of Eastern 
  California. This course entails a rapid introduction to geology, a week-long field trip to the Eastern Sierra Nevada during which the students conduct field exercises and collect 
  data for research projects, and culminates with poster presentations of the research projects. I was tasked with helping the students in the field and teaching them to analyze their data 
  using Excel and QGIS afterward. Field exercises encompassed topics in glacial geomorphology, igneous petrology, and structural geology. For the research projects, students characterized the 
  performance of the SWOT satellite's measurements of elevation and areal extent for small lakes and rivers."
  
  
)

choices <- setNames(places$id, places$name)  # show names, store ids

# ---- UI ----
ui <- fluidPage(
  titlePanel("Interactive CV"),
  tags$style(HTML("
    .sidebar { max-height: 75vh; overflow-y: auto; }
    #details-box { border: 1px solid #ddd; border-radius: 8px; padding: 12px; }
  ")),
  fluidRow(
    column(
      width = 3,
      div(class = "sidebar",
          h4("Locations"),
          radioButtons(
            inputId = "loc",
            label = NULL,
            choices = choices,
            selected = names(choices)[1]
          ),
          actionButton("btn_fit_all", "Fit to All")
      )
    ),
    column(
      width = 9,
      leafletOutput("map", height = 500),
      br(),
      div(id = "details-box",
          htmlOutput("details")
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  # Initial map
  output$map <- renderLeaflet({
    leaflet(places) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addCircleMarkers(
        ~lon, ~lat,
        layerId = ~id, label = ~name,
        radius = 6, weight = 2, fillOpacity = 0.9
      ) |>
      fitBounds(
        lng1 = min(places$lon), lat1 = min(places$lat),
        lng2 = max(places$lon), lat2 = max(places$lat)
      )
  })
  
  # Helper: get selected row
  get_selected <- reactive({
    req(input$loc)
    places %>% filter(id == input$loc) %>% slice(1)
  })
  
  # Highlight selection on map + zoom
  observeEvent(input$loc, {
    sel <- get_selected()
    leafletProxy("map") |>
      clearGroup("selected") |>
      addCircleMarkers(sel$lon, sel$lat,
                       radius = 10, color = "#2C7FB8",
                       weight = 3, fillOpacity = 0.2,
                       group = "selected") |>
      setView(lng = sel$lon, lat = sel$lat, zoom = sel$zoom)
  }, ignoreInit = FALSE)
  
  # Clicking a marker updates the selection and details
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    if (!is.null(click$id) && click$id %in% places$id) {
      updateRadioButtons(session, "loc", selected = click$id)
    }
  })
  
  # Details panel
  output$details <- renderUI({
    sel <- get_selected()
    tagList(
      h4(sel$name),
      HTML(sel$info_html),
      tags$hr(),
      # tags$p(
      #   tags$b("Coordinates: "), sprintf("%.4f, %.4f", sel$lat, sel$lon), br(),
      #   tags$b("Suggested zoom: "), sel$zoom
      # )
    )
  })
  
  # Fit to all button
  observeEvent(input$btn_fit_all, {
    leafletProxy("map") |>
      clearGroup("selected") |>
      fitBounds(
        lng1 = min(places$lon), lat1 = min(places$lat),
        lng2 = max(places$lon), lat2 = max(places$lat)
      )
  })
}

shinyApp(ui, server)
