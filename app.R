

library(shiny)
library(bslib)
library(tidyverse)

library(plotly)


source("anc.R")



themed <- bs_theme(
  # Controls the default grayscale palette
  bg = "white", fg = "black",
  # Controls the accent (e.g., hyperlink, button, etc) colors
  primary = "firebrick", secondary = "midnightblue",
  base_font = c("Gill Sans Pro", "sans-serif"),
  code_font = c("Courier", "monospace"),
  heading_font = "'Helvetica Neue', Helvetica, sans-serif",
  # Can also add lower-level customization
  "input-border-color" = "white"
)


# Define UI for application that draws a histogram
ui <- page_navbar(title = "LifeNet Uganda Dashboard",
                  underline = TRUE,fillable = FALSE,
                   theme = themed,
                  
           
                  nav_spacer(),
                  
                  
                  
                  nav_panel(title = "About the Dashboard",class = "bslib-page-dashboard"),
                  
                  
                  nav_panel(title = "Highlights"),
                  
                  
                  nav_panel("OPD Consultations"),
                  
                  
                  
                  nav_panel("ANC",anc_value_boxes,anc_cards, layout_columns(anc_distribution) ),
                  
                  nav_panel("Maternity",maternity,deliveries_dist),
                  
                  nav_panel("Nutrition"),
                  
                 
                  
                  
                  
                  
                  
                  
                  nav_item(
                    input_dark_mode(id = "dark_mode", mode = "light")
                  )

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  
  observeEvent(input$dark_mode, {
    if (input$dark_mode == "dark") {
      showNotification("You are in dark Mode!")
    }
    
    else {
      
      showNotification("You are in light Mode!")
      
    }
  })
  
  
  
  
  # Regions of Lifenet - ANC
  
  regions <- reactive({
    filter(combined_dataset, orgunitlevel2 == input$region)
  })
  
  observeEvent(regions(), {
    choices <- unique(regions()$orgunitlevel3)
    updateSelectInput(inputId = "district", choices = choices) 
  })
  
  
  
  
  district <- reactive({
    req(input$district)
    filter(regions(), orgunitlevel3 == input$district)
  })
  
  
  
  observeEvent(district(), {
    choices <- unique(district()$orgunitlevel5)
    
    updateSelectInput(inputId = "facility", choices = choices)
  })
  
  
  
  
  
  
  # Regions of Lifenet - Maternity
  
  regions_selected <- reactive({
    filter(combined_dataset, orgunitlevel2 == input$select_region)
  })
  
  observeEvent(regions_selected(), {
    choices <- unique(regions_selected()$orgunitlevel3)
    updateSelectInput(inputId = "select_district", choices = choices) 
  })
  
  
  
  
  select_district <- reactive({
    req(input$select_district)
    filter(regions_selected(), orgunitlevel3 == input$select_district)
  })
  
  
  
  observeEvent(select_district(), {
    choices <- unique(select_district()$orgunitlevel5)
    
    updateSelectInput(inputId = "select_facility", choices = choices)
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  anc_1consultations <- reactive({
    combined_dataset |> 
      select(date,`105-AN01a. ANC 1st Visit for women`,`105-AN01b. ANC 1st Visit for women (1st Trimester)`) |> 
      group_by(date) |> 
      summarise(total_anc_1_visitss = sum(`105-AN01a. ANC 1st Visit for women`,na.rm = TRUE),
                total_anc_1_firsts = sum(`105-AN01b. ANC 1st Visit for women (1st Trimester)`,na.rm = TRUE), .groups = "drop")
  })
  
  
  
  
  
  # Display charts
  output$anc_consultations_plot <- renderDygraph({
    dygraph(anc_1consultations()) |> 
      dySeries("total_anc_1_visitss",
                label = "Total ANC 1 Visits",
                color = "#09bc8a",
                strokeWidth = 2,
                drawPoints = TRUE, 
                pointSize = 4,
               pointShape = "circle",fillGraph = TRUE) |> 
      dySeries("total_anc_1_firsts",
               label = "Total ANC 1 Visits (First trimester)",
               color = "#004346",
               strokeWidth = 2,
               drawPoints = TRUE, pointSize = 4,
               # Set labels for x and y axes
               pointShape = "circle",
               fillGraph = TRUE)  |> 
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 0.2,highlightSeriesOpts = list(strokeWidth = 3),
                  hideOnMouseOut = TRUE)
             
  })
  
  
  
  
  
  
  
  
  
  
  anc_1consultations_group <- reactive({
    combined_dataset |> 
      select(date,orgunitlevel5, `105-AN01a. ANC 1st Visit for women`,`105-AN01b. ANC 1st Visit for women (1st Trimester)`) |> 
      group_by(date,orgunitlevel5) |> 
      summarise(total_anc1_visits = sum(`105-AN01a. ANC 1st Visit for women`,na.rm = TRUE),
                total_anc1_visits_first = sum(`105-AN01b. ANC 1st Visit for women (1st Trimester)`,na.rm = TRUE),.groups = "drop") |> 
      filter(orgunitlevel5 %in% input$facility)
  })
  
  
  
  
  
  # Display charts
  output$anc_1_timing <- renderDygraph({
    dygraph(anc_1consultations_group(),
            main = paste0( "Total ANC 1 visits & ANC 1 visits (1st Trimester) for ", input$facility)) |> 
      
      dySeries("total_anc1_visits",
               label = "Total ANC 1 Visits",
               color = "#FF9900",strokeWidth = 2,drawPoints = TRUE, pointSize = 4,
               # Set labels for x and y axes
               pointShape = "circle",fillGraph = TRUE) |> 
      dySeries("total_anc1_visits_first",
               label = "Total ANC 1 Visits (First trimester)",
               color = "maroon",
               strokeWidth = 2,
               drawPoints = TRUE, pointSize = 4,
               # Set labels for x and y axes
               pointShape = "circle",
               fillGraph = TRUE) |> 
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 0.2,highlightSeriesOpts = list(strokeWidth = 3),
                  hideOnMouseOut = TRUE)
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  total_deliveris_ <- reactive({
    combined_dataset |> 
      select(date,,`105-AN01b. ANC 1st Visit for women (1st Trimester)`) |> 
      group_by(date) |> 
      summarise(total_anc_1_visitss = sum(`105-AN01a. ANC 1st Visit for women`,na.rm = TRUE),
                total_anc_1_firsts = sum(`105-AN01b. ANC 1st Visit for women (1st Trimester)`,na.rm = TRUE), .groups = "drop")
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)
