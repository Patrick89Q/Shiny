
rm(list = ls())
library(shiny)
library(tidyverse)
library(here)
library(rio)
library(dygraphs)


# Reading all the data sets for ANC

# All 2020

all_2020_anc <- import(here("datasets","ANC","All 2020 ANC LN Data.csv"))

# All 2021
all_2021_anc <- import(here("datasets","ANC","All 2021 ANC LN Data.csv"))

# All 2022
all_2022_anc <- import(here("datasets","ANC","All 2022 ANC LN Data.csv"))

# All 2023
all_2023_anc <- import(here("datasets","ANC","All 2023 ANC LN Data.csv"))

# Part of 2024
all_2024_anc <- import(here("datasets","ANC","All 2024 ANC LN Data (till April 2024).csv"))


# Merging all the data sets


combined_dataset <- rbind(all_2020_anc,all_2021_anc,all_2022_anc,all_2023_anc,all_2024_anc) |> 
  as_tibble() |> mutate(year = substr(periodid,1,4),
                        month = substr(periodid,nchar(periodid) - 1,nchar(periodid)),
                        date = clock::date_build(as.integer(year),as.integer(month)))


min_date <- format(min(combined_dataset$date),"%b-%Y")


max_date <- format(max(combined_dataset$date),"%b-%Y")



regions <- sort(unique(combined_dataset$orgunitlevel2))

districts <- sort( unique(combined_dataset$orgunitlevel3))


facility <- sort( unique(combined_dataset$orgunitlevel5))



total_anc1 <- sum(combined_dataset$`105-AN01a. ANC 1st Visit for women`,na.rm = TRUE)



total_anc1_time <- sum(combined_dataset$`105-AN01b. ANC 1st Visit for women (1st Trimester)`,na.rm = TRUE)



overall_anc_1rate <- total_anc1_time/total_anc1




# Total anaemia screening


total_hb_tests <- sum(combined_dataset$`105-AN17. Pregnant women who were tested for Anaemia using Hb Test at ANC 1st contact / visit`,na.rm = TRUE)

total_anae_cases <- sum(combined_dataset$`105-AN18. Pregnant women with Hb <10g/dl at ANC 1st contact / visit`,na.rm = TRUE)



hb_testing_rate <- total_hb_tests/total_anc1

















anc_value_boxes <- layout_columns(
  value_box(
    title = h3("Total ANC 1"),tags$p("from ", min_date,"to ",max_date),
    value = paste0(scales::comma(total_anc1), " visits"),
    theme = "bg-gradient-yellow-green",
    showcase = fontawesome::fa_i("person-pregnant","solid"),
    class = "border"
  ),
  value_box(
    title = "ANC 1 (First Trimester)",
    tags$p("from ", min_date,"to ",max_date),
    tags$p("with",scales::percent(overall_anc_1rate,suffix = "%"),"Early ANC initiation"),
    value = paste0(scales::comma(total_anc1_time), " visits"),
    theme = if (overall_anc_1rate < 0.8) {
     "bg-gradient-red-green"
   } else {
     "bg-gradient-yellow-green"
   },
    showcase = fontawesome::fa_i("user-clock"),
    # showcase_layout = "top right"
  ),
  value_box(
    title = "Aneamia screening",
    tags$p("from ", min_date,"to ",max_date),
    tags$p("with",scales::percent(hb_testing_rate,suffix = "%"),"hb testing rate"),
    value = paste0(scales::comma(total_hb_tests), " Tests"),
    theme = "bg-gradient-teal-red",
    showcase =  fontawesome::fa_i("droplet-slash","solid"),
    class = "bg-red"
    # showcase_layout = "top right"
  ),
  value_box(
    title = "Daily Active Users",
    value = "8,507",
    #theme = "bg-gradient-teal-purple",
    showcase = fontawesome::fa_i("stethoscope","solid"),
    showcase_layout = "top right"
  ))








anc_value_boxes2 <- layout_columns(
  value_box(
    title = h3("Total ANC 1"),tags$p("from ", min_date,"to ",max_date),
    value = paste0(scales::comma(total_anc1), " visits"),
    theme = "bg-gradient-yellow-green",
    showcase = fontawesome::fa_i("person-pregnant","solid"),
    class = "border"
  ),
  value_box(
    title = "ANC 1 (First Trimester)",
    tags$p("from ", min_date,"to ",max_date),
    tags$p("with",scales::percent(overall_anc_1rate,suffix = "% early ANC timing")),
    value = paste0(scales::comma(total_anc1_time), " visits"),
    theme = if (overall_anc_1rate < 0.8) {
      "bg-gradient-red-green"
    } else {
      "bg-gradient-yellow-green"
    },
    showcase = fontawesome::fa_i("user-clock"),
    # showcase_layout = "top right"
  ),
  value_box(
    title = "Aneamia screening",
    value = "8,507",
    theme = "bg-gradient-teal-purple",
    showcase =  fontawesome::fa_i("house-medical-circle-check"),
    # showcase_layout = "top right"
  ),
  
  value_box(
    title = h3("Total ANC 1"),tags$p("from ", min_date,"to ",max_date),
    value = paste0(scales::comma(total_anc1), " visits"),
    theme = "bg-gradient-yellow-green",
    showcase = fontawesome::fa_i("person-pregnant","solid"),
    class = "border"
  ))
  





## ANC crads ----

anc_cards <- layout_columns(
  flights_card <- card(
    full_screen = TRUE,
    card_header(
      "Total ANC 1 Consultations",
      tooltip(
        bsicons::bs_icon("info-circle", title = "About ANC 1 Visits"),
        "This is an overall trend of ANC 1 visits"
      ),
      class = "d-flex justify-content-between align-items-center"
    ),
    dygraphOutput("anc_consultations_plot")
  ),
  
  
  avg_delay_by_category <- card(
    full_screen = TRUE,
    card_header(
      "Facility level ANC 1 & ANC 1 Timing", 
      popover(
        bsicons::bs_icon("gear", title = "Navigation"),
        selectInput(
          "region", "Region",regions,selected = "All"),
        selectInput(
          "district", "District",choices = NULL,selected = "All"),
        selectInput(
          "facility", "Facility",choices = NULL,selected = "All"),
        
      ),
      class = "d-flex justify-content-between align-items-center"
    ),
    dygraphOutput("anc_1_timing")
  )
)






anc_cards_2 <- layout_columns(
  hb_tests <- card(
    full_screen = TRUE,
    card_header(
      "Total Hb screening among ANC 1",
      
      class = "d-flex justify-content-between align-items-center"
    ),
    dygraphOutput("hb_tests")
  ),
  
  
  
  hb_tests <- card(
    full_screen = TRUE,
    card_header(
      "Total Hb screening among ANC 1",
      popover(
        bsicons::bs_icon("gear", title = "Navigation"),
        selectInput(
          "region_hb", "Region",regions,selected = "All"),
        selectInput(
          "district_hb", "District",choices = NULL,selected = "All"),
        selectInput(
          "facility_hb", "Facility",choices = NULL,selected = "All")
        
      ),
      
      class = "d-flex justify-content-between align-items-center"
    ),
    dygraphOutput("anaemia_cases")
  )
  
  )














anc_distribution <- navset_card_underline(
  title = "Distribution of ANC Visits",
  full_screen = TRUE,
  id = "delay_dist_nav",
  sidebar = sidebar(
    position = "right",
    open = FALSE,
    radioButtons(
      "delay_dist_type", "Delay type",
      c("Arrival" = "arr_delay", "Departure" = "dep_delay"),
      inline = TRUE
    ),
    conditionalPanel(class = "bg-red",
      "input.delay_dist_nav === 'Overall'",
      selectizeInput(
        "delay_dist_category", "Split by",
        c("Choose a category" = "", "Carrier", "Month", "Weekday"),
        options = list(plugins = "remove_button")
      )
    )
  ),
  nav_panel(
    "Overall (All Facilities)",
    plotlyOutput("delay_dist")
  ),
  nav_panel(
    "By Region",
    plotlyOutput("arr_delay_series")
  )
)















































deliveries_dist <- navset_card_underline(
  title = "Distribution of Deliveries and live births",
  full_screen = TRUE,
  id = "delay_dist_nav",
  sidebar = sidebar(
    position = "right",
    open = FALSE,
    radioButtons(
      "delay_dist_type", "Delay type",
      c("Arrival" = "arr_delay", "Departure" = "dep_delay"),
      inline = TRUE
    ),
    conditionalPanel(
      "input.delay_dist_nav === 'Overall'",
      selectizeInput(
        "delay_dist_category", "Split by",
        c("Choose a category" = "", "Carrier", "Month", "Weekday"),
        options = list(plugins = "remove_button")
      )
    )
  ),
  nav_panel(
    "Overall (All Facilities)",
    plotlyOutput("delay_dist")
  ),
  nav_panel(
    "Over time",
    plotlyOutput("arr_delay_series")
  )
)









maternity <- layout_columns(
  total_deliveries <- card(
    full_screen = TRUE,
    card_header(
      "Total Deliveries",
      tooltip(
        bsicons::bs_icon("info-circle", title = "About marker areas"),
        "Marker areas are proportional to mean arrival delay"
      ),
      class = "d-flex justify-content-between align-items-center"
    ),
    plotlyOutput("deliveries")
  ),
  
  
  live_births <- card(
    full_screen = TRUE,
    card_header(
      "Total live births",
      popover(
        bsicons::bs_icon("gear", title = "Settings"),
        selectInput(
          "select_region", "Region",regions,selected = "All"),
        selectInput(
          "select_district", "District",choices = NULL,selected = "All"),
        selectInput(
          "select_facility", "Facility",choices = NULL,selected = "All"),
        
      ),
      class = "d-flex justify-content-between align-items-center"
    ),
    plotlyOutput("scatter_delay")
  )
)





















