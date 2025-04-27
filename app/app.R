# Libraries
library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(leaflet)
library(leaflet)
library(htmltools)
library(plotly)
library(lubridate) 

# Helper Function Files
source("~/Desktop/Data_Viz/website/map_functions.R")
source("~/Desktop/Data_Viz/website/line_functions.R")
source("~/Desktop/Data_Viz/website/BarGraphs.R")

ui <- navbarPage(
  title = "NYC Health Code Violations",
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Poppins:400,500,600,700"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.0/css/all.min.css"),
    tags$style(HTML("
                  body {
                    font-family: 'Poppins', sans-serif;
                    background-color: #f8f9fa;
                    color: #343a40;
                  }
                  .custom-header {
                    padding: 100px 0;
                    text-align: center;
                    background: linear-gradient(135deg, #264653, #2A9D8F);
                    color: #fff;
                    margin-bottom: 60px;
                    margin-left: 30px;
                    margin-right: 30px;
                    border-radius: 8px;
                  }
                  .feature {
                    text-align: center;
                    padding: 40px 20px;
                    box-shadow: 0px 4px 10px rgba(0, 0, 0, 0.1);
                    border-radius: 8px;
                    background-color: #fff;
                    margin: 10px;
                    line-height:28px;
                  }
                  .custom-row {
                    display: flex;
                    justify-content: space-around;
                  }
                  .col-md-4 {
                    flex: 1;
                  }
                  .feature i {
                    font-size: 48px;
                    margin-bottom: 20px;
                    color: #F4A261;
                  }
                  .custom-container{
                    margin-top: 20px;
                    margin-left: 30px;
                    margin-right: 30px;
                    padding-bottom: 10px;
                  }
                  .custom-text{
                    font-size: 18px;
                    margin-right: 40px;
                    line-height:28px;
                  }
                  .navbar {
                    padding: 10px 0px;
                  }
                  .navbar .navbar-brand {
                    font-weight: 600;
                    color: #264653;
                    font-size: 20px;
                    padding-left: 70px;
                  }
                  .navbar-nav > li > a {
                    color: #343a40;
                    font-weight: 600;
                    font-size: 20px;
                    color: #343a40; 
                    padding: 15px 20px;
                  }
                  .custom-list {
                    font-size: 20px;
                    margin-bottom: 60px;
                  }
                  .custom-list2 {
                    font-size: 17px;
                    margin-bottom: 5px;
                  }
                  .custom-dropdown{
                    margin-top: 20px;
                    margin-left: 30px;
                    margin-right: 30px;
                    padding-bottom: 60px;
                    font-size: 18px;
                  }
                  .single-figure{
                    margin-left: 30px;
                    padding-right: 200px;
                    padding-left: 200px;
                    padding-bottom: 10px;
                  }
                "))
  ),
  tabPanel("Home",
           fluidPage(
                 tags$div(class = "custom-header",
                          h1("Health Code Violations in NYC"),
                          h3("An exploration of trends in restaurant health code violations across New York City between 2015 - 2023"),
                          h4("Katherine Lin, Humaira Ahmed, and Juna Kawai-Yue")
                 ),
                 tags$div(class = "custom-container",
                          h3("What's up with NYC restaurants?"),
                          tags$div(class = "custom-text",
                                  p("Our team was interested in investigating patterns in restaurant health code violations across New York City. We wanted to look at a few key aspects of these violations in tandem with reviews of these restaurants to see whether or not health code violations impact customer experiences but also what general trends there are within the city regarding these violations."),
                                  p("Within this website, you will find maps, barplots, word clouds, and line plots that demonstrate trends in the number of violations across NYC and between boroughs, cuisines, types of violations, and the customer experience and how these factors relate to socioeconomic makeups of the neighborhoods."))),
                 tags$div(class = "custom-container",
                          h3("This website explores health code violations in NYC in the following ways:"),
                          tags$div(class = "custom-row",
                                   tags$div(class = "col-md-4 feature",
                                            tags$i(class = "fas fa-magnifying-glass-chart"),
                                            h3("Overall Trends"),
                                            h4("Visualize health code violation trends across NYC.")
                                   ),
                                   tags$div(class = "col-md-4 feature",
                                            tags$i(class = "fas fa-triangle-exclamation"),
                                            h3("Violations"),
                                            h4("Look closer at trends around specific health code violations.")
                                   ),
                                   tags$div(class = "col-md-4 feature",
                                            tags$i(class = "fas fa-quote-right"),
                                            h3("Google Reviews"),
                                            h4("Check out trends of Google Reviews for restaurants with high numbers of violations.")
                                   )
                          )
                 ),
                 tags$div(class = "custom-container",
                          h3("Data Sources & APIs Used"),
                          tags$div(class = 'custom-list',
                                  tags$ul(
                                    tags$li(tags$a(href = "https://data.cityofnewyork.us/Health/DOHMH-New-York-City-Restaurant-Inspection-Results/43nn-pn8j/about_data",
                                                    "DOHMH New York City Restaurant Inspection Results (Health Code Violations)")),
                                    tags$li(tags$a(href = "https://github.com/erikgregorywebb/nyc-housing/blob/master/Data/nyc-zip-codes.csv",
                                                    "NYC Zip Codes by Borough")),
                                    tags$li(tags$a(href = "https://www.irs.gov/downloads/irs-soi?order=filesize&sort=desc&page=0",
                                                    "IRS Data on Income by Zip Code")),
                                    tags$li(tags$a(href = "https://developers.google.com/maps/documentation/places/web-service/overview",
                                                    "Google Places API (Web Scraping API for Google Reviews)"))
                          )))
            )
        ),
  tabPanel('Overall Trends',
           fluidPage(
             tags$div(class = "custom-container",
                      h2("Trends in Health Code Violations Across NYC and by Borough"),
                      tags$div(class = "custom-text",
                               p("This page breaks down the greater trends of health code violations across New York City and within each borough and how other factors, like socioeconomic status are related."))
             ),
             tags$div(class = 'custom-container',
                      h3("Trends from 2019-2023"),
                      tags$div(class = "custom-row",
                        plotlyOutput("last_5"),
                        plotlyOutput("last_5_month"))
             ),
             tags$div(class = 'custom-container',
                      h3("Number of Violations Across Boroughs"),
                      tags$div(class = 'single-figure',
                              plotlyOutput("by_borough")),
             ),
             tags$div(class = "custom-dropdown",
                      h3("Borough Violations"),
                      selectInput("region", "Region to Visualize:", 
                                  c(
                                    "All of NYC" = "nyc",
                                    "Brooklyn" = "brook",
                                    "Queens" = "queen",
                                    "Bronx" = "bronx",
                                    "Manhattan" = "man",
                                    "Staten Island" = "stat"
                                  ),
                                  width = "500px"),
                      leafletOutput("region_map", height = 600)
             )
           )),
  tabPanel('Violations',
           fluidPage(
              tags$div(class = "custom-container",
                       h2("Trends in Health Code Violations"),
                       tags$div(class = "custom-text",
                                p("This page dives deeper into a range of different types of violations, including, the top 10 most common violations across New York City."))
                 ),
              tags$div(class = "custom-container",
                       h3("Top 10 Most Common Violations by Borough"),
                       tags$div(class = "custom-text",
                                p("The following visualizations highlights these violations through their violation codes. Hover over each bar to read what each represents.")),
                       plotlyOutput("viol_borough")),
              tags$div(class = 'custom-container',
                       h3("Maps of Top 10 Most Common Critical Violations Across NYC"),
                       tags$div(class = "custom-text",
                                p("The following maps allow you to explore restaurants who have been issued violations of the following critical health code violations:")),
                       tags$div(class = 'custom-list2',
                                tags$ol(
                                  tags$li('Food contact surface not properly washed, rinsed and sanitized after each use and following any activity when contamination may have occurred'),
                                  tags$li("Evidence of mice or live mice in establishment's food or non-food areas"),
                                  tags$li("Cold TCS food item held above 41 °F; smoked or processed fish held above 38 °F; intact raw eggs held above 45 °F; or reduced oxygen packaged (ROP) TCS foods held above required temperatures except during active necessary preparation"),
                                  tags$li("Hot TCS food item not held at or above 140 °F."),
                                  tags$li("Raw, cooked or prepared food is adulterated, contaminated, cross-contaminated, or not discarded in accordance with HACCP plan"),
                                  tags$li("Evidence of mice or live mice present in facility's food and/or non-food areas"),
                                  tags$li("Filth flies or food/refuse/sewage associated with (FRSA) flies or other nuisance pests  in  establishment’s food and/or non-food areas. FRSA flies include house flies, blow flies, bottle flies, flesh flies, drain flies, Phorid flies and fruit flies"),
                                  tags$li("Food Protection Certificate (FPC) not held by manager or supervisor of food operations"),
                                  tags$li("Cold food item held above 41º F (smoked fish and reduced oxygen packaged foods above 38 ºF) except during necessary preparation"),
                                  tags$li("Food not protected from potential source of contamination during storage, preparation, transportation, display or service"),
                                ))),
              tags$div(class = "custom-dropdown",
                       selectInput("violation", "Select a Violation:", 
                                   c(
                                     "(1) Improper contact surface cleaning" = "1",
                                     "(2) Mice in food and non-food areas" = "2",
                                     "(3) Improper cold TCS food temperature storage" = "3",
                                     "(4) Improper hot TCS food temperature storage" = "4",
                                     "(5) Violation of HCCP Plan" = "5",
                                     "(6) Mice in facility's food and non-food areas" = "6",
                                     "(7) Filth flies, FRSA flies, or other nuisance pests" = "7",
                                     "(8) FPC not held by manager or supervisor of food operations." = "8",
                                     "(9) Improper cold food item temperature storage" = "9",
                                     "(10) Food not protected from potential source of contamination." = "10"
                                   ),
                                   width = "540px"),
                       leafletOutput('viol_map', height = 600)
              )
           )),
  tabPanel('Google Reviews',
           fluidPage(
             tags$div(class = "custom-container",
                      h2("Text Analysis on Google Reviews from Top Violation Restaurants per Borough"),
                      tags$div(class = "custom-text",
                               p("This page covers text analysis of the 5 most recent reviews from the top 10 restaurants per borough with the most health code violations."))
             )
           ))
)

server <- function(input, output) {
  # Load health code violations data
  hcv <- read.csv("~/Desktop/Data_Viz/hcvyear.csv")
  nyczips <- read.csv("~/Desktop/Data_Viz/nyc-zip-codes.csv")
  dta <- read.csv("~/Desktop/Data_Viz/22zpallagi.csv")
  
  # filter dta to only rows with zip code in the list of NYC Zip Codes
  dta_nyc <- dta[dta$zipcode %in% nyczips$ZipCode, ]
  
  income_filtered <- dta_nyc %>%
    select(STATE, zipcode, agi_stub, A00100, A02650)
  
  # average AGI and income per ZIP code
  avg_income <- dta_nyc %>%
    group_by(zipcode) %>%
    summarise(
      avg_AGI = mean(A00100, na.rm = TRUE),
      avg_income = mean(A02650, na.rm = TRUE)
    )
  
  # handle regional maps
  output$region_map <- renderLeaflet({
    map_allviolations(hcv, input$region)
  })
  
  # handle violation maps
  output$viol_map <- renderLeaflet({
    map_critviolations(hcv, input$violation)
  })
  
  # handle line plots
  output$last_5 <- renderPlotly({
    plot_violations_by_borough(hcv)
  })
  
  output$last_5_month <- renderPlotly({
    plot_violations_by_borough2(hcv)
  })
  
  # handle bar graphs
  output$by_borough <- renderPlotly({
    borough_violations_plot_func(hcv)
  })
  
  output$viol_borough <- renderPlotly({
    borough_common_violations_plot_func(hcv)
  })
}
shinyApp(ui = ui, server = server)



