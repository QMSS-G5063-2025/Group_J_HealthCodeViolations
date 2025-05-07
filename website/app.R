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
library(wordcloud)
library(tm)
library(syuzhet)
library(data.table)
library(tidytext)
library(scales)

# Helper Function Files
source("map_functions.R")
source("line_functions.R")
source("BarGraphs.R")
source("text_functions.R")

ui <- navbarPage(
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
                    margin-left: 20px;
                    margin-right: 20px;
                    border-radius: 8px;
                  }
                  .custom-header2 {
                    padding: 100px 0;
                    text-align: center;
                    background-image: url('https://drive.google.com/uc?export=download&id=1aArcPl0B1uFQLp2PaRrGq9dIx7AavRzU');
                    background-size: cover;
                    background-position: center;
                    color: #fff;
                    margin-bottom: 60px;
                    margin-left: 20px;
                    margin-right: 20px;
                    border-radius: 8px;
                  }
                  .feature {
                    text-align: center;
                    padding: 40px 20px;
                    border-radius: 8px;
                    background-color: #fff;
                    margin: 10px;
                    line-height:28px;
                  }
                  .custom-row {
                    display: flex;
                    justify-content: space-around;
                  }
                  .custom-row2 {
                    display: flex;
                    justify-content: space-around;
                    margin-bottom: 20px;
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
                    margin-left: 20px;
                    margin-right: 20px;
                    padding-bottom: 10px;
                  }
                  .custom-text{
                    font-size: 16px;
                    margin-right: 40px;
                    line-height:28px;
                  }
                  .navbar {
                    padding: 10px 0px;
                  }
                  .navbar .navbar-brand {
                    font-weight: 600;
                    color: #264653;
                    font-size: 18px;
                    padding-left: 70px;
                  }
                  .navbar-nav > li > a {
                    color: #343a40;
                    font-weight: 600;
                    font-size: 18px;
                    color: #343a40; 
                    padding: 15px 20px;
                  }
                  .custom-list {
                    font-size: 16px;
                    margin-bottom: 60px;
                  }
                  .custom-list2 {
                    font-size: 16px;
                    margin-bottom: 5px;
                  }
                  .custom-dropdown{
                    margin-top: 20px;
                    margin-left: 20px;
                    margin-right: 20px;
                    padding-bottom: 60px;
                    font-size: 16px;
                  }
                  .single-figure{
                    padding-right: 20px;
                    padding-left: 20px;
                    padding-bottom: 10px;
                  }
                  .single-figure-bot{
                    padding-right: 200px;
                    padding-left: 200px;
                    padding-bottom: 60px;
                  }
                ")),
    tabsetPanel(id = "tabs",
                tabPanel("Home",
                         fluidPage(
                           tags$div(class = "custom-header",
                                    h2("Health Code Violations in NYC"),
                                    h4("An exploration of trends in restaurant health code violations across New York City between 2015 - 2023"),
                                    h5("Katherine Lin, Humaira Ahmed, and Juna Kawai-Yue"),
                                    h5("Quantitative Methods in the Social Sciences @ Columbia University")
                           ),
                           tags$div(class = "custom-container",
                                    h4("Project Overview"),
                                    tags$div(class = "custom-text",
                                             p("Our team was interested in investigating patterns in restaurant health code violations across New York City. We wanted to look at a few key aspects of these violations in tandem with reviews of these restaurants to see whether or not health code violations impact customer experiences but also what general trends there are within the city regarding these violations."),
                                             p("Within this website, you will find maps, barplots, word clouds, and line plots that allow you to explore trends in the number of violations across NYC and between boroughs and cuisines, types of violations, and the customer experience and how these factors relate to socioeconomic makeups of neighborhoods."))),
                           tags$div(class = "custom-container",
                                    h4("Within this website you can explore:"),
                                    tags$div(class = "custom-row",
                                             tags$div(class = "col-md-4 feature",
                                                      tags$i(class = "fas fa-magnifying-glass-chart"),
                                                      h4("Overall Trends"),
                                                      h5("Visualizations of health code violation trends across NYC")
                                             ),
                                             tags$div(class = "col-md-4 feature",
                                                      tags$i(class = "fas fa-triangle-exclamation"),
                                                      h4("Violations"),
                                                      h5("Deep dive into specific health code violations")
                                             ),
                                             tags$div(class = "col-md-4 feature",
                                                      tags$i(class = "fas fa-quote-right"),
                                                      h4("Google Reviews"),
                                                      h5("Analysis of recent Google Reviews for restaurants with high numbers of violations")
                                             )
                                    )
                           ),
                           tags$div(class = "custom-container",
                                    h4("Data Sources & APIs Used"),
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
                                    h3("Trends in Health Code Violations Across New York City and by Borough"),
                                    tags$div(class = "custom-text",
                                             p("Check out greater trends of health code violations across New York City, within each borough, and by cuisine and how other factors, like socioeconomic status are related."))
                           ),
                           tags$div(class = 'custom-container',
                                    h4("But First...Let's Get a General Understanding of the Data"),
                                    tags$div(class = "custom-text",
                                             p("Let's get a base understanding of the distribution of restaurants across NYC"),
                                    tags$div(class = "'single-figure'",
                                             tags$div(class = "custom-text",
                                                      plotlyOutput("gen_restcount", height = 450, width = "100%"))),
                                    tags$div(class = "custom-text",
                                             p("Let's also look at the distribution of cuisines across NYC.")),
                                    tags$div(class = "custom-dropdown",
                                             selectInput("cuis_region", "Select a Region:", 
                                                      c(
                                                        "All of NYC" = "NYC",
                                                        "Brooklyn" = "Brooklyn",
                                                        "Queens" = "Queens",
                                                        "Bronx" = "Bronx",
                                                        "Manhattan" = "Manhattan",
                                                        "Staten Island" = "Staten Island"
                                                      ), width = "540px"),
                                             plotlyOutput("region_cuis", height=450))
                           ),
                           tags$div(class = 'custom-container',
                                    h4("Number of Violations Issued between 2019-2023"),
                                    tags$div(class = "custom-text",
                                             p("Results show a general spike in restaurant reviews between the years of 2021 and 2022. This may be explained by the impacts of the COVID-19 pandemic on public health perspectives on hygiene and increased emphasis on maintaining high standards for health code evaluations."),
                                             p("Results also showed higher numbers of violations per borough within March and May. This could be explained by the climate shift that occurs within those months to warmer temperatures, in which pests, for example, tend to become more visibly prominent, and thus may result in increased health code violations overall.")),
                                    tags$div(class = "custom-row",
                                             plotlyOutput("last_5", height = 450, width = "100%"),
                                             plotlyOutput("last_5_month", height = 450, width = "100%"))
                           ),
                           tags$div(class = 'custom-container',
                                    h4("Number of Violations by Borough and with Respect to Number of Restaurants"),
                                    tags$div(class = "custom-text",
                                      p("These visualizations demonstrate that while Manhattan has the most violations, it has the lowest rates of violations issued per restaurant.")),
                                    tags$div(class = "custom-row",
                                             plotlyOutput("by_borough", height = 450, width = "100%"),
                                             plotlyOutput("viol_per_rest", height = 450, width = "100%")
                                    )
                           ),
                           tags$div(class = 'custom-container',
                                    h4("Number of Violations with Respect to Income by Borough"),
                                    tags$div(class = "custom-text",
                                             p("An interesting trend to note from this graph is that in Brooklyn, neighborhoods with higher average incomes are correlated with increasingly higher numbers of violations after around $800,000. This general trend of higher average income levels within neighrbohoods being associated with high numbers of violations is also present in other boroughs. However, this could be due to the number of establishments present in these areas and accessibility of these restaurants within these nieghborhoods.")),
                                    tags$div(class = 'single-figure',
                                             tags$div(class = "custom-text",
                                                      plotlyOutput("income", height = 450, width = "100%"))
                                    )
                           ),
                           tags$div(class = "custom-container",
                                    h4("Cuisine Types with the Most Health Code Violations"),
                                    tags$div(class = 'single-figure',
                                             tags$div(class = "custom-text",
                                                      plotlyOutput('cuis_viol'), height = 600, width = "100%")
                                    )
                           ),
                           tags$div(class = "custom-dropdown",
                                    h4("Explore the Top 500 Restaurants with the Most Health Code Violations Across NYC"),
                                    selectInput("region", "Select a Region:", 
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
                         ))),
                tabPanel('Violations',
                         fluidPage(
                           tags$div(class = "custom-container",
                                    h3("Trends in Health Code Violations"),
                                    tags$div(class = "custom-text",
                                             p("Dive deeper into common critical and non-critical violations across New York City."),
                                             p("To better visualize the violations, they were categorized into the following 10 categories: ")),
                                    tags$div(class = 'custom-list2',
                                             tags$ul(
                                               tags$li('Temperature Control - Improper food storage temperatures, lack of properly scaled thermometer'),
                                               tags$li("Pest Infestation - Pests in food and non-food areas, lack of pest control contracts, etc."),
                                               tags$li("Cleanliness & Hygiene - Insufficient cleaning and hygiene maintenance practices"),
                                               tags$li("Food Protection - Risk of contaimination, improper food storage, etc."),
                                               tags$li("Plumbing & Facilities - Poor availability and maintenance of plumbing and related facilities"),
                                               tags$li("Chemical Safety - Improper storage or handling of toxic materials"),
                                               tags$li("Equipment Maintenance - Poor maintence of food contact surfaces, equipment, etc."),
                                               tags$li("Employee Practices - Improper handwashing, use of gloves, handling of food, etc."),
                                               tags$li("Administrative Compliance - Issues with licensing, permits, certificates, etc."),
                                               tags$li("Other")
                                             ))
                           ),
                           tags$div(class = 'custom-dropdown',
                                    h4("Top 5 Common Types of Violations Across NYC"),
                                    tags$div(class = "custom-text",
                                             p("View the top 5 non-critical and critical violation types overall and just the top 5 critical violation types below.")),
                                    tags$div(class = "custom-text",
                                             p("Critical violations differ from non-critical violations by the increased likelihood they will contribute to food borne illnesses and risks to the public's health."),
                                             tags$a(href = "https://www.nyc.gov/assets/doh/downloads/pdf/rii/blue-book.pdf",
                                                    "Learn more about it from the New York Health department")),
                                    selectInput("crit", "Select View:",
                                                c('Overall violations (non-critical and critical)'="overall", 'Critical violations'="crit"), width = "540px"),
                                    plotlyOutput('crit_graph'), height = 450, width = "100%"),
                           tags$div(class = 'custom-dropdown',
                                    h4("Top 5 Common Types of  Violations by Borough"),
                                    tags$div(class = "custom-text",
                                             p("View the top 5 non-critical and critical violation types overall and just the top 5 critical violation types by borough below.")),
                                    selectInput("crit_boro", "Select View:",
                                                c('Overall violations (non-critical and critical)'="overall", 'Critical violations'="crit"), width = "540px"),
                                    plotlyOutput('crit_boro_graph'), height = 450, width = "100%"),
                           tags$div(class = "custom-container",
                                    h4("Top 5 Common Types of Violations by Common Cuisine Type"),
                                    tags$div(class = "custom-text",
                                             p("View the top 5 non-critical and critical violation types overall by common cuisines below."),
                                             p("While all types of cuisines share the same top 5 most common violations, coffee and tea establishments appear to have more issues with cleanliness and hygiene than pest infestations compared to their counterparts.")),
                                    tags$div(class = 'single-figure',
                                             plotlyOutput('cuis_viol_type'), height = 600, width = "100%")
                                    
                           ),
                           tags$div(class = 'custom-container',
                                    h4("Explore the Restaurants that have been Issued the Top 10 Most Common Critical Violations Across NYC"),
                                    tags$div(class = "custom-text",
                                             p("Use the following maps to do further exploration of restaurants that have been issued the following most common critical health code violations:")),
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
                                               tags$li("Food not protected from potential source of contamination during storage, preparation, transportation, display or service")
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
                         )
                ),
                tabPanel('Google Reviews',
                         fluidPage(
                           tags$div(class = "custom-container",
                                    h3("Text Analysis on Most Recent Google Reviews from Top Violation Restaurants per Borough"),
                                    tags$div(class = "custom-text",
                                             p("Explore Google Reviews from the top 10 restaurants with the most health code violations per borough and whether or not high numbers of health code violations impact customer experiences."),
                                             p("Note: Due to limitations of the Google Places API, only 5 most recent reviews of each restaurant were able to be obtained for analysis.")
                                    )),
                           tags$div(class = "custom-dropdown",
                                    h4("Top 10 Restaurants with Highest Health Code Violations by Borough"),
                                    selectInput("top_10_rest", "Select a Borough:", 
                                                c(
                                                  "Manhattan" = "Manhattan",
                                                  "Brooklyn" = "Brooklyn",
                                                  "Bronx" = "Bronx",
                                                  "Queens" = "Queens",
                                                  "Staten Island" = "Staten Island"
                                                ),
                                                width = "540px"),
                                    plotlyOutput('top_10_rest_graph', height = 450)
                           ),
                           tags$div(class = "custom-container",
                                    h4("Most Common Words in Recent Restaurant Reviews"),
                                    tags$div(class = 'custom-list2',
                                             tags$div(class = "custom-text",
                                                      p("Notes:")),
                                             tags$ul(
                                               tags$li('Reviews from Master Wok, The Arch Diner, Temaske, Prima Pasta & Cafe, and Brass Riz & Grill Express were not available due to closure or lack of reviews'),
                                               tags$li("‘All of NYC’ represents the collection of reviews pulled from each borough"))),
                                    tags$div(class = "custom-text",
                                             p("Aside from food and establishment related words, like 'service' and 'food', other common words suggest positive reviews across the data, such as 'great' and 'delicious'.")),
                                    tags$div(class = "custom-row",
                                             tags$div(class = "custom-dropdown",
                                                      selectInput("word_boro", "Select a Region:", 
                                                                  c(
                                                                    "All of NYC" = "nyc",
                                                                    "Brooklyn" = "brook",
                                                                    "Queens" = "queen",
                                                                    "Bronx" = "bronx",
                                                                    "Manhattan" = "man",
                                                                    "Staten Island" = "stat"
                                                                  ), width = "540px"),
                                                      plotOutput("clouds", height=450, width = "100%")),
                                             tags$div(class = "custom-dropdown",
                                                      selectInput("top_15", "Select a Region:", 
                                                                  c(
                                                                    "All of NYC" = "New York City",
                                                                    "Brooklyn" = "Brooklyn",
                                                                    "Queens" = "Queens",
                                                                    "Bronx" = "Bronx",
                                                                    "Manhattan" = "Manhattan",
                                                                    "Staten Island" = "Staten Island"
                                                                  ), width = "540px"),
                                                      plotlyOutput("top_15_graph", height=450, width = "100%")))
                           ),
                           tags$div(class = "custom-container",
                                    h4("Sentiment Analysis of Restaurant Reviews by Boroughs"),
                                    tags$div(class = "custom-text",
                                             p("The following visualization compares the overall sentiment (positive, negative, or neutral tone) of recent online reviews for the top 10 restaurants with the most health code violations in each NYC borough (as of 2023)."),
                                             p("Sentiment analysis uses natural language processing to determine whether the words used in a review express a positive, negative, or neutral attitude. This helps us understand how customers feel about these restaurants — beyond just the violation data."),
                                             p("Sentiment scores in this figure can be interpretted in the following ways:")),
                                    tags$div(class = 'custom-list2',
                                             tags$ul(
                                               tags$li('< 0 = Negative sentiment'),
                                               tags$li("0 to 1 = Neutral to slightly positive sentiment"),
                                               tags$li("1 to 2.5 = Generally positive sentiment"),
                                               tags$li("> 2.5 = Strongly positive sentiment")
                                             )),
                                    tags$div(class = "custom-text",
                                             p("Results of sentiment analysis suggest that while these restaurants have had a significant number of health code violations, the customer experience is not harmed, in fact, reviews remain positive overall. This ultimately suggests that health code violations do not necessarily indicate or reflect a poor experience at a restaurant establishment.")),
                                    tags$div(class = 'single-figure-bot',
                                             tags$div(class = "custom-text",
                                                      plotlyOutput('sent_graph'), height = 600, width = "100%")
                                    )
                                    
                            )
                           )
                         )
    )
  )
)

server <- function(input, output) {
  # Load health code violations data
  hcv <- readRDS("data/hcv.rds")
  avg_income <- read.csv("data/avg_income.csv")
  queens_revs <-  read.csv("data/queens_revs.csv")
  bronx_revs <- read.csv("data/bronx_revs.csv")
  man_revs <- read.csv("data/man_revs.csv")
  brook_revs <- read.csv("data/brook_revs.csv")
  stat_revs <- read.csv("data/stat_revs.csv")
  
  all_revs <- bind_rows(
    man_revs,
    brook_revs,
    queens_revs,
    bronx_revs,
    stat_revs
  )
  
  observeEvent(input$tabs, {
    if (input$tabs == "Overall Trends"){
      output$gen_restcount <- renderPlotly({
        borough_restaurants_plot_func(hcv)
      })
      
      output$viol_per_rest <- renderPlotly({
        borough_1000_violations_plot_func(hcv)
      })
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
      
      
      output$income <- renderPlotly({
        plot_income(hcv, avg_income)
      })
      
      output$region_map <- renderLeaflet({
        map_allviolations(hcv, input$region)
      })
      
      output$region_cuis <- renderPlotly({
        if(input$cuis_region == "NYC"){
          overall_cuisine_plot_func(hcv)
        }else if(input$cuis_region == "Brooklyn"){
          borough_common_cuisines_plot_func(hcv, "Brooklyn")
        }else if(input$cuis_region == "Queens"){
          borough_common_cuisines_plot_func(hcv, "Queens")
        }else if(input$cuis_region == "Bronx"){
          borough_common_cuisines_plot_func(hcv, "Bronx")
        }else if(input$cuis_region == "Staten Island"){
          borough_common_cuisines_plot_func(hcv, "Staten Island")
        }else if(input$cuis_region == "Manhattan"){
          borough_common_cuisines_plot_func(hcv, "Manhattan")
        }
      })
      
      output$cuis_viol <- renderPlotly({
        cuisine_violations_plot_func(hcv)
      })
    }
    
    if (input$tabs == "Violations"){
      output$crit_graph <- renderPlotly({
        select_viol(hcv, input$crit)
      })
      
      output$crit_boro_graph <- renderPlotly({
        select_viol_boro(hcv, input$crit_boro)
      })
      
      output$viol_map <- renderLeaflet({
        map_critviolations(hcv, input$violation)
      })
      
      output$cuis_viol_type <- renderPlotly({
        cuisine_common_violations_plot_func(hcv)
      })
      
    }
    
    if(input$tabs == "Google Reviews"){
      output$top_10_rest_graph <- renderPlotly({
        top10_rest(hcv, input$top_10_rest)
      })
      
      output$clouds <- renderPlot({
        if (input$word_boro == "nyc") {
          word_clouds(all_revs)
        } else if (input$word_boro == "brook") {
          word_clouds(brook_revs)
        } else if (input$word_boro == "queen") {
          word_clouds(queens_revs)
        } else if (input$word_boro == "bronx") {
          word_clouds(bronx_revs)
        } else if (input$word_boro == "man") {
          word_clouds(man_revs)
        } else if (input$word_boro == "stat") {
          word_clouds(stat_revs)
        }
      })
      
      # top words
      observe({
        if(input$top_15=="New York City"){
          output$top_15_graph <- renderPlotly({
            top_15(all_revs, "New York City")
          })
        }
        if(input$top_15=="Brooklyn"){
          output$top_15_graph <- renderPlotly({
            top_15(brook_revs, "Brooklyn")
          })
        }
        if(input$top_15=="Queens"){
          output$top_15_graph <- renderPlotly({
            top_15(queens_revs, "Queens")
          })
        }
        if(input$top_15=="Bronx"){
          output$top_15_graph <- renderPlotly({
            top_15(bronx_revs, "Bronx")
          })
        }
        if(input$top_15=="Manhattan"){
          output$top_15_graph <- renderPlotly({
            top_15(man_revs, "Manhattan")
          })
        }
        if(input$top_15=="Staten Island"){
          output$top_15_graph <- renderPlotly({
            top_15(stat_revs, "Staten Island")
          })
        }
      })
      
      output$sent_graph <- renderPlotly({
        graph_sent(queens_revs, bronx_revs, man_revs, brook_revs, stat_revs)
      })
      
    }
  })
}
shinyApp(ui = ui, server = server)



