library(ggplot2)
library(dplyr)
library(plotly)
library(lubridate)

clean_year <- function(data){
  data$inspection.date <- as.Date(data$inspection.date)
  data$year <- year(data$inspection.date)
  
  current_year <- max(data$year, na.rm = TRUE)
  last_5_years <- current_year - 4
  
  hcv_recent_5 <- data %>%
    filter(year >= last_5_years)
  
  hcv_borough_year <- hcv_recent_5 %>%
    group_by(boro, year) %>%
    summarise(violation_count = n()) %>%
    ungroup()
  return(hcv_borough_year)
}

# line plot
plot_violations_by_borough <- function(data){
  
  hcv_borough_year <- clean_year(data)
  #define colors
  colors_borough <- c("Manhattan" = "#6D9AC6",  
                      "Brooklyn" = "#F0A88C",  
                      "Queens" = "#A1D6B9", 
                      "Bronx" = "#F296B3",   
                      "Staten Island" = "#C89BCC")
  
  #ggplot
  plot_violations_by_borough <- ggplot(hcv_borough_year, aes(x = year, y = violation_count, color = boro)) +
    geom_line(size = 1.2) +
    geom_point(size = 3, shape = 21, fill = "white", 
               aes(text = paste(
                 "Borough: ", boro, "<br>",
                 "Year: ", year, "<br>",
                 "Violation Count: ", violation_count
               ))) +
    scale_color_manual(values = colors_borough) +
    labs(
      title = "Health Code Violations by Borough",
      x = "Year",
      y = "Number of Violations",  # Proper y-axis label
      color = "Borough"
    ) +
    scale_y_continuous(
      breaks = seq(0, max(hcv_borough_year$violation_count), by = 5000) 
    ) +
    theme_minimal(base_size = 14) + 
    theme(
      plot.background = element_rect(fill = "#f8f9fa"),
      plot.title = element_text(size = 13, color = "#333333"),
      axis.title = element_text(size = 11, face = "light"),
      axis.text = element_text(size = 10, color = "#555555"),
      legend.title = element_text(size = 11, face = "light"),
      legend.text = element_text(size = 10),
      panel.grid.major = element_line(color = "#DDDDDD", size = 0.5), 
      panel.grid.minor = element_line(color = "#DDDDDD", size = 0.25)
    )
  
  # make interactive
  interactive_plot_borough_year <- ggplotly(plot_violations_by_borough, tooltip = "text") %>%
    layout(
      xaxis = list(
        showticklabels = TRUE
      ),
      yaxis = list(
        fixedrange = FALSE # zooming
      ),
      hoverlabel = list(
        bgcolor = "white", 
        font = list(color = "black") 
      ),
      margin = list(l = 100)
    )
  return(interactive_plot_borough_year)
}

clean_month <- function(data){
  hcv$inspection.date <- as.Date(hcv$inspection.date)
  current_year <- max(hcv$inspection.date, na.rm = TRUE)
  last_5_years <- current_year - years(5)
  
  hcv_recent_5 <- hcv %>%
    filter(inspection.date >= last_5_years)
  
  # year and month
  hcv_recent_5 <- hcv_recent_5 %>%
    mutate(
      year = format(inspection.date, "%Y"),
      month = format(inspection.date, "%m")
    )
  
  hcv_borough_month <- hcv_recent_5 %>%
    group_by(boro, year, month) %>%
    summarise(violation_count = n()) %>%
    ungroup()
  return(hcv_borough_month)
}

clean_oneyear <- function(data){
  data$inspection.date <- as.Date(data$inspection.date)
  data$year <- year(data$inspection.date)
  
  data <- data %>%
    mutate(
      year = year(inspection.date),
      month = month(inspection.date)
    )
  
  hcv_recent <- data %>%
    filter(year == 2023)
  
  hcv_recent <- hcv_recent %>%
    mutate(month_abb = month.abb[as.numeric(month)])
       
  hcv_recent$month_abb <- factor(hcv_recent$month_abb, levels=month.abb) # keep in order of month thorough factor
  
  hcv_borough_month <- hcv_recent %>%
    group_by(boro, year, month, month_abb) %>%
    summarise(violation_count = n()) %>%
    ungroup()
  return(hcv_borough_month)
}

plot_violations_by_borough2 <- function(data){
  colors_borough <- c("Manhattan" = "#6D9AC6",  
                      "Brooklyn" = "#F0A88C",  
                      "Queens" = "#A1D6B9", 
                      "Bronx" = "#F296B3",   
                      "Staten Island" = "#C89BCC")
  
  hcv_borough_month <- clean_oneyear(data)
  
  #ggplot
  plot_violations_by_borough <- ggplot(hcv_borough_month, aes(x = month_abb, y = violation_count, color = boro, group = boro)) +
    geom_line(size = 0.8) +  
    geom_point(size = 3, shape = 21, fill = "white", 
               aes(text = paste(
                 "Borough: ", boro, "<br>",
                 "Month: ", month_abb, "<br>",
                 "Violation Count: ", violation_count
               ))) +  
    scale_color_manual(values = colors_borough) +
    labs(
      title = "Health Code Violations by Borough by Month (2023)",
      x = "Month",
      y = "Number of Violations",
      color = "Borough"
    ) +
    scale_y_continuous(
      breaks = seq(0, max(hcv_borough_month$violation_count), by = 500)
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.background = element_rect(fill = "#f8f9fa"),
      plot.title = element_text(size = 13, color = "#333333"),
      axis.title = element_text(size = 11, face = "light"),
      axis.text = element_text(size = 10, color = "#555555"),
      legend.title = element_text(size = 11, face = "light"),
      legend.text = element_text(size = 10),
      panel.grid.major = element_line(color = "#DDDDDD", size = 0.5), 
      panel.grid.minor = element_line(color = "#DDDDDD", size = 0.25),
      axis.text.x = element_text(),
      axis.text.y = element_text()
    )
  
  #plotly
  interactive_plot_borough2 <- ggplotly(plot_violations_by_borough, tooltip = "text") %>%
    layout(
      xaxis = list(
        showticklabels = TRUE
      ),
      yaxis = list(
        fixedrange = FALSE
      ),
      hoverlabel = list(
        bgcolor = "white",
        font = list(color = "black")
      ),
      margin = list(l = 100)
    )
  return(interactive_plot_borough2)
}