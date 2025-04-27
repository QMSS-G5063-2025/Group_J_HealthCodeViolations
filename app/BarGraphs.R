# 1. WHICH BOROUGHS HAD THE MOST VIOLATIONS? (4/26)

borough_violations_func <- function(data) {
  temp <- data %>%
    filter(critical.flag != 'Not Applicable') %>% 
    count(boro) %>% 
    arrange(desc(n))
  return(temp)
}

# plotting from last function 
borough_violations_plot_func <- function(data) {
  boroughviolations <- borough_violations_func(data)
  
  colors_borough <- c("Manhattan" = "#6D9AC6",  
                      "Brooklyn" = "#F0A88C",  
                      "Queens" = "#A1D6B9", 
                      "Bronx" = "#F296B3",   
                      "Staten Island" = "#C89BCC")
  
  graphtemp <- ggplot(boroughviolations, aes(x = reorder(boro, n), y = n, fill = boro)) +
    geom_col(aes(text = paste(
      "Violation Count: ", n
    ))) +
    scale_fill_manual(values = colors_borough) +
    coord_flip() +
    xlab("Borough") + ylab("Number of violations") +
    guides(fill = guide_legend(title = "Borough")) +
    theme_minimal(base_size = 14) +
    theme(
          plot.background = element_rect(fill = "#f8f9fa"),
          axis.title = element_text(size = 11, face = "plain"),
          axis.text = element_text(size = 10, color = "#555555"),
          legend.title = element_text(size = 11, face = "plain"),
          legend.text = element_text(size = 10),
          panel.grid.major = element_line(color = "#DDDDDD", size = 0.5), 
          panel.grid.minor = element_line(color = "#DDDDDD", size = 0.25),
          axis.text.x = element_text(),
          axis.text.y = element_text())
  
  graph <- ggplotly(graphtemp, tooltip = "text") %>%
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
      )
    )
  return(graph)
}



# 2. MOST COMMON VIOLATIONS BY BOROUGH (TOP 10) (4/26)


borough_common_violations_func <- function(data) {
  violationmerge <- data %>% # need to clean up violation.descriptions since some entries have extra whitespace, etc.
    select(violation.code, violation.description) %>%
    distinct(violation.code, .keep_all = TRUE)
  
  temp <- data %>%
    filter(critical.flag != 'Not Applicable') %>%
    count(boro, violation.code, name = 'violation.code.count') %>%
    left_join(violationmerge, by = "violation.code") %>% # merging with unique violation description data
    group_by(boro) %>%
    arrange(desc(violation.code.count)) %>%  # ordering by descending order
    slice_head(n = 10) # getting top 10 per borough
  
  return(temp)
}


# plotting from last function 
borough_common_violations_plot_func <- function(data) {
  boroughcommonviolationsdata <- borough_common_violations_func(data)
  colors_borough <- c("Manhattan" = "#6D9AC6",  
                      "Brooklyn" = "#F0A88C",  
                      "Queens" = "#A1D6B9", 
                      "Bronx" = "#F296B3",   
                      "Staten Island" = "#C89BCC")
  
  plot <- ggplot(boroughcommonviolationsdata, aes(
    x = reorder(violation.code, violation.code.count), 
    y = violation.code.count, 
    fill = boro, 
    text = paste(
      "Violation Code Description:", violation.description,
      "<br>Borough:", boro,
      "<br>Violation Code:", violation.code,
      "<br>Number of Violations:", violation.code.count
    )
  )) +
    scale_fill_manual(values = colors_borough) +
    geom_col() +
    facet_grid(. ~ boro) +
    xlab("Violation Type") + 
    ylab("Number of Violations") + 
    guides(fill = guide_legend(title = "Borough")) +
    theme_minimal(base_size = 14) +
    theme(
      plot.background = element_rect(fill = "#f8f9fa"),
      axis.title = element_text(size = 11, face = "plain"),
      axis.text = element_text(size = 10, color = "#555555"),
      legend.title = element_text(size = 11, face = "plain"),
      legend.text = element_text(size = 10),
      panel.grid.major = element_line(color = "#DDDDDD", size = 0.5), 
      panel.grid.minor = element_line(color = "#DDDDDD", size = 0.25),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text())
  
  plot2 <- ggplotly(plot, tooltip = "text")%>%
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
      )
    )
  return(plot2)
}




# 3. MOST COMMON VIOLATIONS (OVERALL) (4/27)

common_violations_func <- function(data) {
  violationmerge <- data %>% # need to clean up violation.descriptions since some entries have extra whitespace, etc.
    select(violation.code, violation.description) %>%
    distinct(violation.code, .keep_all = TRUE)
  
  temp <- data %>%
    filter(critical.flag != 'Not Applicable') %>%
    count(violation.code, name = 'violation.code.count') %>%
    left_join(violationmerge, by = "violation.code") %>% # merging with unique violation description data
    arrange(desc(violation.code.count)) %>%  # ordering by descending order
    slice_head(n = 10) # getting top 10 
  
  return(temp)
}

# plotting from last function
common_violations_plot_func <- function(data) {
  commonviolationsdata <- common_violations_func(data)
  # colors_violations <- c("02B" = "#6D9AC6",  
  #                     "02G" = "#F0A88C",  
  #                     "04A" = "#A1D6B9", 
  #                     "04L" = "#F296B3",   
  #                     "04N" = "#C89BCC", 
  #                     "06C" = "insert color here", 
  #                     "06D" = "insert color here",
  #                     "08A" = "insert color here",
  #                     "10B" = "insert color here", 
  #                     "10F" = "insert color here")
  
  plot <- ggplot(commonviolationsdata, aes(
    x = reorder(violation.code, violation.code.count), 
    y = violation.code.count, 
    fill = violation.code, 
    text = paste(
      "Violation Code Description:", violation.description,
      "<br>Violation Code:", violation.code,
      "<br>Number of Violations:", violation.code.count
    )
  )) +
    # scale_fill_manual(values = colors_violations) +
    geom_col() +
    coord_flip() +
    xlab("Violation Type") + 
    ylab("Number of Violations") + 
    guides(fill = guide_legend(title = "Violation Code")) +
    theme_minimal(base_size = 14) +
    theme(
      plot.background = element_rect(fill = "#f8f9fa"),
      axis.title = element_text(size = 11, face = "plain"),
      axis.text = element_text(size = 10, color = "#555555"),
      legend.title = element_text(size = 11, face = "plain"),
      legend.text = element_text(size = 10),
      panel.grid.major = element_line(color = "#DDDDDD", size = 0.5), 
      panel.grid.minor = element_line(color = "#DDDDDD", size = 0.25),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text())
  
  plot2 <- ggplotly(plot, tooltip = "text")%>%
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
      )
    )
  return(plot2)
}





# 4. MOST COMMON CRITICAL VIOLATIONS (OVERALL) (4/27)


# getting top 10 most common critical violations 
critical_violations_func <- function(data) {
  violationmerge <- data %>% # need to clean up violation.descriptions since some entries have extra whitespace, etc.
    select(violation.code, violation.description) %>%
    distinct(violation.code, .keep_all = TRUE)
  
  temp <- data %>%
    filter(critical.flag == 'Critical') %>%
    count(violation.code, name = 'violation.code.count') %>%
    left_join(violationmerge, by = "violation.code") %>% # merging with unique violation description data
    arrange(desc(violation.code.count)) %>%  # ordering by descending order
    slice_head(n = 10) # getting top 10 criticals
  
  return(temp)
}


# plotting from last function
critical_violations_plot_func <- function(data) {
  criticalviolationsdata <- critical_violations_func(data)
  # colors_violations <- c("02B" = "#6D9AC6",  
  #                     "02G" = "#F0A88C",  
  #                     "04A" = "#A1D6B9", 
  #                     "04H" = "#F296B3",   
  #                     "04L" = "#C89BCC", 
  #                     "04M" = "insert color here", 
  #                     "04N" = "insert color here",
  #                     "06A" = "insert color here",
  #                     "06C" = "insert color here", 
  #                     "06D" = "insert color here")
  
  plot <- ggplot(criticalviolationsdata, aes(
    x = reorder(violation.code, violation.code.count), 
    y = violation.code.count, 
    fill = violation.code, 
    text = paste(
      "Violation Code Description:", violation.description,
      "<br>Violation Code:", violation.code,
      "<br>Number of Violations:", violation.code.count
    )
  )) +
    # scale_fill_manual(values = colors_violations) +
    geom_col() +
    coord_flip() +
    xlab("Critical Violation Type") + 
    ylab("Number of Violations Flagged As Critical") + 
    guides(fill = guide_legend(title = "Critical Violation Code")) +
    theme_minimal(base_size = 14) +
    theme(
      plot.background = element_rect(fill = "#f8f9fa"),
      axis.title = element_text(size = 11, face = "plain"),
      axis.text = element_text(size = 10, color = "#555555"),
      legend.title = element_text(size = 11, face = "plain"),
      legend.text = element_text(size = 10),
      panel.grid.major = element_line(color = "#DDDDDD", size = 0.5), 
      panel.grid.minor = element_line(color = "#DDDDDD", size = 0.25),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text())
  
  plot2 <- ggplotly(plot, tooltip = "text")%>%
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
      )
    )
  return(plot2)
}


# 5. TOP 10 MOST COMMON CRITICAL VIOLATIONS PER BOROUGH (4/27)
crit_borough_violations_func <- function(data) {
  violationmerge <- data %>% # need to clean up violation.descriptions since some entries have extra whitespace, etc.
    select(violation.code, violation.description) %>%
    distinct(violation.code, .keep_all = TRUE)
  
  temp <- data %>%
    filter(critical.flag == 'Critical') %>%
    count(boro, violation.code, name = 'violation.code.count') %>%
    left_join(violationmerge, by = "violation.code") %>% # merging with unique violation description data
    group_by(boro) %>%
    arrange(desc(violation.code.count)) %>%  # ordering by descending order
    slice_head(n = 10) # getting top 10 per borough
  
  return(temp)
}



# plotting from last function 
crit_borough_violations_plot_func <- function(data) {
  critboroughviolationsdata <- crit_borough_violations_func(data)
  colors_borough <- c("Manhattan" = "#6D9AC6",  
                      "Brooklyn" = "#F0A88C",  
                      "Queens" = "#A1D6B9", 
                      "Bronx" = "#F296B3",   
                      "Staten Island" = "#C89BCC")
  
  plot <- ggplot(critboroughviolationsdata, aes(
    x = reorder(violation.code, violation.code.count), 
    y = violation.code.count, 
    fill = boro, 
    text = paste(
      "Violation Code Description:", violation.description,
      "<br>Borough:", boro,
      "<br>Violation Code:", violation.code,
      "<br>Number of Violations:", violation.code.count
    )
  )) +
    scale_fill_manual(values = colors_borough) +
    geom_col() +
    facet_grid(. ~ boro) +
    xlab("Critical Violation Type") + 
    ylab("Number of Critical Violations") + 
    guides(fill = guide_legend(title = "Borough")) +
    theme_minimal(base_size = 14) +
    theme(
      plot.background = element_rect(fill = "#f8f9fa"),
      axis.title = element_text(size = 11, face = "plain"),
      axis.text = element_text(size = 10, color = "#555555"),
      legend.title = element_text(size = 11, face = "plain"),
      legend.text = element_text(size = 10),
      panel.grid.major = element_line(color = "#DDDDDD", size = 0.5), 
      panel.grid.minor = element_line(color = "#DDDDDD", size = 0.25),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text())
  
  plot2 <- ggplotly(plot, tooltip = "text")%>%
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
      )
    )
  return(plot2)
}





# 6. HOW MANY RESTAURANTS ARE THERE PER BOROUGH? (4/27)

borough_restaurants_func <- function(data) {
  temp <- data %>%
    filter(boro != '0') %>% # for some reason there are ones without any boroughs? I think these get filtered out in our other graphs regardless so whatever
    count(boro, name = 'restaurant.count') %>% 
    return(temp)
}

# plotting from last function 
borough_restaurants_plot_func <- function(data) {
  boroughrestaurantsdata <- borough_restaurants_func(data)
  
  colors_borough <- c("Manhattan" = "#6D9AC6",  
                      "Brooklyn" = "#F0A88C",  
                      "Queens" = "#A1D6B9", 
                      "Bronx" = "#F296B3",   
                      "Staten Island" = "#C89BCC")
  
  graphtemp <- ggplot(boroughrestaurantsdata, aes(x = reorder(boro, restaurant.count), y = restaurant.count, fill = boro)) +
    geom_col(aes(text = paste(
      "Restaurant Count: ", restaurant.count
    ))) +
    scale_fill_manual(values = colors_borough) +
    coord_flip() +
    xlab("Borough") + ylab("Number of restaurants") +
    guides(fill = guide_legend(title = "Borough")) + # getting legend 
    theme_minimal(base_size = 14) +
    theme(
      plot.background = element_rect(fill = "#f8f9fa"),
      axis.title = element_text(size = 11, face = "plain"),
      axis.text = element_text(size = 10, color = "#555555"),
      legend.title = element_text(size = 11, face = "plain"),
      legend.text = element_text(size = 10),
      panel.grid.major = element_line(color = "#DDDDDD", size = 0.5), 
      panel.grid.minor = element_line(color = "#DDDDDD", size = 0.25),
      axis.text.x = element_text(),
      axis.text.y = element_text())
  
  graph <- ggplotly(graphtemp, tooltip = "text") %>%
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
      )
    )
  return(graph)
}







