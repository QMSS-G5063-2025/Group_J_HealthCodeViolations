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
    xlab("Borough") + ylab("Number of Violations") +
    guides(fill = FALSE) +
    theme_minimal(base_size = 14) +
    labs(title = "Number of Health Code Violations by Borough") +
    theme(
          plot.background = element_rect(fill = "#f8f9fa"),
          axis.title = element_text(size = 11, face = "plain"),
          axis.text = element_text(size = 10, color = "#555555"),
          legend.title = element_text(size = 11, face = "plain"),
          legend.text = element_text(size = 10),
          panel.grid.major = element_line(color = "#DDDDDD", linewidth = 0.5), 
          panel.grid.minor = element_line(color = "#DDDDDD", linewidth = 0.25),
          axis.text.x = element_text(),
          axis.text.y = element_text(),
          plot.title = element_text(size = 13, hjust = 0.5))
  
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
    facet_grid(~ boro) +
    xlab("Violation Type") + 
    ylab("Number of Violations") + 
    guides(fill = guide_legend(title = "Borough")) +
    theme_minimal(base_size = 14) +
    theme(
      panel.spacing = unit(0.2, "lines"),
      plot.background = element_rect(fill = "#f8f9fa"),
      axis.title = element_text(size = 11, face = "plain"),
      axis.text = element_text(size = 10, color = "#555555"),
      legend.title = element_text(size = 11, face = "plain"),
      legend.text = element_text(size = 10),
      panel.grid.major = element_line(color = "#DDDDDD", linewidth = 0.5), 
      panel.grid.minor = element_line(color = "#DDDDDD", linewidth = 0.25),
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
   colors_violations <- c("02B" = "#6D9AC6",  
                       "02G" = "#F0A88C",  
                       "04A" = "#A1D6B9", 
                       "04L" = "#F296B3",   
                       "04N" = "#C89BCC", 
                       "06C" = "#F4A261", 
                       "06D" = "#FF8F85",
                       "08A" = "#19758A",
                       "10B" = "#B6C8E2", 
                       "10F" = "#B690A1")
  
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
    scale_fill_manual(values = colors_violations) +
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
      panel.grid.major = element_line(color = "#DDDDDD", linewidth = 0.5), 
      panel.grid.minor = element_line(color = "#DDDDDD", linewidth = 0.25),
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
  colors_violations <- c("02B" = "#6D9AC6",  
                       "02G" = "#F0A88C",  
                       "04A" = "#A1D6B9", 
                       "04H" = "#F296B3",   
                       "04L" = "#C89BCC", 
                       "04M" = "#F4A261", 
                       "04N" = "#FF8F85",
                       "06A" = "#19758A",
                       "06C" = "#B6C8E2", 
                       "06D" = "#B690A1")
  
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
    scale_fill_manual(values = colors_violations) +
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
      panel.grid.major = element_line(color = "#DDDDDD", linewidth = 0.5), 
      panel.grid.minor = element_line(color = "#DDDDDD", linewidth = 0.25),
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

select_viol <- function(data, type){
  if(type=="overall"){
    viol_graph <- common_violations_plot_func(data)
  }
  
  if(type=="crit"){
    viol_graph <- critical_violations_plot_func(data)
  }
  return(viol_graph)
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
      panel.grid.major = element_line(color = "#DDDDDD", linewidth = 0.5), 
      panel.grid.minor = element_line(color = "#DDDDDD", linewidth = 0.25),
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

select_viol_boro <- function(data, type){
  if(type=="overall"){
    viol_graph_boro <- borough_common_violations_plot_func(data)
  }
  
  if(type=="crit"){
    viol_graph_boro <- crit_borough_violations_plot_func(data)
  }
  return(viol_graph_boro)
}



# 6. HOW MANY RESTAURANTS ARE THERE PER BOROUGH? (4/27)

borough_restaurants_func <- function(data) {
  temp <- data %>%
    filter(boro != '0') %>% # for some reason there are ones without any boroughs? I think these get filtered out in our other graphs regardless so whatever
    distinct(dba, boro, latitude, longitude, street, building, zipcode) %>%
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
    xlab("Borough") + ylab("Number of Restaurants") +
    guides(fill = FALSE) + # getting legend 
    theme_minimal(base_size = 14) +
    theme(
      plot.background = element_rect(fill = "#f8f9fa"),
      axis.title = element_text(size = 11, face = "plain"),
      axis.text = element_text(size = 10, color = "#555555"),
      legend.title = element_text(size = 11, face = "plain"),
      legend.text = element_text(size = 10),
      panel.grid.major = element_line(color = "#DDDDDD", linewidth = 0.5), 
      panel.grid.minor = element_line(color = "#DDDDDD", linewidth = 0.25),
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


# 7. TOP 10 RESTAURANTS PER BOROUGH WITH MOST VIOLATIONS
count_bor <- function(data, target){
  data %>%
    filter(boro==target) %>%
    filter(critical.flag != 'Not Applicable') %>%
    filter(inspection.date != 1900-01-01) %>%
    count(dba, building, street, zipcode, cuisine.description, latitude, longitude, address, name = 'hcv_count')
}

top10_rest <- function(data, region) {
  data$address <- paste(data$building, data$street, data$zipcode)
  # Let's create a new data frame to do so
  hcv_region <- count_bor(data, region)
  top10_region <- hcv_region %>%
    arrange(desc(hcv_count)) %>%
    slice(1:10)
  
  colors_borough <- c("Manhattan" = "#6D9AC6",  
                      "Brooklyn" = "#F0A88C",  
                      "Queens" = "#A1D6B9", 
                      "Bronx" = "#F296B3",   
                      "Staten Island" = "#C89BCC")
  
  plot <- ggplot(top10_region, aes(x = reorder(dba, hcv_count), y = hcv_count)) +
    geom_col(fill = colors_borough[region], aes(text = paste(
      "Violation Count: ", hcv_count
    ))) +
    coord_flip() +
    labs(
      x = "Restaurant",
      y = "Violation Count") +
    theme_minimal(base_size = 14) + 
    theme(
      plot.background = element_rect(fill = "#f8f9fa"),
      plot.title = element_text(size = 13, color = "#333333", hjust=0.5),
      axis.title = element_text(size = 11, face = "light"),
      axis.text = element_text(size = 10, color = "#555555"),
      legend.title = element_text(size = 11, face = "light"),
      legend.text = element_text(size = 10),
      panel.grid.major = element_line(color = "#DDDDDD", linewidth = 0.5), 
      panel.grid.minor = element_line(color = "#DDDDDD", linewidth = 0.25)
    )
  
  #plotly 
  top_10_rest <- ggplotly(plot, tooltip="text") %>%
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
  return(top_10_rest)
}








# 8. TOP TEN MOST COMMON CUISINES OVERALL (5/4)
overall_cuisine_func <- function(data) {
  temp <- data %>%
    filter(cuisine.description != '') %>%
    distinct(dba, boro, cuisine.description, latitude, longitude, street, building, zipcode) %>%
    count(cuisine.description, name = 'cuisine.count') %>% 
    arrange(desc(cuisine.count)) %>%
    slice_head(n = 10)
  return(temp)
}

overall_cuisine_plot_func <- function(data) {
  overall_cuisine_data <- overall_cuisine_func(data)
  
  colors_cuisine <- c("American" = "#6D9AC6",  
                      "Chinese" = "#F0A88C",  
                      "Coffee/Tea" = "#A1D6B9", 
                      "Pizza" = "#F296B3",   
                      "Italian" = "#C89BCC", 
                      "Mexican" = "#F4A261", 
                      "Japanese" = "#FF8F85",
                      "Latin American" = "#19758A",
                      "Bakery Products/Desserts" = "#B6C8E2", 
                      "Caribbean" = "#B690A1")
  
  graphtemp <- ggplot(overall_cuisine_data, aes(x = reorder(cuisine.description, cuisine.count), y = cuisine.count, fill = cuisine.description)) +
    geom_col(aes(text = paste(
      "Restaurant Count: ", cuisine.count
    ))) +
    scale_fill_manual(values = colors_cuisine) +
    coord_flip() +
    xlab("Cuisine Type") + ylab("Number of restaurants") +
    guides(fill = guide_legend(title = "Cuisine Type")) + # getting legend 
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






# 9. MOST COMMON CUISINES BY BOROUGH (5/4)
borough_common_cuisines_func <- function(data) {
  temp <- data %>%
    filter(cuisine.description != '') %>%
    distinct(dba, boro, cuisine.description, latitude, longitude, street, building, zipcode) %>% # counting distinct restaurants and therefore cuisine descriptions
    count(boro, cuisine.description, name = 'cuisine.count') %>%
    group_by(boro) %>%
    arrange(desc(cuisine.count)) %>%  # ordering by descending order
    slice_head(n = 10) # getting top 10 per borough
  return(temp)
}

# plotting from last function 
borough_common_cuisines_plot_func <- function(data) {
  boroughcommoncuisinesdata <- borough_common_cuisines_func(data)
  colors_borough <- c("Manhattan" = "#6D9AC6",
                      "Brooklyn" = "#F0A88C",
                      "Queens" = "#A1D6B9",
                      "Bronx" = "#F296B3",
                      "Staten Island" = "#C89BCC")
  
  plot <- ggplot(boroughcommoncuisinesdata, aes(
    x = reorder_within(cuisine.description, cuisine.count, boro), 
    y = cuisine.count, 
    fill = boro, 
    text = paste(
      "Cuisine Description:", cuisine.description,
      "<br>Borough:", boro,
      "<br>Number of Cuisines:", cuisine.count
    )
  )) +
    scale_fill_manual(values = colors_borough) +
    geom_col() +
    facet_wrap(~ boro, scales = "free_x") +  
    scale_x_reordered() +
    xlab("Cuisine Type") + 
    ylab("Number of Restaurants") + 
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
      axis.text.x = element_text(angle = 70, size = 5, hjust = 1),
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


# 10. CUISINES WITH THE MOST VIOLATIONS (5/4) 
cuisine_violations_func <- function(data) {
  temp <- data %>%
    filter(cuisine.description != '') %>%
    filter(critical.flag != 'Not Applicable') %>% 
    # distinct(dba, boro, cuisine.description, latitude, longitude, street, building, zipcode) %>%
    count(cuisine.description, name = 'cuisine.count') %>% 
    arrange(desc(cuisine.count)) %>%
    slice_head(n = 10) # getting top 10 per borough
  return(temp)
}

cuisine_violations_plot_func <- function(data) {
  cuisine_violations_data <- cuisine_violations_func(data)
  
  colors_cuisine <- c("American" = "#6D9AC6",
                      "Chinese" = "#F0A88C",
                      "Coffee/Tea" = "#A1D6B9",
                      "Pizza" = "#F296B3",
                      "Latin American" = "#C89BCC",
                      "Mexican" = "#F4A261",
                      "Bakery Products/Desserts" = "#FF8F85",
                      "Italian" = "#19758A",
                      "Caribbean" = "#B6C8E2",
                      "Japanese" = "#B690A1")
  
  graphtemp <- ggplot(cuisine_violations_data, aes(x = reorder(cuisine.description, cuisine.count), y = cuisine.count, fill = cuisine.description)) +
    geom_col(aes(text = paste(
      "Violation Count: ", cuisine.count
    ))) +
    scale_fill_manual(values = colors_cuisine) +
    coord_flip() +
    xlab("Cuisine Type") + ylab("Number of violations") +
    guides(fill = guide_legend(title = "Cuisine Type")) + # getting legend 
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



# 11. MOST COMMON VIOLATIONS BY CUISINE (5/4)
cuisine_common_violations_func <- function(data) {
  violationmerge <- data %>% # need to clean up violation.descriptions since some entries have extra whitespace, etc.
    select(violation.code, violation.description) %>%
    distinct(violation.code, .keep_all = TRUE)
  
  topcuisines <- data %>%
    filter(cuisine.description != '') %>%
    filter(critical.flag != 'Not Applicable') %>% 
    # distinct(dba, boro, cuisine.description, latitude, longitude, street, building, zipcode) %>%
    count(cuisine.description, name = 'cuisine.count') %>% 
    arrange(desc(cuisine.count)) %>%
    slice_head(n = 5) # getting top 5 for cuisine 
  
  temp <- data %>%
    filter(cuisine.description != '') %>%
    filter(critical.flag != 'Not Applicable') %>%
    count(cuisine.description, violation.code, name = 'violation.code.count') %>%
    left_join(violationmerge, by = "violation.code") %>% # merging with unique violation description data
    filter(cuisine.description %in% topcuisines$cuisine.description) %>%
    group_by(cuisine.description) %>%
    arrange(desc(violation.code.count)) %>%  # ordering by descending order
    slice_head(n = 10) # getting top 10 per borough
  
  return(temp)
}

# plotting from last function 
cuisine_common_violations_plot_func <- function(data) {
  cuisinecommonviolationsdata <- cuisine_common_violations_func(data)
  colors_cuisine <- c("American" = "#6D9AC6",  
                      "Chinese" = "#F0A88C",  
                      "Coffee/Tea" = "#A1D6B9", 
                      "Pizza" = "#F296B3",   
                      "Latin American" = "#C89BCC")
  
  plot <- ggplot(cuisinecommonviolationsdata, aes(
    x = reorder_within(violation.code, violation.code.count, cuisine.description), 
    y = violation.code.count, 
    fill = cuisine.description, 
    text = paste(
      "Violation Code Description:", violation.description,
      "<br>Cuisine Type:", cuisine.description,
      "<br>Violation Code:", violation.code,
      "<br>Number of Violations:", violation.code.count
    )
  )) +
    scale_fill_manual(values = colors_cuisine) +
    geom_col() +
    facet_wrap(~ cuisine.description, scales = "free_x") +  
    scale_x_reordered() +
    xlab("Violation Type") + 
    ylab("Number of Violations") + 
    guides(fill = guide_legend(title = "Cuisine Type")) +
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


# 12. HEALTH CODE VIOLATIONS PER EACH RESTAURANT IN EACH BOROUGH
borough_1000_violations_func <- function(data) {
  violationsummary <- data %>%
    filter(critical.flag != 'Not Applicable') %>%
    count(boro, name = 'totalviolations')
  
  restaurantsummary <- data %>%
    filter(boro != '0') %>% # for some reason there are ones without any boroughs? I think these get filtered out in our graphs regardless so whatever
    distinct(dba, boro, latitude, longitude, street, building, zipcode) %>%
    count(boro, name = 'uniquerestaurants') 
  
  temp <- violationsummary %>%
    inner_join(restaurantsummary, by = "boro") %>%
    mutate(avginspections = totalviolations/uniquerestaurants) %>%
    arrange(desc(avginspections))
  
  return(temp)
}

# plotting from last function 
borough_1000_violations_plot_func <- function(data) {
  borough1000violations <- borough_1000_violations_func(data)
  
  colors_borough <- c("Manhattan" = "#6D9AC6",  
                      "Brooklyn" = "#F0A88C",  
                      "Queens" = "#A1D6B9", 
                      "Bronx" = "#F296B3",   
                      "Staten Island" = "#C89BCC")
  
  graphtemp <- ggplot(borough1000violations, aes(x = reorder(boro, avginspections), y = avginspections, fill = boro)) +
    geom_col(aes(text = paste(
      "Violation Count: ", avginspections
    ))) +
    scale_fill_manual(values = colors_borough) +
    xlab("Borough") + ylab("Number of Violations Per Restaurant") +
    coord_flip() +
    guides(fill = FALSE) +
    theme_minimal(base_size = 14) +
    labs(title = "Number of Health Code Violations Per Restaurants in each Borough") +
    theme(
      plot.background = element_rect(fill = "#f8f9fa"),
      axis.title = element_text(size = 11, face = "plain"),
      axis.text = element_text(size = 10, color = "#555555"),
      legend.title = element_text(size = 11, face = "plain"),
      legend.text = element_text(size = 10),
      panel.grid.major = element_line(color = "#DDDDDD", linewidth = 0.5), 
      panel.grid.minor = element_line(color = "#DDDDDD", linewidth = 0.25),
      axis.text.x = element_text(),
      axis.text.y = element_text(),
      plot.title = element_text(size = 13, hjust = 0.5))
  
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


