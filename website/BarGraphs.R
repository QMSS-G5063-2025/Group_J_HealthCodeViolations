library(dplyr)
library(tidytext)
library(scales)

# 1. WHICH BOROUGHS HAD THE MOST VIOLATIONS? (4/26)

borough_violations_func <- function(data) {
  temp <- data %>%
    filter(critical.flag != 'Not Applicable') %>% 
    dplyr::count(boro) %>% 
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



# 2. MOST COMMON VIOLATIONS BY BOROUGH (TOP 5) (4/26)

borough_common_violations_func <- function(data) {
  # violationmerge <- data %>% # need to clean up violation.descriptions since some entries have extra whitespace, etc.
  #   select(violation.code, violation.description) %>%
  #   distinct(violation.code, .keep_all = TRUE)
  
  # add in categories
  data <- data %>%
    filter(violation.description != '') %>%
    filter(critical.flag != 'Not Applicable') %>%
    mutate(
      violation.category = case_when(
        grepl("temperature|cold|hot|refrigerator|holding|thermometer", tolower(violation.description)) ~ "Temperature Control",
        grepl("rodent|mice|rat|flies|roaches|insects|vermin|pests", tolower(violation.description)) ~ "Pest Infestation",
        grepl("clean|dirty|filth|unsanit|sanitize|soil|grease|grime", tolower(violation.description)) ~ "Cleanliness & Hygiene",
        grepl("contaminat|exposed|protect|cover|storage|cross-contam", tolower(violation.description)) ~ "Food Protection",
        grepl("plumbing|sink|toilet|water|sewage|leak|drain", tolower(violation.description)) ~ "Plumbing & Facilities",
        grepl("chemical|toxic|poison|hazardous|cleaning agent", tolower(violation.description)) ~ "Chemical Safety",
        grepl("equipment|utensil|dishwasher|surface|cutting board", tolower(violation.description)) ~ "Equipment Maintenance",
        grepl("employee|handwash|glove|hygiene|handling", tolower(violation.description)) ~ "Employee Practices",
        grepl("license|permit|record|certificat|signage|posting", tolower(violation.description)) ~ "Administrative Compliance",
        TRUE ~ "Other"
      )
    )
  
  temp <- data %>%
    filter(critical.flag != 'Not Applicable') %>%
    dplyr::count(boro, violation.category, name = 'violation.category.count') %>%
   # left_join(violationmerge, by = "violation.code") %>% # merging with unique violation description data
    group_by(boro) %>%
    arrange(desc(violation.category.count)) %>%  # ordering by descending order
    slice_head(n = 5) # getting top 5 per borough
  
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
    x = reorder(violation.category, violation.category.count), 
    y = violation.category.count, 
    fill = boro, 
    text = paste(
      "<br>Number of Violations:", violation.category.count
    )
  )) +
    scale_fill_manual(values = colors_borough) +
    geom_col() +
    facet_grid(~ boro) +
    xlab("Violation Type") + 
    ylab("Number of Violations") + 
    guides(fill = "none") +
    theme_minimal(base_size = 14) +
    theme(
      panel.spacing = unit(0.2, "lines"),
      plot.background = element_rect(fill = "#f8f9fa"),
      axis.title = element_text(size = 11, face = "plain"),
      axis.text = element_text(size = 9, color = "#555555"),
      legend.title = element_text(size = 11, face = "plain"),
      legend.text = element_text(size = 10),
      panel.grid.major = element_line(color = "#DDDDDD", linewidth = 0.5), 
      panel.grid.minor = element_line(color = "#DDDDDD", linewidth = 0.25),
      axis.title.x = element_text(margin = margin(t = 40)),
      axis.text.x = element_text(angle = 25, hjust = 1),
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
  # violationmerge <- data %>% # need to clean up violation.descriptions since some entries have extra whitespace, etc.
  #   select(violation.code, violation.description) %>%
  #   distinct(violation.code, .keep_all = TRUE)
  
  # add in categories
  data <- data %>%
    filter(violation.description != '') %>%
    filter(critical.flag != 'Not Applicable') %>%
    mutate(
      violation.category = case_when(
        grepl("temperature|cold|hot|refrigerator|holding|thermometer", tolower(violation.description)) ~ "Temperature Control",
        grepl("rodent|mice|rat|flies|roaches|insects|vermin|pests", tolower(violation.description)) ~ "Pest Infestation",
        grepl("clean|dirty|filth|unsanit|sanitize|soil|grease|grime", tolower(violation.description)) ~ "Cleanliness & Hygiene",
        grepl("contaminat|exposed|protect|cover|storage|cross-contam", tolower(violation.description)) ~ "Food Protection",
        grepl("plumbing|sink|toilet|water|sewage|leak|drain", tolower(violation.description)) ~ "Plumbing & Facilities",
        grepl("chemical|toxic|poison|hazardous|cleaning agent", tolower(violation.description)) ~ "Chemical Safety",
        grepl("equipment|utensil|dishwasher|surface|cutting board", tolower(violation.description)) ~ "Equipment Maintenance",
        grepl("employee|handwash|glove|hygiene|handling", tolower(violation.description)) ~ "Employee Practices",
        grepl("license|permit|record|certificat|signage|posting", tolower(violation.description)) ~ "Administrative Compliance",
        TRUE ~ "Other"
      )
    )
  
  temp <- data %>%
    filter(critical.flag != 'Not Applicable') %>%
    dplyr::count(violation.category, name = 'violation.category.count') %>%
    #left_join(violationmerge, by = "violation.code") %>% # merging with unique violation description data
    arrange(desc(violation.category.count)) %>%  # ordering by descending order
    slice_head(n = 5) # getting top 5 
  
  return(temp)
}

# plotting from last function
common_violations_plot_func <- function(data) {
  commonviolationsdata <- common_violations_func(data)
  colors_violations <- c("Pest Infestation" = "#6D9AC6",  
                         "Temperature Control" = "#F0A88C",  
                         "Cleanliness & Hygiene" = "#A1D6B9", 
                         "Plumbing & Facilities" = "#F296B3",   
                         "Other" = "#C89BCC")
  
  plot <- ggplot(commonviolationsdata, aes(
    x = reorder(violation.category, violation.category.count), 
    y = violation.category.count, 
    fill = violation.category, 
    text = paste(
      "<br>Number of Violations:", violation.category.count
    )
  )) +
    scale_fill_manual(values = colors_violations) +
    geom_col() +
    coord_flip() +
    xlab("Violation Type") + 
    ylab("Number of Violations") + 
    guides(fill = "none") +
    theme_minimal(base_size = 14) +
    theme(
      plot.background = element_rect(fill = "#f8f9fa"),
      axis.title = element_text(size = 11, face = "plain"),
      axis.text = element_text(size = 10, color = "#555555"),
      legend.title = element_text(size = 11, face = "plain"),
      legend.text = element_text(size = 10),
      panel.grid.major = element_line(color = "#DDDDDD", linewidth = 0.5), 
      panel.grid.minor = element_line(color = "#DDDDDD", linewidth = 0.25),
      #axis.text.x = element_text(angle = 25, hjust = 1),
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


# getting top 5 most common critical violations 
critical_violations_func <- function(data) {
  # violationmerge <- data %>% # need to clean up violation.descriptions since some entries have extra whitespace, etc.
  #   select(violation.code, violation.description) %>%
  #   distinct(violation.code, .keep_all = TRUE)
  
  # add in categories
  data <- data %>%
    filter(violation.description != '') %>%
    filter(critical.flag != 'Not Applicable') %>%
    mutate(
      violation.category = case_when(
        grepl("temperature|cold|hot|refrigerator|holding|thermometer", tolower(violation.description)) ~ "Temperature Control",
        grepl("rodent|mice|rat|flies|roaches|insects|vermin|pests", tolower(violation.description)) ~ "Pest Infestation",
        grepl("clean|dirty|filth|unsanit|sanitize|soil|grease|grime", tolower(violation.description)) ~ "Cleanliness & Hygiene",
        grepl("contaminat|exposed|protect|cover|storage|cross-contam", tolower(violation.description)) ~ "Food Protection",
        grepl("plumbing|sink|toilet|water|sewage|leak|drain", tolower(violation.description)) ~ "Plumbing & Facilities",
        grepl("chemical|toxic|poison|hazardous|cleaning agent", tolower(violation.description)) ~ "Chemical Safety",
        grepl("equipment|utensil|dishwasher|surface|cutting board", tolower(violation.description)) ~ "Equipment Maintenance",
        grepl("employee|handwash|glove|hygiene|handling", tolower(violation.description)) ~ "Employee Practices",
        grepl("license|permit|record|certificat|signage|posting", tolower(violation.description)) ~ "Administrative Compliance",
        TRUE ~ "Other"
      )
    )
  
  temp <- data %>%
    filter(critical.flag == 'Critical') %>%
    dplyr::count(violation.category, name = 'violation.category.count') %>%
    #left_join(violationmerge, by = "violation.code") %>% # merging with unique violation description data
    arrange(desc(violation.category.count)) %>%  # ordering by descending order
    slice_head(n = 5) # getting top 5 criticals
  
  return(temp)
}


# plotting from last function
critical_violations_plot_func <- function(data) {
  criticalviolationsdata <- critical_violations_func(data)
  colors_violations <- c("Pest Infestation" = "#6D9AC6",  
                         "Cleanliness & Hygiene" = "#F0A88C",  
                         "Temperature Control" = "#A1D6B9", 
                         "Equipment Maintenance" = "#F296B3",   
                         "Other" = "#C89BCC")
  
  plot <- ggplot(criticalviolationsdata, aes(
    x = reorder(violation.category, violation.category.count), 
    y = violation.category.count, 
    fill = violation.category, 
    text = paste(
      "<br>Number of Violations:", violation.category.count
    )
  )) +
    scale_fill_manual(values = colors_violations) +
    geom_col() +
    coord_flip() +
    xlab("Critical Violation Type") + 
    ylab("Number of Violations") + 
    guides(fill = "none") +
    theme_minimal(base_size = 14) +
    theme(
      plot.background = element_rect(fill = "#f8f9fa"),
      axis.title = element_text(size = 11, face = "plain"),
      axis.text = element_text(size = 10, color = "#555555"),
      legend.title = element_text(size = 11, face = "plain"),
      legend.text = element_text(size = 10),
      panel.grid.major = element_line(color = "#DDDDDD", linewidth = 0.5), 
      panel.grid.minor = element_line(color = "#DDDDDD", linewidth = 0.25),
      #axis.text.x = element_text(angle = 45, hjust = 1),
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


# 5. TOP 5 MOST COMMON CRITICAL VIOLATIONS PER BOROUGH (4/27)
crit_borough_violations_func <- function(data) {
  # violationmerge <- data %>% # need to clean up violation.descriptions since some entries have extra whitespace, etc.
  #   select(violation.code, violation.description) %>%
  #   distinct(violation.code, .keep_all = TRUE)
  
  # add in categories
  data <- data %>%
    filter(violation.description != '') %>%
    filter(critical.flag != 'Not Applicable') %>%
    mutate(
      violation.category = case_when(
        grepl("temperature|cold|hot|refrigerator|holding|thermometer", tolower(violation.description)) ~ "Temperature Control",
        grepl("rodent|mice|rat|flies|roaches|insects|vermin|pests", tolower(violation.description)) ~ "Pest Infestation",
        grepl("clean|dirty|filth|unsanit|sanitize|soil|grease|grime", tolower(violation.description)) ~ "Cleanliness & Hygiene",
        grepl("contaminat|exposed|protect|cover|storage|cross-contam", tolower(violation.description)) ~ "Food Protection",
        grepl("plumbing|sink|toilet|water|sewage|leak|drain", tolower(violation.description)) ~ "Plumbing & Facilities",
        grepl("chemical|toxic|poison|hazardous|cleaning agent", tolower(violation.description)) ~ "Chemical Safety",
        grepl("equipment|utensil|dishwasher|surface|cutting board", tolower(violation.description)) ~ "Equipment Maintenance",
        grepl("employee|handwash|glove|hygiene|handling", tolower(violation.description)) ~ "Employee Practices",
        grepl("license|permit|record|certificat|signage|posting", tolower(violation.description)) ~ "Administrative Compliance",
        TRUE ~ "Other"
      )
    )
  
  temp <- data %>%
    filter(critical.flag == 'Critical') %>%
    dplyr::count(boro, violation.category, name = 'violation.category.count') %>%
    #left_join(violationmerge, by = "violation.code") %>% # merging with unique violation description data
    group_by(boro) %>%
    arrange(desc(violation.category.count)) %>%  # ordering by descending order
    slice_head(n = 5) # getting top 10 per borough
  
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
    x = reorder(violation.category, violation.category.count), 
    y = violation.category.count, 
    fill = boro, 
    text = paste(
      "<br>Number of Violations:", violation.category.count
    )
  )) +
    scale_fill_manual(values = colors_borough) +
    geom_col() +
    facet_grid(. ~ boro, scales = "free_x") +
    xlab("Critical Violation Type") + 
    ylab("Number of Critical Violations") + 
    guides(fill = "none") +
    theme_minimal(base_size = 14) +
    theme(
      plot.background = element_rect(fill = "#f8f9fa"),
      axis.title = element_text(size = 11, face = "plain"),
      axis.text = element_text(size = 9, color = "#555555"),
      legend.title = element_text(size = 11, face = "plain"),
      legend.text = element_text(size = 10),
      panel.grid.major = element_line(color = "#DDDDDD", linewidth = 0.5), 
      panel.grid.minor = element_line(color = "#DDDDDD", linewidth = 0.25),
      axis.title.x = element_text(margin = margin(t = 35)),
      axis.text.x = element_text(angle = 25, hjust = 1),
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
    dplyr::count(boro, name = 'restaurant.count') %>% 
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
      plot.title = element_text(size = 13, color = "#333333", hjust=0.5),
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
    dplyr::count(dba, building, street, zipcode, cuisine.description, latitude, longitude, address, name = 'hcv_count')
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
    dplyr::count(cuisine.description, name = 'cuisine.count') %>% 
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
    guides(fill = "none") + # getting legend 
    theme_minimal(base_size = 14) +
    theme(
      plot.background = element_rect(fill = "#f8f9fa"),
      axis.title = element_text(size = 11, face = "plain"),
      axis.text = element_text(size = 10, color = "#555555"),
      #legend.title = element_text(size = 11, face = "plain"),
      #legend.text = element_text(size = 10),
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
    dplyr::count(boro, cuisine.description, name = 'cuisine.count') %>%
    group_by(boro) %>%
    arrange(desc(cuisine.count)) %>%  # ordering by descending order
    slice_head(n = 10) # getting top 10 per borough
  return(temp)
}

# so i can use a drop down and have a shorter page
borough_common_cuisines_func_2 <- function(data, target) {
  temp <- data %>%
    filter(cuisine.description != '') %>%
    filter(boro==target) %>%
    distinct(dba, boro, cuisine.description, latitude, longitude, street, building, zipcode) %>% # counting distinct restaurants and therefore cuisine descriptions
    dplyr::count(boro, cuisine.description, name = 'cuisine.count') %>%
    group_by(boro) %>%
    arrange(desc(cuisine.count)) %>%  # ordering by descending order
    slice_head(n = 10) # getting top 10 per borough
  return(temp)
}

# plotting from last function 
borough_common_cuisines_plot_func <- function(data, region) {
  boroughcommoncuisinesdata <- borough_common_cuisines_func_2(data, region)
  colors_borough <- c("Manhattan" = "#6D9AC6",
                      "Brooklyn" = "#F0A88C",
                      "Queens" = "#A1D6B9",
                      "Bronx" = "#F296B3",
                      "Staten Island" = "#C89BCC")
  
  plot <- ggplot(boroughcommoncuisinesdata, aes(
    x = reorder(cuisine.description, cuisine.count), 
    y = cuisine.count, 
    fill = boro, 
    text = paste(
      "<br>Number of Restaurants:", cuisine.count
    )
  )) +
    geom_col(fill = colors_borough[region]) +
    xlab("Cuisine Type") + 
    ylab("Number of Restaurants") + 
    coord_flip() +
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
      axis.text.x = element_text(size = 10, hjust = 1),
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
    dplyr::count(cuisine.description, name = 'cuisine.count') %>% 
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
    guides(fill = "none") + # getting legend 
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
  # violationmerge <- data %>% # need to clean up violation.descriptions since some entries have extra whitespace, etc.
  #   dplyr::select(violation.code, violation.description) %>%
  #   distinct(violation.code, .keep_all = TRUE)
  
  # add in categories
  data <- data %>%
    filter(violation.description != '') %>%
    filter(critical.flag != 'Not Applicable') %>%
    mutate(
      violation.category = case_when(
        grepl("temperature|cold|hot|refrigerator|holding|thermometer", tolower(violation.description)) ~ "Temperature Control",
        grepl("rodent|mice|rat|flies|roaches|insects|vermin|pests", tolower(violation.description)) ~ "Pest Infestation",
        grepl("clean|dirty|filth|unsanit|sanitize|soil|grease|grime", tolower(violation.description)) ~ "Cleanliness & Hygiene",
        grepl("contaminat|exposed|protect|cover|storage|cross-contam", tolower(violation.description)) ~ "Food Protection",
        grepl("plumbing|sink|toilet|water|sewage|leak|drain", tolower(violation.description)) ~ "Plumbing & Facilities",
        grepl("chemical|toxic|poison|hazardous|cleaning agent", tolower(violation.description)) ~ "Chemical Safety",
        grepl("equipment|utensil|dishwasher|surface|cutting board", tolower(violation.description)) ~ "Equipment Maintenance",
        grepl("employee|handwash|glove|hygiene|handling", tolower(violation.description)) ~ "Employee Practices",
        grepl("license|permit|record|certificat|signage|posting", tolower(violation.description)) ~ "Administrative Compliance",
        TRUE ~ "Other"
      )
    )
  
  topcuisines <- data %>%
    filter(cuisine.description != '') %>%
    filter(critical.flag != 'Not Applicable') %>% 
    # distinct(dba, boro, cuisine.description, latitude, longitude, street, building, zipcode) %>%
    dplyr::count(cuisine.description, name = 'cuisine.count') %>% 
    arrange(desc(cuisine.count)) %>%
    slice_head(n = 5) # getting top 5 for cuisine 
  
  temp <- data %>%
    filter(cuisine.description != '') %>%
    filter(critical.flag != 'Not Applicable') %>%
    dplyr::count(cuisine.description, violation.category, name = 'violation.category.count') %>%
    #left_join(violationmerge, by = "violation.code") %>% # merging with unique violation description data
    filter(cuisine.description %in% topcuisines$cuisine.description) %>%
    group_by(cuisine.description) %>%
    arrange(desc(violation.category.count)) %>%  # ordering by descending order
    slice_head(n = 5) # getting top 10 per borough
  
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
    x = reorder_within(violation.category, violation.category.count, cuisine.description), 
    y = violation.category.count, 
    fill = cuisine.description, 
    text = paste(
      "<br>Number of Violations:", violation.category.count
    )
  )) +
    scale_fill_manual(values = colors_cuisine) +
    geom_col() +
    facet_wrap(~ cuisine.description, scales = "free_x", nrow=1) +  
    scale_x_reordered() +
    xlab("Violation Type") + 
    ylab("Number of Violations") + 
    guides(fill = "none") +
    theme_minimal(base_size = 14) +
    theme(
      plot.background = element_rect(fill = "#f8f9fa"),
      axis.title = element_text(size = 11, face = "plain"),
      axis.text = element_text(size = 9, color = "#555555"),
      legend.title = element_text(size = 11, face = "plain"),
      legend.text = element_text(size = 10),
      panel.grid.major = element_line(color = "#DDDDDD", size = 0.5), 
      panel.grid.minor = element_line(color = "#DDDDDD", size = 0.25),
      axis.title.x = element_text(margin = margin(t = 35)),
      axis.text.x = element_text(angle = 25, hjust = 1),
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
    labs(title = "Rates of Health Code Violations Issued per Restaurant in Each Borough") +
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



