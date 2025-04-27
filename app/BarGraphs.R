# WHICH BOROUGHS HAD THE MOST VIOLATIONS?

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


# MOST COMMON VIOLATIONS BY BOROUGH (TOP 10)


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

# MOST COMMON CRITICAL VIOLATIONS


