# WHICH BOROUGHS HAD THE MOST VIOLATIONS?

borough_violations_func <- function(data) {
  data %>%
    filter(critical.flag != 'Not Applicable') %>% 
    count(boro) %>% 
    arrange(desc(n))
}

# plotting from last function 
borough_violations_plot_func <- function(boroughviolations) {
  ggplot(boroughviolations, aes(x = reorder(boro, n), y = n, fill = boro)) +
    geom_col() +
    coord_flip() +
    ggtitle("Which boroughs had the most violations?") +
    xlab("Borough") + ylab("Number of violations") + labs(color = "Borough") + 
    theme_minimal()
}






# MOST COMMON VIOLATIONS BY BOROUGH (TOP 10)


borough_common_violations_func <- function(data) {
  violationmerge <- data %>% # need to clean up violation.descriptions since some entries have extra whitespace, etc.
    select(violation.code, violation.description) %>%
    distinct(violation.code, .keep_all = TRUE)
  data %>%
    filter(critical.flag != 'Not Applicable') %>%
    count(boro, violation.code, name = 'violation.code.count') %>%
    left_join(violationmerge, by = "violation.code") %>% # merging with unique violation description data
    group_by(boro) %>%
    arrange(desc(violation.code.count)) %>%  # ordering by descending order
    slice_head(n = 10) # getting top 10 per borough
}


# plotting from last function 
borough_common_violations_plot_func <- function(boroughcommonviolationsdata) {
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
    geom_col() +
    facet_grid(. ~ boro) + 
    ggtitle("What are the most common violations per borough?") +
    xlab("Violation Type") + 
    ylab("Number of Violations") + 
    labs(color = "Borough") +
    theme_minimal()
  
  ggplotly(plot, tooltip = "text")
}



