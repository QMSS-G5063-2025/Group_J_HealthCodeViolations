library(leaflet)
library(htmltools)
library(dplyr)

count_bor <- function(data, target){
  data %>%
    filter(boro==target) %>%
    filter(critical.flag != 'Not Applicable') %>%
    filter(inspection.date != 1900-01-01) %>%
    dplyr::count(dba, building, street, zipcode, cuisine.description, latitude, longitude, address, name = 'hcv_count')
}

count_whole <- function(data){
  data %>%
    filter(inspection.date != 1900-01-01)  %>%
    filter(critical.flag != 'Not Applicable') %>%
    dplyr::count(dba, building, street, zipcode, cuisine.description, latitude, longitude, address, name = 'hcv_count')
}
create_map <- function(region_data){
  m <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    setView(lng = -74.0060, lat = 40.7128, zoom = 12)
  
  m1 <- m %>% addAwesomeMarkers(
    data = region_data,
    lng = ~longitude, 
    lat = ~latitude,
    clusterOptions = markerClusterOptions(),
    popup = ~paste(
      "<h4>", dba, "</h4>",
      "<b>Address:</b> ", address, "<br>",
      "<b>Cuisine Type:</b> ", cuisine.description, "<br>",
      "<b>Total Violation Count:</b> ", hcv_count, "<br>"
    ),
    label = ~dba,
    icon = awesomeIcons(
      icon = 'info-circle',  # Valid FontAwesome icon
      markerColor = ifelse(region_data$hcv_count > 0 & region_data$hcv_count <= 20, "beige", 
                           ifelse(
                             region_data$hcv_count >= 21 & region_data$hcv_count <= 40, "orange",
                             ifelse(
                               region_data$hcv_count >= 41 & region_data$hcv_count <= 60, "red",
                               "darkred"))),
      library = 'fa'  # Use FontAwesome instead of glyphicon
    )
  ) %>% addLegend( 
    'bottomright' , 
    colors=c("beige","orange", "red", "darkred"),
    labels=c('1-20','21-40', '41-60', '61+'),
    title = "Total Number of Violations"
  )
  return(m1)
}

# MAIN FUNCTION FOR CREATING MAPS OF TOP 500 WITH MOST VIOLATIONS
map_allviolations <- function(data, borough){
  # create sub data
  data$address <- paste(data$building, data$street, data$zipcode)
  hcv_man <- count_bor(data, 'Manhattan') %>%
    arrange(desc(hcv_count)) %>%
    slice(1:500)
  hcv_bronx <- count_bor(data, 'Bronx') %>%
    arrange(desc(hcv_count)) %>%
    slice(1:500)
  hcv_brook <- count_bor(data, 'Brooklyn') %>%
    arrange(desc(hcv_count)) %>%
    slice(1:500)
  hcv_queen <- count_bor(data, 'Queens') %>%
    arrange(desc(hcv_count)) %>%
    slice(1:500)
  hcv_stat <- count_bor(data, 'Staten Island') %>%
    arrange(desc(hcv_count)) %>%
    slice(1:500)
  hcv_total <- count_whole(data) %>%
    arrange(desc(hcv_count)) %>%
    slice(1:500)
  
  # Map
  if(borough=='nyc'){
    map_res <- create_map(hcv_total)
  }
  
  if(borough=='brook'){
    map_res <- create_map(hcv_brook)
  }
  if(borough=='queen'){
    map_res <- create_map(hcv_queen)
  }
  
  if(borough=='bronx'){
    map_res <- create_map(hcv_bronx)
  }
  if(borough=='man'){
    map_res <- create_map(hcv_man)
  }
  
  if(borough=='stat'){
    map_res <- create_map(hcv_stat)
  }
  return(map_res)
}

count_whole2 <- function(data){
  data %>%
    filter(inspection.date != 1900-01-01) %>%
    filter(critical.flag != 'Not Applicable') %>%
    dplyr::count(violation.description, critical.flag, name = 'viol_count')
}

count_whole3 <- function(data){
  data %>%
    dplyr::count(dba, building, street, zipcode, cuisine.description, latitude, longitude, violation.description, address, name = 'v_count')
}

top10_viol <- function(data){
  violations <- count_whole2(data) %>%
    filter(critical.flag == 'Critical') %>%
    arrange(desc(viol_count)) %>%
    slice(1:10)
  return(violations)
}

create_violmap <- function(viol_data){
  v <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    setView(lng = -74.0060, lat = 40.7128, zoom = 12)
  
  v1 <- v %>% addAwesomeMarkers(
    data = viol_data,
    lng = ~longitude, 
    lat = ~latitude,
    clusterOptions = markerClusterOptions(),
    popup = ~paste(
      "<h4>", dba, "</h4>",
      "<b>Address:</b> ", address, "<br>",
      "<b>Cusisine Type:</b> ", cuisine.description, "<br>",
      "<b>Violation:</b> ", violation.description, "<br>",
      "<b>Total Violation Count:</b> ", v_count, "<br>"
    ),
    label = ~dba,
    icon = awesomeIcons(
      icon = 'info-circle',  # Valid FontAwesome icon
      markerColor = ifelse(viol_data$v_count >= 1 & viol_data$v_count <= 2, "beige", 
                           ifelse(
                             viol_data$v_count >= 3 & viol_data$v_count <= 4, "orange",
                             ifelse(
                               viol_data$v_count >= 5 & viol_data$v_count <= 6, "red",
                               "darkred"))),
      library = 'fa'  # Use FontAwesome instead of glyphicon
    )
  ) %>% addLegend( 
    'bottomright' , 
    colors=c("beige","orange", "red", "darkred"),
    labels=c('1-2','3-4', '5-6', '7+'),
    title = "Total Number of Violations"
  )
  return(v1)
}

# MAIN FUNCTION FOR MAPPING BY VIOLATION TYPE
map_critviolations <- function(data, violation){
  data$address <- paste(data$building, data$street, data$zipcode)
  
  #grab top 10 violations
  violations <- top10_viol(data)
  violations_list <- violations$violation.description
  
  # Let's create lists for restaurants that have these violations
  viol1 <- data %>%
    filter(violation.description == violations_list[1])
  viol2 <- data%>%
    filter(violation.description == violations_list[2])
  viol3 <- data %>%
    filter(violation.description == violations_list[3])
  viol4 <- data %>%
    filter(violation.description == violations_list[4])
  viol5 <- data %>%
    filter(violation.description == violations_list[5])
  viol6 <- data %>%
    filter(violation.description == violations_list[6])
  viol7 <- data %>%
    filter(violation.description == violations_list[7])
  viol8 <- data %>%
    filter(violation.description == violations_list[8])
  viol9 <- data %>%
    filter(violation.description == violations_list[9])
  viol10 <- data %>%
    filter(violation.description == violations_list[10])
  
  # Create counts
  viol1_total <- count_whole3(viol1) %>%
    filter(v_count >= 1)
  viol2_total <- count_whole3(viol2)%>%
    filter(v_count >= 1)
  viol3_total <- count_whole3(viol3)%>%
    filter(v_count >= 1)
  viol4_total <- count_whole3(viol4)%>%
    filter(v_count >= 1)
  viol5_total <- count_whole3(viol5)%>%
    filter(v_count >= 1)
  viol6_total <- count_whole3(viol6)%>%
    filter(v_count >= 1)
  viol7_total <- count_whole3(viol7)%>%
    filter(v_count >= 1)
  viol8_total <- count_whole3(viol8)%>%
    filter(v_count >= 1)
  viol9_total <- count_whole3(viol9)%>%
    filter(v_count >= 1)
  viol10_total <- count_whole3(viol10)%>%
    filter(v_count >= 1)
  
  # Map
  if(violation == '1'){
    violmap_res <- create_violmap(viol1_total)
  }
  if(violation == '2'){
    violmap_res <- create_violmap(viol2_total)
  }
  if(violation == '3'){
    violmap_res <- create_violmap(viol3_total)
  }
  if(violation == '4'){
    violmap_res <- create_violmap(viol4_total)
  }
  if(violation == '5'){
    violmap_res <- create_violmap(viol5_total)
  }
  if(violation == '6'){
    violmap_res <- create_violmap(viol6_total)
  }
  if(violation == '7'){
    violmap_res <- create_violmap(viol7_total)
  }
  if(violation == '8'){
    violmap_res <- create_violmap(viol8_total)
  }
  if(violation == '9'){
    violmap_res <- create_violmap(viol9_total)
  }
  if(violation == '10'){
    violmap_res <- create_violmap(viol10_total)
  }
  
  return(violmap_res)
  
}