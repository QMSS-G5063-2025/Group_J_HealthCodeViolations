library(wordcloud)
library(tm)
library(syuzhet)
library(ggplot2)
library(plotly)


## Word Clouds 

prep <- function(data){
  text_corpus <- Corpus(VectorSource(data$text))
  
  text_corpus <- text_corpus %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords("english")) %>%
    tm_map(stripWhitespace)
  
  tdm <- TermDocumentMatrix(text_corpus)
  m <- as.matrix(tdm)
  
  word_freqs <- sort(rowSums(m), decreasing = TRUE)
  
  word_freqs_df <- data.frame(word = names(word_freqs), freq = word_freqs)
  return(word_freqs_df)
} 
word_clouds <- function(data){
  word_freqs_df <- prep(data)
  
  colors_borough <- c("Manhattan" = "#6D9AC6",  
                      "Brooklyn" = "#F0A88C",  
                      "Queens" = "#A1D6B9", 
                      "Bronx" = "#F296B3",   
                      "Staten Island" = "#C89BCC",
                      "New York City" = "#B690A1")
  
  cloud <- wordcloud(words = word_freqs_df$word, freq = word_freqs_df$freq,
                     min.freq = 2,           
                     max.words = 100,    
                     random.order = FALSE,  
                     colors = colors_borough)
  return(cloud)
}


# Sentiment analysis 
# Function to get sentiment scores
get_sentiment_scores <- function(text_data) {
  # Clean text (optional if you want very pure text, can skip if already clean)
  text_data <- tolower(text_data)
  text_data <- gsub("[^[:alnum:] ]", "", text_data)
  
  # Get sentiment
  sentiment_scores <- get_sentiment(text_data, method = "syuzhet")
  return(sentiment_scores)
}

graph_sent <- function(queens, bronx, man, brook, stat){
  queens_sentiment <- get_sentiment_scores(queens$text)
  bronx_sentiment <- get_sentiment_scores(bronx$text)
  man_sentiment <- get_sentiment_scores(man$text)
  brook_sentiment <- get_sentiment_scores(brook$text)
  stat_sentiment <- get_sentiment_scores(stat$text)
  
  colors_borough <- c("Manhattan" = "#6D9AC6",  
                      "Brooklyn" = "#F0A88C",  
                      "Queens" = "#A1D6B9", 
                      "Bronx" = "#F296B3",   
                      "Staten Island" = "#C89BCC",
                      "New York City" = "#B690A1")
  
  sentiment_df <- data.frame(
    Borough = c("Queens", "Bronx", "Manhattan", "Brooklyn", "Staten Island"),
    Mean_Sentiment = c(mean(queens_sentiment),
                       mean(bronx_sentiment),
                       mean(man_sentiment),
                       mean(brook_sentiment),
                       mean(stat_sentiment))
  )
  
  borough_colors <- colors_borough[sentiment_df$Borough]
  
  sent_graph <- plot_ly(
    data = sentiment_df,
    x = ~Borough,
    y = ~Mean_Sentiment,
    type = 'bar',
    marker = list(color = unname(borough_colors))  # match borough colors
  ) %>%
    layout(
      xaxis = list(title = "Borough"),
      yaxis = list(title = "Average Sentiment"),
      hovermode = "closest",
      paper_bgcolor= '#f8f9fa')
  return(sent_graph)
}

top_15 <- function(data, region){
  colors_borough <- c("Manhattan" = "#6D9AC6",  
                      "Brooklyn" = "#F0A88C",  
                      "Queens" = "#A1D6B9", 
                      "Bronx" = "#F296B3",   
                      "Staten Island" = "#C89BCC",
                      "New York City" = "#B690A1")
  word_freqs_df <- prep(data)
  # top 15 words
  top_words <- word_freqs_df %>% 
    top_n(15, freq) %>%
    arrange(freq)
    
  # static ggplot
  bar <- ggplot(top_words, aes(x = reorder(word, freq), y = freq)) +
    geom_col(fill = colors_borough[region], aes(text = paste(
      "Frequency: ", freq
    ))) +
    coord_flip() +
    labs(
      x = "Word",
      y = "Frequency") +
    theme_minimal(base_size = 14) + 
    theme(
      plot.background = element_rect(fill = "#f8f9fa"),
      plot.title = element_text(size = 13, color = "#333333", hjust=0.5),
      axis.title = element_text(size = 11, face = "light"),
      axis.text = element_text(size = 10, color = "#555555"),
      legend.title = element_text(size = 11, face = "light"),
      legend.text = element_text(size = 10),
      panel.grid.major = element_line(color = "#DDDDDD", size = 0.5), 
      panel.grid.minor = element_line(color = "#DDDDDD", size = 0.25)
    )
    
  #plotly 
  top_15_graph <- ggplotly(bar, tooltip="text") %>%
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
  return(top_15_graph)
}
