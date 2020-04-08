
library(dplyr)
library(stringr)
library(tidytext)
library(tidyr)
library(scales)
library(textreadr)
library(ggraph)
library(igraph)
library(textdata)
data(stop_words)



setwd("C:/Users/SHEETHAL/Desktop/Text analytics/Individual Assignment/Tesla")
nm <- list.files(path="C:/Users/SHEETHAL/Desktop/Text analytics/Individual Assignment/Tesla")
Q1_data <- read_document(file='C:/Users/SHEETHAL/Desktop/Text analytics/Individual Assignment/Tesla/Q1.txt')
Q2_data <- read_document(file='C:/Users/SHEETHAL/Desktop/Text analytics/Individual Assignment/Tesla/Q2.txt')
Q3_data <- read_document(file='C:/Users/SHEETHAL/Desktop/Text analytics/Individual Assignment/Tesla/Q3.txt')
Q4_data <- read_document(file='C:/Users/SHEETHAL/Desktop/Text analytics/Individual Assignment/Tesla/Q4.txt')
Q5_data <- read_document(file='C:/Users/SHEETHAL/Desktop/Text analytics/Individual Assignment/Tesla/Q5.txt')
Q6_data <- read_document(file='C:/Users/SHEETHAL/Desktop/Text analytics/Individual Assignment/Tesla/Q6.txt')
Q7_data <- read_document(file='C:/Users/SHEETHAL/Desktop/Text analytics/Individual Assignment/Tesla/Q7.txt')
Q8_data <- read_document(file='C:/Users/SHEETHAL/Desktop/Text analytics/Individual Assignment/Tesla/Q8.txt')


Q1_data <- data.frame(line=1:length(Q1_data), text=Q1_data, stringAsFactors = FALSE)
Q2_data <- data.frame(line=1:length(Q2_data), text=Q2_data, stringAsFactors = FALSE)
Q3_data <- data.frame(line=1:length(Q3_data), text=Q3_data, stringAsFactors = FALSE)
Q4_data <- data.frame(line=1:length(Q4_data), text=Q4_data, stringAsFactors = FALSE)
Q5_data <- data.frame(line=1:length(Q5_data), text=Q5_data, stringAsFactors = FALSE)
Q6_data <- data.frame(line=1:length(Q6_data), text=Q6_data, stringAsFactors = FALSE)
Q7_data <- data.frame(line=1:length(Q7_data), text=Q7_data, stringAsFactors = FALSE)
Q8_data <- data.frame(line=1:length(Q8_data), text=Q8_data, stringAsFactors = FALSE)


my_text <- bind_rows(mutate(Q1_data, author="Q1"),
                    mutate(Q2_data, author="Q2"),
                    mutate(Q3_data, author="Q3"),
                    mutate(Q4_data, author="Q4"),
                    mutate(Q5_data, author="Q5"),
                    mutate(Q6_data, author="Q6"),
                    mutate(Q7_data, author="Q7"),
                    mutate(Q8_data, author="Q8"))

##Tokenization

my_tokens <- my_text %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  count(word, sort=TRUE)

##nrc sentiment analysis
my_nrc <- my_tokens %>%
  inner_join(get_sentiments("nrc")) %>%
  count(sentiment)

##Afinn sentiment analysis
my_afinn <- my_tokens %>%
  inner_join(get_sentiments("afinn"))

sum(my_afinn$value)
mean(my_afinn$value)

##bing sentiment analysis
my_bing <- my_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment)

##nrc sentiment plot
my_nrc %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(sentiment=reorder(sentiment, n)) %>%
  ggplot(aes(sentiment, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

##bing sentiment plot
my_bing %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(sentiment=reorder(sentiment, n)) %>%
  ggplot(aes(sentiment, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

##Bigrams

my_bigrams <- my_text %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

my_bigrams #We want to see the bigrams (words that appear together, "pairs")

my_bigrams_count <- my_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

bigrams_separated <- my_bigrams_count %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
bigram_counts <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep=' ')
#want to see the new bigrams
bigram_counts

bigram_graph <- bigram_counts %>%
  filter(n>20) %>% #change to 3 or 4
  graph_from_data_frame()

bigram_graph
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)




