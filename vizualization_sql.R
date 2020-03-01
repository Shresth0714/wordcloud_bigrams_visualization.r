library(textreadr)
library(tidyr)
library(dplyr)
library(stringr)
library(tidytext)
library(reshape2)
library(wordcloud)
library(wordcloud2)
library(ggplot2)
library(caret)
library(quanteda)
library(RColorBrewer)
library(tm) 
library(SnowballC)
library(RColorBrewer)
library(RCurl)
library(XML)
# install.packages("SnowballC")
# install.packages("RCurl")
#install.packages("wordcloud2")

#loading surevy text files
path_responses_chase <- "/Users/shresthsethi/Desktop/MSBA Hult/MOD B/SQL/text_scripts/Chase, EF Data Strategy, 2.25.2020.docx"
responses_chase <- read_document(file=path_responses_chase)
path_responses_sandy <- "/Users/shresthsethi/Desktop/MSBA Hult/MOD B/SQL/text_scripts/Sandy, Interview about EF’s Data Strategy.docx"
responses_sandy <- read_document(file=path_responses_sandy)

#combining both files from two survey customers to analyze results
hp_combine <- c(responses_chase, responses_sandy)

#converting to a data frame
review_df <- data_frame(text=hp_combine)

#tockenizing the dataframe for further text analysis with removing stop words
data(stop_words)
frequencies_token_sql <- review_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  dplyr::count(word, sort=TRUE)

frequencies_token_sql
#OUTPUT
# A tibble: 638 x 2
# word          n
# <chr>     <int>
#   1 data         46
# 2 students     40
# 3 satisfied    22
# 4 yeah         19
# 5 people       16
# 6 decisions    14
# 7 lot          14
# 8 student      12
# 9 tech         12
# 10 classes      10
# … with 628 more rows

# Now the structured data has been converted into a data frame of tokenized words as word and frquency of that word as n

#Vizulizing data based on the tokens
#normal word cloud
frequencies_token_sql %>%
  with(wordcloud(word, n, color = c("grey80","grey60","grey40", "gray20") , max.words = 75))

#word cloud with nrc sentiment analysis - joy, fear, disgust, anticipation, anger, trust, surprise, sadness
#positive, negative
frequencies_token_sql %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=50,
                   scale = c(0.8,0.8), #formate the scale to fit in
                   fixed.asp = TRUE, #aspect raatio fixed
                   title.size = 1 #
  )

#binnary word cloud with positive and negative sentiments
frequencies_token_sql %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("red", "green"),
                   max.words=50,
                   scale = c(1,1), #formate the scale to fit in
                   fixed.asp = TRUE, #aspect raatio fixed
                   title.size = 1 #
  )

#Initiatiging n-grams to the analysis
#checking further analysis by combining two tokens/words and seing the results
#creating n-grams survey text data
frequency_sql_ngram <- review_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
  count(bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ")

#filtiring by words and removing stop words
frequency_sql_filter <- frequency_sql_ngram %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

frequency_sql_filter
#OUTPUT
# A tibble: 253 x 3
# word1    word2        n
# <chr>    <chr>    <int>
#   1 data     driven       6
# 2 term     students     5
# 3 mm       hmm          4
# 4 culture  shock        3
# 5 language progress     3
# 6 short    term         3
# 7 students tend         3
# 8 tracking system       3
# 9 10       cent         2
# 10 80       people       2
# … with 243 more rows

# Counting the words again- not required in my analysis so i have commented the code
# frequency_sql_count <- frequency_sql_filter %>%
#   count(word1, word2, sort = TRUE)

#######Vizulizing bigram network
# install.packages("igraph")
library(igraph)
bigram_graph <- frequency_sql_filter %>%
  filter(n>1) %>% #filter the results depending on the frquency of bi-grams, overcrowded vizulization is not good
  graph_from_data_frame()

bigram_graph
#this shows the connections between bi-grams
#OUTPUT
# IGRAPH 08d947a DN-- 42 25 -- 
#   + attr: name (v/c), n (e/n)
# + edges from 08d947a (vertex names):
#   [1] data      ->driven       term      ->students     mm        ->hmm          culture   ->shock       
# [5] language  ->progress     short     ->term         students  ->tend         tracking  ->system      
# [9] 10        ->cent         80        ->people       business  ->intelligence called    ->elastics    
# [13] cent      ->cloud        collect   ->data         data      ->strategy     elastics  ->search      
# [17] front     ->lines        increase  ->m.p          negative  ->feedback     product   ->manager     
# [21] satisfied ->basically    students  ->submit       tech      ->decisions    time      ->students    
# [25] washington->d.c 

#vizualizing bi-grams created to check the connections using networ between words
# install.packages("ggraph")
library(ggraph)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)
