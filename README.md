# online-islamophobia
digging into London mayor's mentions(below is the code I used in R studio to scrape about 20,000 the mentions he received)

install.packages("rtweet")
install.packages("httpuv")
install.packages("dplyr")
install.packages("ggplot2")
library(rtweet)
library(httpuv)
library(dplyr)
vignette("auth", package = "rtweet")
appname <- "Iclal Turan" 
key <- "PGau5Y9R5ECL1PV8pKDMAqxaj"
secret <- "pga9d8gsll3OBA7AaPFJ4OqSjaLUvU3bdzCw35IdCNR3xvZvmt"
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret)
## path of home directory
home_directory <- path.expand("~/")

## combine with name for token
file_name <- file.path(home_directory, "twitter_token.rds")

## save token to home directory
saveRDS(twitter_token, file = file_name)
## On my mac, the .Renviron text looks like this:
##     TWITTER_PAT=/Users/mwk/twitter_token.rds

## assuming you followed the procodures to create "file_name"
##     from the previous code chunk, then the code below should
##     create and save your environment variable.
cat(paste0("TWITTER_PAT=", file_name),
    file = file.path(home_directory, ".Renviron"),
    append = TRUE)
readRenviron("~/.Renviron")
# load twitter library - the rtweet library is recommended now over twitteR
library(rtweet)
# plotting and pipes - tidyverse!
library(ggplot2)
library(dplyr)
# text mining library
library(tidytext)
# plotting packages
library(igraph)
library(ggraph)
sadiq_tweets <- search_tweets(q = "@sadiqkhan", n = 20000, lang = "en", include_rts = FALSE)
# check data to see if there are emojis
head(sadiq_tweets$text)
sadiq_tweets$stripped_text <- gsub("http.*","",  sadiq_tweets$text)
sadiq_tweets$stripped_text <- gsub("https.*","", sadiq_tweets$stripped_text)
# note the words that are recognized as unique by R
a_list_of_words <- c("cat", "Cat", "Dog")
unique(a_list_of_words)

# remove punctuation, convert to lowercase, add id for each tweet!
sadiq_tweets_clean <- sadiq_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)
# plot the top 15 words -- notice any issues?
sadiq_tweets_clean %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "words",
       title = "tweets recently sent to @SadiqKhan")
# load list of stop words - from the tidytext package
data("stop_words")
# view first 6 words
head(stop_words)
## # A tibble: 6 x 2
##        word lexicon
##       <chr>   <chr>
## 1         a   SMART
## 2       a's   SMART
## 3      able   SMART
## 4     about   SMART
## 5     above   SMART
## 6 according   SMART

nrow(sadiq_tweets_clean)
## [1] 133650

# remove stop words from your list of words
sadiq_tweet_words <- sadiq_tweets_clean %>%
  anti_join(stop_words)

# there should be fewer words now
nrow(sadiq_tweet_words)
## [1] 74490
# plot the top 15 words -- notice any issues?
sadiq_tweet_words %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "words",
       title = "tweets recently sent to @SadiqKhan",
       subtitle = "analysis of tweets between 20-30 March")
# library(devtools)
#install_github("dgrtwo/widyr")
library(widyr)
install.packages("widyr")

# remove punctuation, convert to lowercase, add id for each tweet!
sadiq_tweets_paired_words <- sadiq_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)

sadiq_tweets_paired_words %>%
  count(paired_words, sort = TRUE)
## # A tibble: 62,682 x 2
##        paired_words     n
##               <chr> <int>
##  1   climate change  1282
##  2           in the   636
##  3           of the   403
##  4       the arctic   341
##  5 climatechange is   299
##  6       learn more   298
##  7        more here   279
##  8 of climatechange   250
##  9             is a   212
## 10           on the   209
## # ... with 62,672 more rows
library(tidyr)
sadiq_tweets_separated_words <- sadiq_tweets_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

sadiq_tweets_filtered <- sadiq_tweets_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
sadiq_words_counts <- sadiq_tweets_filtered %>%
  count(word1, word2, sort = TRUE)

head(sadiq_words_counts)
## # A tibble: 6 x 3
##           word1         word2     n
##           <chr>         <chr> <int>
## 1       climate        change  1282
## 2 climatechange globalwarming   146
## 3        global       warming   132
## 4           sea       turtles   113
## 5        sahara        desert    85
## 6        arctic       climate    81

library(igraph)
library(ggraph)

# plot climate change word network
sadiq_words_counts %>%
  filter(n >= 24) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets sent to Sadiq Khan",
       subtitle = "Text mining twitter data ",
       x = "", y = "")
install.packages("SnowballC")
library(wordcloud)
library(SnowballC)
library(tm)
sadiq_tweets_clean_text <- sapply(sadiq_tweets_clean, function(x) x$getText())

