# load twitter library - the rtweet library is now highly advised over twitteR
library(rtweet)
# plotting, data manipulation , cleaning!
library(ggplot2)
library(dplyr)
library(tidyr)
# text mining library
library(tidytext)
library(tm)
library(widyr)
# plotting packages
library(RColorBrewer)
library(igraph)
library(ggraph)
khan_muslim_tweets <- search_tweets(q = "@sadiqkhan AND terrorist", n = 20000, lang = "en", include_rts = FALSE)

khan_muslim_tweets %>% group_by(source)%>% 
  summarise(Total=n()) %>% arrange(desc(Total)) %>% head(10) %>%
  ggplot(aes(reorder(source, Total), Total, fill = source)) + geom_bar(stat="identity") + coord_flip() + labs(title="Top Tweet Sources for #UFC220", x="", subtitle="There were more tweets coming from iPhone Vs Android smartphones", caption = "\nSource: Data collected from Twitter's REST API via rtweet")

# First, remove http elements manually
khan_muslim_tweets$stripped_text <- gsub("http.*","", khan_muslim_tweets$text)
khan_muslim_tweets$stripped_text <- gsub("https.*","", khan_muslim_tweets$stripped_text)

# Second, remove punctuation, convert to lowercase, add id for each tweet!
khan_muslim_tweets_clean <- khan_muslim_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

# Third, remove stop words from your list of words
cleaned_tweet_words <- khan_muslim_tweets_clean %>%
  anti_join(stop_words)

# Finally, plot the top 15 words
cleaned_tweet_words %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n, fill = word)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "words",
       title = "Most Common words sent to Sadiq Khan",
       subtitle = "analysis of tweets sent between 20-30 March", caption = "\nSource: Data collected from Twitter's REST API via rtweet")

