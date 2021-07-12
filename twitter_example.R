# load libraries
library(rtweet)
library(tidytext)
library(dplyr)
library(ggplot2)
library(wordcloud2)

# import data
# search for 1000 tweets (non-retweeted) under the "rstats" hashtag
rt <- search_tweets(
  "#rstats", n = 1000, include_rts = FALSE
)

# clean tweets
# extra stop words
extra_sw <- "t.co|https|rstats"

# select only tweet content; tokenize and filter out stop words
rt_cleaned <- rt %>%
  select(text) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  filter(!grepl(extra_sw, word))

# build a simple freq count model
# top words; bar graph
rt_cleaned %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()

# visualize freq count using word cloud
rt_cleaned %>%
  count(word, sort = TRUE, name = "freq") %>%
  top_n(50) %>%
  wordcloud2()

