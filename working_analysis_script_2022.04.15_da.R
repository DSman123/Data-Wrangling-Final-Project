# Install and Import Necessary Packages
# install.packages("RedditExtractoR")
# install.packages("tidyverse")
# install.packages("tidytext")
# install.packages("textdata")
library(RedditExtractoR)
library(tidytext)
library(textdata)
library(tidyverse)


# Import Comments from Target Thread
commentData <- get_thread_content("https://www.reddit.com/r/StarWarsBattlefront/comments/7cff0b/seriously_i_paid_80_to_have_vader_locked/dppum98/")$comments

workingDF <- commentData %>% mutate(wdcount = str_count(comment, '\\w+')) %>% filter(wdcount >= 10) %>% select(comment_id, comment, score) 
temp <- workingDF %>% unnest_tokens(word, comment) %>% anti_join(stop_words) %>% inner_join(get_sentiments("bing")) %>% count(comment_id, sentiment) %>% pivot_wider(values_from = "n", names_from = "sentiment")
workingDF <- inner_join(workingDF, temp, by ="comment_id") %>% mutate_all(~replace(., is.na(.), 0)) %>% mutate(sentiment = positive - negative, score_per_sentiment = score/sentiment)