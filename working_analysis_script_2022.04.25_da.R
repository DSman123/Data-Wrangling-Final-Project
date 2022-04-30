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
workingDF <- commentData %>% mutate(wdcount = str_count(comment, '\\w+')) %>% select(comment_id, comment, score) 

# Gather Sentiments for Each Comment from Bing and NRC 
sentTemp <- workingDF %>% unnest_tokens(word, comment) %>% anti_join(stop_words) %>% inner_join(get_sentiments("nrc")) %>% count(comment_id, sentiment) %>% pivot_wider(values_from = "n", names_from = "sentiment")
posnegTemp <- temp <- workingDF %>% unnest_tokens(word, comment) %>% anti_join(stop_words) %>% inner_join(get_sentiments("bing")) %>% count(comment_id, sentiment) %>% pivot_wider(values_from = "n", names_from = "sentiment")

# Combine Data into a Working Tibble
workingData <- inner_join(workingDF, sentTemp, by ="comment_id") %>% inner_join(workingDF, posnegTemp, by ="comment_id") %>% mutate_all(~replace(., is.na(.), 0)) %>% mutate(text = comment.x, popScore = score.x) %>% mutate(text = `comment.x`, popScore = `score.x`) %>% select(-`comment.x`, -`comment.y`, -`score.x`, -`score.y`)

# Calculate Ancillary Variables
workingData <- workingData %>% mutate(overallSentiment = positive - negative, 
                                      parent_ID = substr(comment_id, 1, regexpr("\\_[^\\_]*$", comment_id)-1))
workingData <- workingData %>% mutate(parentSentiment = workingData[match(parent_ID, workingData$comment_id), 14], 
                                      parentPopularity = workingData[match(parent_ID, workingData$comment_id), 13])

# Exploring the Dataset
positives <- workingData %>% filter(overallSentiment > 0) %>% select(text, comment_id) %>% unnest_tokens(word, text) %>% anti_join(stop_words) %>% count(word, sort = TRUE)
negatives <- workingData %>% filter(overallSentiment < 0) %>% select(text, comment_id) %>% unnest_tokens(word, text) %>% anti_join(stop_words) %>% count(word, sort = TRUE)
topPos <- ggplot(positives %>% filter(n>=30), aes(n, word)) + geom_col() + labs(y = NULL)
topNeg <- ggplot(negatives %>% filter(n>=10), aes(n, word)) + geom_col() + labs(y = NULL)
## CAN WE HAVE LIKE TWO MORE SLIDES FOR VISUALIZATIONS OF THE COMMENT DATA? NOT SURE WHAT ELSE MIGHT BE INTERESTING

# Developing the Model
workingData <- workingData %>% mutate(adjPSent = min(parentSentiment, 10))
model <- lm(workingData$popScore ~ log(workingData$parentPopularity) + workingData$adjPSent + workingData$overallSentiment)
summary(model)


hist(log(workingData$parentPopularity), breaks = 100)
hist(workingData$parentSentiment, breaks = 100)




