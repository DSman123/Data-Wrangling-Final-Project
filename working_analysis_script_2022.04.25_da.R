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
sentTemp <- workingDF %>% unnest_tokens(word, comment) %>% anti_join(stop_words%>%filter(word!="game", word!="gaming")) %>% inner_join(get_sentiments("nrc")) %>% count(comment_id, sentiment) %>% pivot_wider(values_from = "n", names_from = "sentiment")
posnegTemp <- temp <- workingDF %>% unnest_tokens(word, comment) %>% anti_join(stop_words%>%filter(word!="game", word!="gaming")) %>% inner_join(get_sentiments("bing")) %>% count(comment_id, sentiment) %>% pivot_wider(values_from = "n", names_from = "sentiment")

# Combine Data into a Working Tibble
workingData <- inner_join(workingDF, sentTemp, by ="comment_id") %>% inner_join(workingDF, posnegTemp, by ="comment_id") %>% mutate_all(~replace(., is.na(.), 0)) %>% mutate(text = comment.x, popScore = score.x) %>% mutate(text = `comment.x`, popScore = `score.x`) %>% select(-`comment.x`, -`comment.y`, -`score.x`, -`score.y`)

# Calculate Ancillary Variables
workingData <- workingData %>% mutate(overallSentiment = positive - negative, 
                                      parent_ID = substr(comment_id, 1, regexpr("\\_[^\\_]*$", comment_id)-1))
workingData <- workingData %>% mutate(parentSentiment = workingData[match(parent_ID, workingData$comment_id), 14], 
                                      parentPopularity = workingData[match(parent_ID, workingData$comment_id), 13], 
                                      absPop = abs(popScore), 
                                      wdcount = str_count(comment, '\\w+'))

# Exploring the Dataset
positives <- workingData %>% filter(overallSentiment > 0) %>% select(text, comment_id) %>% unnest_tokens(word, text) %>% anti_join(stop_words) %>% count(word, sort = TRUE)
negatives <- workingData %>% filter(overallSentiment < 0) %>% select(text, comment_id) %>% unnest_tokens(word, text) %>% anti_join(stop_words) %>% count(word, sort = TRUE)
topPos <- ggplot(positives %>% filter(n>=30), aes(n, word)) + geom_col() + labs(y = NULL)
topNeg <- ggplot(negatives %>% filter(n>=10), aes(n, word)) + geom_col() + labs(y = NULL)
## CAN WE HAVE LIKE TWO MORE SLIDES FOR VISUALIZATIONS OF THE COMMENT DATA? NOT SURE WHAT ELSE MIGHT BE INTERESTING

# Sampling Comments
NegtoUnpop <- workingData %>% filter(parentPopularity < 0, overallSentiment < 0) %>% mutate(cat="Neg Response to Unpop Post")
PostoPop   <- workingData %>% filter(parentPopularity > 150, overallSentiment > 0) %>% mutate(cat="Pos Response to Pop Post")
PostoUnpop <- workingData %>% filter(parentPopularity < 0, overallSentiment > 0) %>% mutate(cat="Pos Response to Unpop Post")
NegtoPop   <- workingData %>% filter(parentPopularity > 150, overallSentiment < 0) %>% mutate(cat="Neg Response to Pop Post")
plot1 <- ggplot(data=(bind_rows(NegtoUnpop, PostoPop, PostoUnpop, NegtoPop)), mapping = aes(x=popScore, y=cat), color=parentPopularity) + geom_point() + ylab("Comment Category") + xlab("Post Popularity (Votes)")
plot1

# Developing the Model
modeltest <- bind_rows(NegtoUnpop, PostoPop)
workingModel <- lm(popScore ~ overallSentiment + parentSentiment + parentPopularity, data=workingData)
summary(workingModel)
