#########################################################
library(quanteda)
library(dplyr)
library(stringi)
library(ggplot2)
library(RColorBrewer)

#########################################################

## Summary

# The goal of this Assignment is explore three datasets. The data sets comes from  
# different sources: news, blogs and twitter. I'll briefly explain only the major features 
# of the data  

#########################################################

# Download from the Internet and unzip file

if(!file.exists("dataset.zip")){
  url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
  download.file(url = url, destfile = "dataset.zip", mode = "wb")
  unzip(zipfile = "dataset.zip")
  rm(url)
}

con <- file("final/en_US/en_US.twitter.txt", "r")
twitter <- readLines(con = con, encoding = "UTF-8", skipNul = TRUE)
close(con)

con <- file("final/en_US/en_US.news.txt", "r")
news <- readLines(con = con, encoding = "UTF-8", skipNul = TRUE)
close(con)

con <- file("final/en_US/en_US.blogs.txt", "r")
blogs <- readLines(con = con, encoding = "UTF-8", skipNul = TRUE)
close(con)

#########################################################

## Basic summaries of the three files

# Blogs

blogSummary <- data.frame(
  "Line_count" = length(blogs),
  "Word_count" = sum(stri_count_words(blogs)),
  "Mean_of_word_count" =  mean(stri_count_words(blogs))
)
blogSummary %>% formattable()

# Example of post on a blog:
blogs[1]

# news

newsSummary <- data.frame(
  "Line_count" = length(news),
  "Word_count" = sum(stri_count_words(news)),
  "Mean_of_word_count" =  mean(stri_count_words(news))
)
newsSummary %>% formattable()

# Example of news:
news[1]

# Twitter

twitterSummary <- data.frame(
  "Line_count" = length(twitter),
  "Word_count" = sum(stri_count_words(twitter)),
  "Mean_of_word_count" =  mean(stri_count_words(twitter))
)
twitterSummary %>% formattable()

# Example of tweet:
twitter[1]


#########################################################

## Randomly sample documents

# I'll take random samples of text due to large size of files

set.seed(123)
texts <- c(blogs, news, twitter)
sample <- sample(x = 1:length(texts), size = length(texts) * 0.01, replace = FALSE)
texts.sample <- texts[sample]

#########################################################

## The most common words in all sentences 

# As you can see below the most common word is “just”“, which appears 2589 times. 
# Next comes ”get” which appears 2532 times. We also see words  “like” and “one”, 
# which appear 2481 and  2363 times respectively. 


tokens.ng1 <- tokens(x = texts.sample, what = "word", 
                     remove_numbers = TRUE, remove_punct = TRUE,
                     remove_symbols = TRUE,remove_separators = TRUE,
                     remove_hyphens = TRUE)

dfm.ng1 <- dfm(x = tokens.ng1, tolower = TRUE, stem = TRUE, remove = stopwords())


freq.ng1 <- textstat_frequency(dfm.ng1)
head(freq.ng1, n = 8)

freq.ng1 %>%
  arrange(desc(frequency)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(feature, frequency), y = frequency, fill = feature)) +
  geom_bar(stat = "identity") +
  xlab(label = "") +
  theme(legend.position = "none")

#########################################################

## A word cloud to visualize the text data

# A word cloud is graphical representation of frequently used words in the 
# normalized text.  The height of each word in this picture is an indication 
# of frequency of occurrence # of the word in entire text

set.seed(123)
textplot_wordcloud(x = dfm.ng1, random.color = TRUE, rot.per = .25, max.words = 70, 
                   random.order = FALSE,  colors = brewer.pal(8, "Dark2"))


#########################################################

## Let's see ngrams

# The general idea is that you can look at each pair (or triple, set of four, etc.) 
# of words that occur next to each other. In a large corpus, you're likely to see "the red" 
# and "red apple" several times, but less likely to see "apple red" and "red the". 
# This may be useful to predict next word in typing. 
# These co-occuring words are 
# known as "n-grams", where "n" is a number saying how long a string of words you considered. 
# (Unigrams are single words, bigrams are two words, trigrams are three words, 4-grams are 
# four words, etc.)


tokens.ng2 <- tokens_ngrams(x = tokens.ng1, n = 2L)

dfm.ng2 <- dfm(x = tokens.ng2, tolower = TRUE, stem = TRUE, remove = stopwords())


freq.ng2 <- textstat_frequency(dfm.ng2)
head(freq.ng2, n = 8)
freq.ng2 %>%
  arrange(desc(frequency)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(feature, frequency), y = frequency, fill = feature)) +
  geom_bar(stat = "identity") +
  xlab(label = "") +
  theme(legend.position = "none")

