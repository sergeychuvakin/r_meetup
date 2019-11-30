setwd("/home/serge/Desktop/lections/lda")
df <- read.csv('for_lda.csv') # читаем данные 
# довольно известные данные sklearn.datasets.fetch_20newsgroups. Размеченные данные для тренировки
# алгоритмов классификации 
str(df) # смотрим структуру 
df[['a']][1] # смотрим первыйй текст 
# нужные пакеты
library(tm)
library(stringr)
library(tidytext)
library(tidyr)
library(topicmodels)
library(dplyr)
library(ggplot2)

# функция для чистки текста
Clean_Corpus <- function(docs) {
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  docs <- tm_map(docs, toSpace, "\\r\\n")
  
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # Remove your own stop word
  # specify your stopwords as a character vector
  docs <- tm_map(docs, removeWords, c("will", "can", "etc", "amp")) 
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
}


# подготовленная функция для лда
top_terms_by_topic_LDA <- function(input_text, # should be a columm from a dataframe
                                  number_of_topics = number_topics_to_model) # number of topics (4 by default)
{    
  # create a corpus (type of object expected by tm) and document term matrix
  Corpus <- Clean_Corpus(Corpus(VectorSource(input_text))) # make a corpus object
  DTM <- DocumentTermMatrix(Corpus) # get the count of words/document
  
  # remove any empty rows in our document term matrix (if there are any 
  # we'll get an error when we try to run our LDA)
  # unique_indexes <- unique(DTM$i) # get the index of each unique value
  # DTM <- DTM[unique_indexes,] # get a subset of only those indexes
  
  # preform LDA & get the words/topic in a tidy text format
  lda <- LDA(DTM, k = number_of_topics, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta")
  
  # vector of topic for every document
  topic_list <- apply(lda@gamma, 1, which.max)
  
  # get the top ten terms for each topic
  top_terms <- topics  %>% # take the topics data frame and..
    group_by(topic) %>% # treat each topic as a different group
    top_n(10, beta) %>% # get the top 10 most informative words
    ungroup() %>% # ungroup
    arrange(topic, -beta) # arrange words in descending informativeness
  
  # plot the top ten terms for each topic in order
  return(
    list(Plots =
           top_terms %>% # take the top terms
           mutate(term = reorder(term, beta)) %>% # sort terms by beta value 
           ggplot(aes(term, beta, fill = factor(topic))) + # plot beta by theme
           geom_col(show.legend = FALSE) + # as a bar plot
           facet_wrap(~ topic, scales = "free") + # each topic in a seperate plot
           labs(x = NULL, y = "Beta") + # no x label, change y label 
           coord_flip(), # turn bars sideways

         Topics = topics,
         topic_list=topic_list))
  
}

t <- df[1:100, 'a']

lda <- top_terms_by_topic_LDA(t, 4)
lda$Plots

