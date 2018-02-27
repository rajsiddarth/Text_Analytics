rm(list=ls(all=T))

if(!dir.exists("review_analysis")){
    dir.create("review_analysis")
}

setwd("review_analysis")

library(tidyverse)
library(RCurl)
library(tibble)

data=read.csv("https://raw.githubusercontent.com/rajsiddarth/
Text_Analytics/master/deceptive-opinion.csv",header=T)

reviews=as_tibble(data)
rm(data)

str(reviews)
reviews$text=as.character(reviews$text)

library(tm)
top_termsby_topic_LDA=function(input_text,plot=T,number_of_topics=4){
    
    Corpus=Corpus(VectorSource(input_text))
#Gets the count of words in the document
    DTM=DocumentTermMatrix(Corpus)
    unique_indexes=unique(DTM$i)

    DTM=DTM[unique_indexes,]

    library(topicmodels)

    lda=LDA(DTM,k=number_of_topics,control = list(seed=1234))

    library(tidytext)

    topics=tidy(lda,matrix="beta")

#Getting top 10 terms for each topic

    top_terms=topics%>%group_by(topic)%>%
        top_n(10,beta)%>%
        ungroup()%>%
        arrange(topic,-beta)
    
    if(plot==T){
        top_terms%>%
            mutate(term=reorder(term,beta))%>%
            ggplot(aes(term,beta,fill=factor(topic)))+
            geom_col(show.legend = FALSE)+
            facet_wrap(~topic,scales="free")+
            labs(x=NULL,y="Beta")+coord_flip()
    }
    else{
        return(top_terms)
        }
}


top_termsby_topic_LDA(reviews$text,number_of_topics = 2)

#Preprocessing the text

reviewsCorpus <- Corpus(VectorSource(reviews$text)) 
reviewsDTM <- DocumentTermMatrix(reviewsCorpus)

reviewsDTM_tidy=tidy(reviewsDTM)

custom_stop_words <- tibble(word = c("hotel", "room"))

# remove stopwords
reviewsDTM_tidy_cleaned <- reviewsDTM_tidy %>% 
    anti_join(stop_words, by = c("term" = "word")) %>% 
    anti_join(custom_stop_words, by = c("term" = "word"))

top_termsby_topic_LDA(reviewsDTM_tidy_cleaned$term,number_of_topics = 2)

#Stemming
library(SnowballC)
reviewsDTM_tidy_cleaned=reviewsDTM_tidy_cleaned%>%
    mutate(stem=wordStem(term))

top_termsby_topic_LDA(reviewsDTM_tidy_cleaned$stem,number_of_topics = 2)

#Supervised technique


