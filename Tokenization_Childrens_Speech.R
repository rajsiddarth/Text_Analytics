rm(list=ls(all=T))

#Reading data directory
library(RCurl)

link="https://raw.githubusercontent.com/rajsiddarth/Text_Analytics/master/Children_Speech/"
library(tibble)
file_info=as_data_frame(read.table(text=getURL(paste0(link,"guide_to_files.csv")),header=T,sep=","))
file_info$file_name[1]

#Reading each file
file_name=paste0(link,as.character(file_info$file_name[1]))
file_name=trimws(file_name)

library(tm)
file_text=readLines(file_name)
str(file_text)

library(dplyr)
#Selecting words spoken by a child which starts with *CHI
childs_speech=data_frame('value'=file_text[grep("\\*CHI",file_text)])

library(tidytext)

word_frequency=childs_speech %>% unnest_tokens(word,value)
# Punctuation has been removed  and everything has been made lowercase
#Counting the frequency of words
head(word_frequency)
word_count=word_frequency %>% count(word,sort = T)

#Removing the word chi 

word_count=word_frequency%>%anti_join(data_frame('word'="chi"))%>%count(word,sort=T)

#.................Implementing tokenization using Functions.............................#
# to prep file names
#Dataset is the name of csv file containing
rm(list=ls(all=T))
library(RCurl)

link="https://raw.githubusercontent.com/rajsiddarth/Text_Analytics/master/Children_Speech/"
library(tibble)
file_info=as_data_frame(read.table(text=getURL(paste0(link,"guide_to_files.csv")),header=T,sep=","))


filename_dataframe=data_frame("name"=NA)

textfilenames=function(dataset){
for(name in 1:nrow(dataset)){
  textfile=paste0(link,as.character(dataset$file_name[name]))
  textfile=trimws(textfile)
  filename_dataframe=rbind(filename_dataframe,textfile)
}
  filenames=filename_dataframe[-1,]
  return(filenames)
}
filenames=textfilenames(file_info)

#Tokenizing each text file
#Giving output of textfilenames as input to tokenize
library(tidytext)
library(dplyr)
temp=setNames(data.frame(matrix(ncol = ncol(file_info)+2, nrow = 0)), c("word", "n",names(file_info)))
tokenize=function(x){
  for(i in 1:nrow(x)){
    text=readLines(x$name[i])
    childs_speech=data_frame('value'=text[grep("\\*CHI",text)])%>%unnest_tokens(word,value)
    word_count=childs_speech%>%anti_join(data_frame('word'="chi"))%>%count(word,sort=T)
    word_count=merge(word_count,file_info[i,] )
    temp=rbind(temp,word_count)
    
    }
  return(temp)
  
}
#Tokenized words for each child

y=tokenize(filenames)

#From the above table we can then fond the most frequently used words 
#whose first language is different

   
