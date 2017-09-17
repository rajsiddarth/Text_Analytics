---
title: "Text analytics implementation"
author: "Siddarth"
date: "16 September 2017"
output:
  html_document:
    number_sections: true
    toc: true
    fig_width: 7
    fig_height: 4.5
    theme: cosmo
    highlight: tango
    keep_md:true
 
---
#Introduction
The code written is used to learn the basics about text classification libraries such as tidyr,tidytext,tidyverse,SnowballC,tibble and also visualization libraries such as ggplot, wordcloud.The code is for learning purposes and is loosely based on the kernal submitted in Kaggle by [heads or tails](https://www.kaggle.com/headsortails/personalised-medicine-eda-with-tidy-r) for a competition described below.The kernel is selected because the text information holds the key to the classification problem and will have to be understood/modelled well.
We need to develop algorithms to classify genetic mutations into nine different classes based on clinical evidence (text).The data is from the kaggle challenge [Personalized Medicine: Redefining Cancer Treatment](https://www.kaggle.com/c/msk-redefining-cancer-treatment).
  The [data](https://www.kaggle.com/c/msk-redefining-cancer-treatment/data) in two csv files and two text files:

- *training/test variants* 

- *training/test text* 

##Reading data
Reading train and test variants data in csv files using library **readr**.


```{r,message=FALSE, warning=FALSE}
#install.packages(readr)
library(readr)
train = read_csv('C:/Users/sid/Desktop/Kaggle/Personalized_medicine/training_variants/training_variants.csv')
test  = read_csv('C:/Users/sid/Desktop/Kaggle/Personalized_medicine/test_variants/test_variants.csv')

```
Reading in the text files currently in txt format as tibble data frames using *tidyverse* library.
```{r,message=FALSE, warning=FALSE}
#install.packages(tidyverse)
library(tidyverse)
train_txt_dump = tibble(text = read_lines('C:/Users/sid/Desktop/Kaggle/Personalized_medicine/training_text/training_text.txt', skip = 1))
test_txt_dump = tibble(text = read_lines('C:/Users/sid/Desktop/Kaggle/Personalized_medicine/test_text/test_text.txt', skip = 1))

```
Using functions from *dplyr* to build *train_txt* and *test_txt* tibble dataframes.
```{r,message=FALSE,warning=FALSE}
#install.packages(dplyr)
library(dplyr)
train_txt = train_txt_dump %>%separate(text, into = c("ID", "txt"), sep = "\\|\\|")%>%
  mutate(ID = as.integer(ID))
test_txt = test_txt_dump %>%separate(text, into = c("ID", "txt"), sep = "\\|\\|")%>%
   mutate(ID = as.integer(ID))

```

Converting attributes in *train* and *test* to appropriate data format.
```{r}
train = train %>%mutate(Gene = factor(Gene),Variation = factor(Variation),
                         Class = factor(Class))

test = test %>%mutate(Gene = factor(Gene),Variation = factor(Variation))

glimpse(train)
```

Counting number of distinct Genes in *train* and *test* 
```{r}

train %>%  group_by(Gene) %>% summarise(ct = n()) %>%  arrange(desc(ct))

test %>%  group_by(Gene) %>%  summarise(ct = n()) %>%  arrange(desc(ct))
```

Counting number of distinct variations in *train* and *test*
```{r}

train %>%  group_by(Variation) %>%summarise(ct = n()) %>% arrange(desc(ct))

test %>%  group_by(Variation) %>%  summarise(ct = n()) %>%  arrange(desc(ct))
```

Top 10 Genes in train.Considering Genes whose count is greater than 40.

```{r}

top_gene = train %>%  group_by(Gene) %>%  summarise(ct = n()) %>%   filter(ct > 40)
top_gene
```

Plotting top 10 genes given in train data by count 

```{r fig.align = 'default', warning = FALSE, fig.cap ="Fig. 1", out.width="100%"}

top_gene %>%
  ggplot(aes(reorder(Gene, -ct, FUN = min), ct)) +
  geom_point(size = 4,color="red") +labs(x = "Gene", y = "Frequency") +  coord_flip()
```

Similarly plotting top genes in test.
```{r fig.align = 'default', warning = FALSE, fig.cap ="Fig. 2", out.width="100%"}
top_gene_test = test %>%group_by(Gene) %>%summarise(ct = n())%>% filter(ct > 40)
top_gene_test%>%ggplot(aes(reorder(Gene,-ct,FUN=min),ct))+geom_point(size=4,color="blue")+
  labs(x = "Gene", y = "Frequency")+coord_flip()

```


Checking for count of variations in train and test
```{r fig.align = 'default', warning = FALSE, fig.cap ="Fig. 3", out.width="100%"}

train%>%count(Variation)%>%arrange(desc(n))%>%top_n(10)%>%
  ggplot(aes(reorder(x=Variation,-n),n))+geom_point(size=3,color="red")+coord_flip()+
  labs(x="Variation",y="count")

```

```{r fig.align = 'default', warning = FALSE, fig.cap ="Fig. 4", out.width="100%"}
test%>%count(Variation)%>%arrange(desc(n))%>%slice(1:10)%>%
  ggplot(aes(reorder(x=Variation,-n),n))+geom_point(size=3,color="blue")+coord_flip()+
  labs(x="Variation",y="count")
```

```{r fig.align = 'default', warning = FALSE, fig.cap ="Fig. 5", out.width="100%"}
temp_train=train%>%mutate(set=factor("train"))%>%select(-ID,-Class)
temp_test=test%>%mutate(set=factor("test"))%>%select(-ID)

full_join(temp_train,temp_test)%>%group_by(Variation,set)%>%summarise(ct=n())%>%
   filter(ct > 3) %>%
  ggplot(aes(reorder(Variation, -ct, FUN = median), ct, colour = set)) +
  geom_point(size = 4) +
  coord_cartesian(ylim = c(0, 100))+labs(x = "Variation", y = "Frequency")
```

Checking for class imbalances in train

```{r fig.align = 'default', warning = FALSE, fig.cap ="Fig. 6", out.width="100%",echo=FALSE}
train%>%ggplot(aes(x=Class))+geom_bar(color="red")
```

- *Class* levels 3, 8, and 9 are under-represented

- 5 and 6 are of low frequency

- Levels 1, 2, and 4 are of high frequency

- Level 7 is  the most frequent one

#Text feature Engineering

Taking *train_txt* and *test_txt* read using tidytext library and calulating the length of text.Using *str_length*from *stringr*.
```{r}
library(stringr)

train_txt=train_txt%>%mutate(txt_length=str_length(txt),set="train") 
test_txt=test_txt%>%mutate(txt_length=str_length(txt),set="test")
```

Plotting density plots for text lengths of train_txt and test_txt

```{r fig.align = 'default', warning = FALSE, fig.cap ="Fig. 7", out.width="100%"}
 full_join(train_txt,test_txt) %>%
  ggplot(aes(txt_length, fill = set)) +
   geom_density() +
  labs(x = "Length of text entry")
```

Checking the text lengths for each class.Data from train is now combined with train_txt by ID.Geom density plots from ggplot library is the smooth version of Histograms.The density of the length of text from train data and for each given class is shown in Figure 8 

```{r split=FALSE, fig.align = 'default', warning = FALSE, fig.cap ="Fig.8 ", out.width="100%"}
temp1=train_txt %>% select(ID, txt_length)
temp2=train %>%  select(ID, Class)

full_join(temp1,temp2, by = "ID") %>% ggplot(aes(txt_length)) +
  geom_density(fill = "red", bw = 5e3) +labs(x = "Length of text entry") +
  facet_wrap(~ Class)

```
##Tokenizing using tidytext 
The *unnest_tokens* function from *tidytext* library not only tokenizes the senteces but also  gets rid of punctuation and converts everything to lowercase.

```{r}
library(tidytext)
t1=train_txt %>% select(ID, txt) %>% unnest_tokens(word,txt)
head(t1)
```
##Stop word removal

The *tidytext* package contains a dictionary of *stop words* to help us remove stopwords from tidy text data. We will also define our own selection of stop words based on the typical structuring language of scientific papers. We also remove tokens that are only numbers or symbols.

```{r}
data("stop_words")
my_stopwords=data_frame(word = c(as.character(1:100),
                                    "fig", "figure", "et", "al", "table",
                                    "data", "analysis", "analyze", "study",
                                    "method", "result", "conclusion", "author",
                                    "find", "found", "show", "perform",
                                    "demonstrate", "evaluate", "discuss"))
t1=t1%>%anti_join(my_stopwords,by="word")%>%anti_join(stop_words,by="word")%>%
  filter(str_detect(word,"[a-z]"))

```

Looking at most popular words in train_txt with count greater than 50000.

```{r split=FALSE, fig.align = 'default', warning = FALSE, fig.cap ="Fig.9 ", out.width="100%"}

t1%>%group_by(word)%>%summarise(ct=n())%>%filter(ct>50000)%>%top_n(10)%>%
  ggplot(aes(reorder(word,-ct,FUN=median),y=ct))+geom_bar(stat="identity",color="red")+labs(x="word",y="count")+coord_flip()
```

##Word Stemming
As we can see from Fig 9, although the words mutation and mutations are variants of each other,they are shown to be different.Hence we perform word stemming and reduce them to their basic meaning.We will use the *"SnowballC"* package and its *"wordStem"* tool:

```{r split=FALSE, fig.align = 'default', warning = FALSE, fig.cap ="Fig. 9", out.width="100%"}
library(SnowballC)
t1 = t1 %>%mutate(stem = wordStem(word))

t1%>%group_by(stem)%>%summarise(count=n())%>%filter(stem>50000)%>%top_n(10)%>%
  ggplot(aes(reorder(stem,-count,FUN=median),count))+geom_col(color="blue")+labs(x="stemmed words",y="count")+coord_flip()
```
### Word Cloud
Depicting frequent words as word cloud.

```{r split=FALSE, fig.align = 'default', warning = FALSE, fig.cap ="Fig. 10", out.width="100%"}
library(wordcloud)
t1 %>% count(stem) %>%with(wordcloud(stem,n, max.words = 100))
```
##TF-IDF analysis
Calculating Term frequency and IDF for words based on classes.Term frequency will be the number of times the word appeared in a particular class. Document frequency is the number of times the word appeared in all the classes.We use *bind_tf_idf* from tidytext to extract TF_IDF.

```{r, warning = FALSE}
library(tidytext)
temp_train=train%>%select(ID,Class)
temp_t1=t1%>%select(ID,stem)
tf_idf=inner_join(temp_train,temp_t1,by="ID")%>%count(Class,stem)%>%bind_tf_idf(stem,Class,n)
glimpse(tf_idf)

```

Plotting top 10 TF-IDF values for each class


```{r split=FALSE, fig.align = 'default', warning = FALSE, fig.cap ="Fig. 11", out.width="100%"}
tf_idf %>%
  arrange(desc(tf_idf))%>%mutate(word = factor(stem, levels = unique(stem)))%>%
   group_by(Class)%>%top_n(10,tf_idf)%>%
  ungroup() %>%  
  ggplot(aes(word, tf_idf, fill = Class)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  theme(legend.position = "none") +
  facet_wrap(~ Class, ncol = 3, scales = "free") +
  coord_flip()

```
##Tokenizing into n-grams
Using *unnest_tokens* to split the sentence it bi-grams(pairs of words).Below code is not executed due to the limitations of my laptop's processing power.Remove # and execute.Select the chunk of code and press Ctrl+Shift+C to uncomment or comment.

###Tokenize into bi-grams
```{r}
# t2=train_txt %>% select(ID, txt) %>% unnest_tokens(bigram, txt, token = "ngrams",
# n = 2)
# head(t2)
```

###Removing stop words
```{r}
#bi_sep = t2 %>%
  #separate(bigram, c("word1", "word2"), sep = " ")

#bi_filt = bi_sep %>%
  #filter(!word1 %in% stop_words$word) %>%
 # filter(!word2 %in% stop_words$word) %>%
 # filter(!word1 %in% my_stopwords$word) %>%
  #filter(!word2 %in% my_stopwords$word)

# for later
#bigram_counts = bi_filt %>%
  #count(word1, word2, sort = TRUE)

#t2 =bi_filt %>%
  #unite(bigram, word1, word2, sep = " ")
```

###Estimating tf-idf:

```{r}
# foo =train %>%
#   select(ID, Class)
# 
# t2_class = full_join(t2, foo, by = "ID")
# 
# t2_tf_idf = t2_class %>%
#   count(Class, bigram) %>%
#   bind_tf_idf(bigram, Class, n) %>%
#   arrange(desc(tf_idf))
```

Plot the bigrams per *Class* with the best tf-idf values:

```{r split=FALSE, fig.align = 'default', warning = FALSE, fig.cap ="Fig. 12", out.width="100%"}
# t2_tf_idf %>%
#   arrange(desc(tf_idf)) %>%
#   mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>%
#   group_by(Class) %>%
#   top_n(10, tf_idf) %>%
#   ungroup() %>%  
#   ggplot(aes(bigram, tf_idf, fill = Class)) +
#   geom_col() +
#   labs(x = NULL, y = "tf-idf") +
#   theme(legend.position = "none") +
#   facet_wrap(~ Class, ncol = 3, scales = "free") +
#   coord_flip()
```

### Networks of bigrams

Once we have the bigrams, i.e. sequences of adjacent words, we can also visualise their connections with other words by building a *network*. A network of words is a combination of connected nodes. Here we use the *igraph* package to build the network and the *ggraph* package to visualise it within the context of the tidyverse:

```{r split=FALSE, fig.align = 'default', warning = FALSE, fig.cap ="Fig. 13", out.width="100%"}
# library(igraph)
# library(grid)
# library(ggraph)
# bigram_graph = bigram_counts %>%
#   filter(n > 4e3) %>%
#   graph_from_data_frame()
# 
# set.seed(123)
# 
# a = grid::arrow(type = "closed", length = unit(.1, "inches"))
# 
# ggraph(bigram_graph, layout = "fr") +
#   geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
#                  arrow = a, end_cap = circle(.07, 'inches')) +
#   geom_node_point(color = "lightblue", size = 3) +
#   geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
#   theme_void()
```