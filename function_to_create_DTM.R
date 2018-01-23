
#Function for DTM
dtm_create = function(clean_corpus){
  require(topicmodels)
  corpus_words<-clean_corpus%>% unnest_tokens(word, text)%>% count(document,word,sort = TRUE)%>% ungroup()
  total_words<-corpus_words%>%group_by(document)%>%summarize(total=sum(n))
  corpus_words_cleaned<-left_join(corpus_words,total_words)
  corpus_dtm_tf <-corpus_words_cleaned %>% cast_dtm(document , word, n)

  
  return(corpus_dtm_tf)
}

#function for TFIDF
tf_idf_create = function(clean_corpus)
{
  
  require(tidytext) || install.packages("tidytext")
  library(tidytext)
  
  corpus_words<-clean_corpus %>% unnest_tokens(word,text) %>% count(document,word,sort = TRUE)%>% ungroup()
  total_words<-corpus_words%>%group_by(document)%>%summarize(total=sum(n))
  corpus_words_cleaned<-left_join(corpus_words,total_words)
  corpus_words_cleaned<-corpus_words_cleaned%>% bind_tf_idf(word,document,n)
  corpus_tf_idf<-cast_dtm(corpus_words_cleaned,document , word, idf)
  
  return(corpus_tf_idf)
  
}