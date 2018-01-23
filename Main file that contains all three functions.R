text.clean = function(corpus,stpwrds)                    # text data
{ require(tidytext)
  require(tm)
  require(dplyr)
  require(tibble)
  corpus  =  gsub("<.*?>", " ", corpus)               # regex for removing HTML tags
  corpus  =  iconv(corpus, "latin1", "ASCII", sub="") # Keep only ASCII characters
  corpus  = tolower(corpus)  # convert to lower case characters
  corpus  =  stripWhitespace(corpus)                  # removing white space
  corpus  =  gsub("^\\s+|\\s+$", "", corpus)          # remove leading and trailing white space
  
  
  # Read Stopwords list
  
  #stpwrds=readline(prompt= "Enter each stop word seperated by a comma: ")
  
  
  data(stop_words)# stopwords list from tidytext
  merged_stp_wrds=unique(c(stpwrds, stop_words$word))    
  corpus=removeWords(corpus,merged_stp_wrds)
  
  corpus  =  stripWhitespace(corpus)                  # removing white space
  clean_corpus<-data_frame(document=1:length(corpus),text=corpus)
  return(clean_corpus)
}


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

function_display = function(corpus_dtm_tf)
{
  
  require(wordcloud)
  library(wordcloud)
  require(igraph)
  library(igraph)
  ##Wordcloud
  display_word_cloud=function(corpus_dtm_tf){
    corpus_dtm=as.matrix(corpus_dtm_tf)
    corpus_dtm_columns_sum=apply(corpus_dtm,2,sum)
    
    
    minimum = min(120, length(corpus_dtm_columns_sum))
    words=colnames(corpus_dtm)[1:minimum]  
    freq = 10 * corpus_dtm_columns_sum/mean(corpus_dtm_columns_sum)  # rescaling for better viewing
    wordcloud(words,  # wordcloud func begins
              freq,           
              scale = c(9, 0.3),  # change this to adjust font size
              colors=1:12)        # randomly choose between 12 colors
  }
  ###Bar Graph
  
  
  display_bar_graph = function(corpus_dtm_tf){
    freq <- sort(colSums(as.matrix(corpus_dtm_tf)), decreasing=TRUE)
    wf <- data.frame(word=names(freq), freq=freq)
    i<-subset(wf, freq>20)    %>% ggplot(aes(word, freq)) +
      geom_bar(stat="identity", fill="red", colour="green") +
      theme(axis.text.x=element_text(angle=45, hjust=1))
    plot(i, 
         layout = layout.kamada.kawai)
  }
  
  display_cog=function(corpus_dtm_tf){
    ###Cooccurence graph
    s<-6
    k1<-12
    title<-'Nokia COG'
    
    corpus_mat = as.matrix(corpus_dtm_tf)   # need it as a regular matrix for matrix ops like %*% to apply
    adj.mat = t(corpus_mat) %*% corpus_mat    # making a square symmatric term-term matrix 
    diag(adj.mat) = 0     # no self-references. So diag is 0.
    a0 = order(apply(adj.mat, 2, sum), decreasing = T)   # order cols by descending colSum
    adj.mat = as.matrix(adj.mat[a0[1:50], a0[1:50]]) 
    mat1 = adj.mat
    
    library(igraph)
    a = colSums(mat1) # collect colsums into a vector obj a
    b = order(-a)     # nice syntax for ordering vector in decr order  
    
    mat2 = mat1[b, b]     # order both rows and columns along vector b
    
    diag(mat2) =  0
    
    ## +++ go row by row and find top k adjacencies +++ ##
    
    wc = NULL
    
    for (i1 in 1:s){ 
      thresh1 = mat2[i1,][order(-mat2[i1, ])[k1]]
      mat2[i1, mat2[i1,] < thresh1] = 0   # neat. didn't need 2 use () in the subset here.
      mat2[i1, mat2[i1,] > 0 ] = 1
      word = names(mat2[i1, mat2[i1,] > 0])
      mat2[(i1+1):nrow(mat2), match(word,colnames(mat2))] = 0
      wc = c(wc,word)
    } # i1 loop ends
    
    
    mat3 = mat2[match(wc, colnames(mat2)), match(wc, colnames(mat2))]
    ord = colnames(mat2)[which(!is.na(match(colnames(mat2), colnames(mat3))))]  # removed any NAs from the list
    mat4 = mat3[match(ord, colnames(mat3)), match(ord, colnames(mat3))]
    graph <- graph.adjacency(mat4, mode = "undirected", weighted=T)    # Create Network object
    graph = simplify(graph) 
    V(graph)$color[1:s] = "green"
    V(graph)$color[(s+1):length(V(graph))] = "pink"
    
    graph = delete.vertices(graph, V(graph)[ degree(graph) == 0 ]) # delete singletons?
    
    plot(graph, 
         layout = layout.kamada.kawai, 
         main = title)
  }
  display_word_cloud(corpus_dtm_tf)
  display_bar_graph(corpus_dtm_tf)
  display_cog(corpus_dtm_tf)
  
}