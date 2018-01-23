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