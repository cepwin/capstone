require(Matrix)
require(quanteda)
require(readtext)
require(tm)
##otherStop<-c(intToUtf8(226),intToUtf8(8364),intToUtf8(8482))
##ovid<-VCorpus(DirSource("./final/en_US/"))
##ovid<-tm_map(ovid, stripWhitespace)
##ovid<-tm_map(ovid,tolower)
##ovid<-tm_map(ovid,removeWords,stopwords("english"))
##ovid<-tm_map(ovid,removePunctuation)
text_string = readLines("./final/en_US/en_US.blogs.txt")
##text_string = removePunctuation(text_string,preserve_intra_word_dashes=TRUE)
##ovid<-tm::VCorpus(VectorSource(as.vector(text_string)))
##ovid<-tm_map(ovid, stripWhitespace)
##ovid<-tm_map(ovid,tolower)
##ovid<-tm_map(ovid,removeWords,stopwords("english"))
##ovid<-tm_map(ovid,removePunctuation)
text_string = readLines("./final/en_US/en_US.blogs.txt")


text_string = readtext("./final/en_US/en_US.blogs.txt",textfield = "text")
text_corpus<-quanteda::corpus(text_string)
ng<-quanteda::tokenize(text_corpus,ngrams=3L,remove_punct=TRUE, remove_numbers=TRUE,remove_symbols=TRUE)


text_corpus<-quanteda::corpus(text_string)
tok<-quanteda::tokenize(text_corpus,remove_numbers = TRUE)
##tok<-removeFeatures(tok,stopwords("english"))
ng<-tokens_ngrams(tok,n=3L,remove_punct=TRUE, remove_symbols=TRUE)
myDfm<-dfm(ng)
DT_freq<-sort(colSums(myDfm), decreasing = TRUE)
DT_freq[1:25]



