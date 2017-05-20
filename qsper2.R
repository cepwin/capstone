require(Matrix)
require(quanteda)
require(readtext)
require(data.table)

##text_string = readtext("./final/en_US/en_US.blogs.txt",textfield = "text")
##text_corpus<-quanteda::corpus(text_string)
text_string = readLines("./final/en_US/en_US.blogs.txt",n=30000)
text_string2 = readLines("./final/en_US/en_US.news.txt",n=30000)
text_string3 = readLines("./final/en_US/en_US.twitter.txt",n=30000)
text_string<-c(text_string,text_string2)
text_string<-c(text_string,text_string3)
ng<-quanteda::tokenize(text_corpus,ngrams=3L,remove_punct=TRUE, remove_numbers=TRUE,remove_symbols=TRUE)
myDfm<-dfm(ng)
DT_freq<-sort(colSums(myDfm), decreasing = TRUE)
DT_freq[1:25]
x<-colSums(myDfm)
tot<-sum(x)
perc<-x/tot
newdf<-as.data.table(cbind(names(perc),as.numeric(perc)))
ng<-quanteda::tokenize(text_corpus,ngrams=4L,remove_punct=TRUE, remove_numbers=TRUE,remove_symbols=TRUE)
myDfm4<-dfm(ng)
DT_freq<-sort(colSums(myDfm4), decreasing = TRUE)
DT_freq[1:25]
x<-colSums(myDfm4)
tot<-sum(x)
perc<-x/tot
newdf4<-as.data.table(cbind(names(perc),as.numeric(perc)))
newdt[grep("in_the_years",newdt$V1),]
newdf4[grep("in_the_years",newdf4$V1),]