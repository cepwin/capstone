library(tm)
library(ngram)
library(RWeka)

ovid<-VCorpus(DirSource("./final/en_US/"))
ovid<-tm_map(ovid, stripWhitespace)
ovid<-tm_map(ovid,content_transformer(tolower))
ovid<-tm_map(ovid,removePunctuation)
ovid<-tm_map(ovid,removeWords,stopwords("english"))
ovid<-tm_map(ovid,removeSparseTerms)
ovid<-tm_map(ovid,removeNumbers)

strTwit<-ovid[[3]]
lenTwit <-length(strTwit$content)*0.6 ##training set
groups<-lenTwit/10000 + 1
lStrTwit<-""
for(i in 1:groups) {
  st<- (i-1)*10000 + 1
  end<-min(c(i*10000,lenTwit))
  lStrTwit<- concatenate(lStrTwit,concatenate(lapply(strTwit$content[st:end],"[",1)))
}

ngTwit<-ngram(lStrTwit,3)

ngf<-get.phrasetable(ngTwit)

head(ngf)

