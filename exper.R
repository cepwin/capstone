library(tm)
library(ngram)
library(RWeka)


ovid<-PCorpus(DirSource("./final/en_US/"),dbControl=list(dbName="test.db", dbType="DB1"))
ovid<-VCorpus(DirSource("./final/en_US/"))
ovid<-tm_map(ovid, stripWhitespace)
ovid<-tm_map(ovid,content_transformer(tolower))
ovid<-tm_map(ovid,removePunctuation)
ovid<-tm_map(ovid,removeWords,stopwords("english"))
ovid<-tm_map(ovid,removeSparseTerms)
ovid<-tm_map(ovid,removeNumbers)

small<-ovid[[1]]$content[1:10000]
corp<-Corpus(VectorSource(small))
dtm<-DocumentTermMatrix(corp)
inspect(dtm)
maxterms<-apply(dtm,1,which.max)
dtm$dimnames$Terms[maxterms]

xx<-as.matrix(dtmBlog)
xxdf<-as.data.frame.matrix(xx)
cs<-colSums(xxdf)
terms<-cbind(names(xxdf),as.vector(as.numeric(cs)))
dfTerms<-as.data.frame(terms)
names(dfSort)<-c("Term","Count")

dfSort<-dfTerms[order(dfTerms[2]),]

tail(dfSort,20)



for(i in 0:length(ovid)-1) {
meta(ovid[[i]],"id")
inspect(ovid[[i]])
str<-strwrap(ovid[[i]])
length(str)
lStr<-concatenate(lapply(str,"[",1))
summ[i]<-string.summary(lStr)
ng2[i]<-ngram(lStr,3)
print(ng2[i], output="summary")
pt2[i]<-get.phrasetable(ng2)
}
gp<-ggplot(pt2[0],aes(x=ngrams, y=freq)) + geom_point() + theme(axis.text.x=element_text(angle = 90, vjust = 0.5))

TrigramTokenizer<-function(x) NGramTokenizer(x, Weka_control(min=3, max=3))
tk<-TrigramTokenizer(ovid[[2]])
tkStr<-concatenate((lapply(tk,"[",1)))
tdm = TermDocumentMatrix(ovid[[2]], control=list(tokenize=TrigramTokenizer))

strsplit_space_tokenizer <- function(x)
  unlist(strsplit(as.character(x), "[[:space:]]+"))
ctrl <- list(tokenize = strsplit_space_tokenizer,
             removePunctuation = list(preserve_intra_word_dashes = TRUE),
             stopwords = c("reuter", "that"),
             stemming = TRUE,
             wordLengths = c(4, Inf))
ctrl <- list(tokenize = strsplit_space_tokenizer,
             removePunctuation = list(preserve_intra_word_dashes = TRUE),
             stemming = TRUE,
             wordLengths= c(4, Inf))
tf<-termFreq(ovid[[1]], control = ctrl)