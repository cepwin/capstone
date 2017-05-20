library(tm)
library(ngram)
library(RWeka)


ovid<-PCorpus(DirSource("./final/en_US/"),dbControl=list(dbName="test.db", dbType="DB1"))
ovid<-tm_map(ovid, stripWhitespace)
ovid<-tm_map(ovid,content_transformer(tolower))
ovid<-tm_map(ovid,removePunctuation)
ovid<-tm_map(ovid,removeWords,stopwords("english"))

library(stringi)
library(SnowballC)
out <- stri_extract_all_words(stri_trans_tolower(SnowballC::wordStem(ovid[[1]], "english"))) #in old package versions it was named 'stri_extract_words'
names(out) <- paste0("doc", 1:length(out))

lev <- sort(unique(unlist(out)))
dat <- do.call(cbind, lapply(out, function(x, lev) {
  tabulate(factor(x, levels = lev, ordered = TRUE), nbins = length(lev))
}, lev = lev))
rownames(dat) <- sort(lev)

library(tm)
dat <- dat[!rownames(dat) %in% tm::stopwords("english"), ] 

library(slam)
dat2 <- slam::as.simple_triplet_matrix(dat)

tdm <- tm::as.TermDocumentMatrix(dat2, weighting=weightTf)
tdm
