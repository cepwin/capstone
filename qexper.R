require(Matrix)
require(quanteda)
require(readtext)
require(tm)
ovid<-VCorpus(DirSource("./final/en_US/"))
##ovid<-readtext("./final/en_Us/en_US.blogs")
ovid<-tm_map(ovid, stripWhitespace)
ovid<-tm_map(ovid,content_transformer(tolower))
ovid<-tm_map(ovid,removePunctuation)
ovid<-tm_map(ovid,removeWords,stopwords("english"))
ovid<-tm_map(ovid,removeSparseTerms)
ovid<-tm_map(ovid,removeNumbers)
text_corpus<-corpus(ovid[[1]]$content)
otherStop<-c(intToUtf8(226),intToUtf8(8364),intToUtf8(8482))
docFeatures<-dfm(text_corpus, remove = c(quanteda::stopwords("english"),otherStop))

topfeatures(docFeatures,n=20)
sentences <- parallelizeTask(makeSentences, ovid[1])
makeTokens <- function(input, n = 1L) {
quanteda::tokenize(input, what = "word", removeNumbers = TRUE,
removePunct = TRUE, removeSeparators = TRUE,
removeTwitter = FALSE, removeHyphens = TRUE,
ngrams = n, simplify = TRUE)
}
getProfanityWords <- function(corpus) {
profanityFileName <- "profanity.txt"
if (!file.exists(profanityFileName)) {
profanity.url <- "https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
download.file(profanity.url, destfile = profanityFileName, method = "curl")
}
if (sum(ls() == "profanity") < 1) {
profanity <- read.csv(profanityFileName, header = FALSE, stringsAsFactors = FALSE)
profanity <- profanity$V1
profanity <- profanity[1:length(profanity)-1]
}
profanity
}

parallelizeTask<-function(task, ...) {
  # Calculate the number of cores
  ncores <- detectCores() - 1
  # Initiate cluster
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  #print("Starting task")
  r <- task(...)
  #print("Task done")
  stopCluster(cl)
  r
}

