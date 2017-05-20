##Load in data, 
require(Matrix)
require(quanteda)
require(readtext)
require(data.table)

##text_string = readtext("./final/en_US/en_US.blogs.txt",textfield = "text")
##text_corpus<-quanteda::corpus(text_string)
text_string = readLines("./final/en_US/en_US.blogs.txt",n=50000)
text_string2 = readLines("./final/en_US/en_US.news.txt",n=50000)
text_string3 = readLines("./final/en_US/en_US.twitter.txt",n=50000)

##take % of each item for training set
text_string<-c(text_string[1:40000],text_string2[1:40000])
text_string<-c(text_string,text_string3[1:40000])
##create ngrams (1-6)
ng1<-quanteda::tokenize(text_corpus,ngrams=1L,remove_punct=TRUE, remove_numbers=TRUE,remove_symbols=TRUE)
ng2<-quanteda::tokenize(text_corpus,ngrams=2L,remove_punct=TRUE, remove_numbers=TRUE,remove_symbols=TRUE)
ng3<-quanteda::tokenize(text_corpus,ngrams=3L,remove_punct=TRUE, remove_numbers=TRUE,remove_symbols=TRUE)
ng4<-quanteda::tokenize(text_corpus,ngrams=4L,remove_punct=TRUE, remove_numbers=TRUE,remove_symbols=TRUE)
ng5<-quanteda::tokenize(text_corpus,ngrams=5L,remove_punct=TRUE, remove_numbers=TRUE,remove_symbols=TRUE)
ng6<-quanteda::tokenize(text_corpus,ngrams=6L,remove_punct=TRUE, remove_numbers=TRUE,remove_symbols=TRUE)

## create data.matrix with occurances and get sum totals

newdf<-list()

myDfm<-dfm(ng1)
x<-colSums(myDfm)
tot1<-sum(x)
perc<-x/tot
newdf[[1]]<-as.data.table(cbind(gsub("_"," ",names(x)),as.numeric(x)))

myDfm<-dfm(ng2)
x<-colSums(myDfm)
tot2<-sum(x)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     <-sum(x)
perc<-x/tot
newdf[[2]]<-as.data.table(cbind(gsub("_"," ",names(x)),as.numeric(x)))

myDfm<-dfm(ng3)
x<-colSums(myDfm)
tot3<-sum(x)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     <-sum(x)
perc<-x/tot
newdf[[3]]<-as.data.table(cbind(gsub("_"," ",names(x)),as.numeric(x)))

myDfm<-dfm(ng4)
x<-colSums(myDfm)
tot4<-sum(x)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     <-sum(x)
perc<-x/tot
newdf[[4]]<-as.data.table(cbind(gsub("_"," ",names(x)),as.numeric(x)))


myDfm<-dfm(ng5)
x<-colSums(myDfm)
tot5<-sum(x)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     <-sum(x)
perc<-x/tot
newdf[[5]]<-as.data.table(cbind(gsub("_"," ",names(x)),as.numeric(x)))

myDfm<-dfm(ng6)
x<-colSums(myDfm)
tot6<-sum(x)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     <-sum(x)
perc<-x/tot
newdf[[6]]<-as.data.table(cbind(gsub("_"," ",names(x)),as.numeric(x)))


dofind<-function(phase,n,tables,discount) {
##for each phase entered

## convert to lower case  (either replace " " to "_" or remove when creating phase table)
searchterm<-concatenate(c(phase," [a-z]+$"),collapse="")
tableSearch<-tables[[n]]
tableSearch$V2<-as.numeric(tableSearch$V2)*discount
res<-tableSearch[grep(searchterm,tableSearch$V1),]
##search for phase (at end of n gram)
if(nrow(res) >= 1) {
  res
} else {
  z<-strsplit(phase," ")
  numlen<-length(z[[1]])
  if(numlen>1) {
  phase<-concatenate(as.vector(z[[1]][2:numlen]))
  res<-dofind(phase,n-1,tables,discount*0.4)
  } else {
    "phase not found"
  }
}

##if it's found then return probabilities 
##  p(a/b) = p(A)*p(b)/p(b) (the prob of item with next word )
## if not found do it again with (n-1 gram)*d (discount)...if not found at all then return not found
}