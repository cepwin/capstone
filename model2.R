##Load in data, 
require(Matrix)
require(quanteda)
require(readtext)
require(data.table)
require(ngram)
require(tm)


doLoad<-function() {
  text_blogs = readLines("./final/en_US/en_US.blogs.txt")
  text_news = readLines("./final/en_US/en_US.news.txt")
  text_twit = readLines("./final/en_US/en_US.twitter.txt")
  lenBlogs <-length(text_blogs)
  groups<-(lenBlogs*0.4)/10000 + 1
  lStrBlogs<-""
  for(i in 1:groups) {
    st<- (i-1)*10000 + 1
    end<-min(c(i*10000,lenBlogs))
    lStrBlogs<- concatenate(lStrBlogs,concatenate(lapply(text_blogs[st:end],"[",1)))
  }
  print("loaded blogs")
  
  lenNews <-length(text_news)
  groups<-(lenNews*0.7)/10000 + 1
  lStrNews<-""
  for(i in 1:groups) {
    st<- (i-1)*10000 + 1
    end<-min(c(i*10000,lenNews))
    lStrNews<- concatenate(lStrNews,concatenate(lapply(text_news[st:end],"[",1)))
  }
  print("loaded news")
  
  lenTwit <-length(text_twit)
  groups<-(lenTwit*0.8)/10000 + 1
  lStrTwit<-""
  for(i in 1:groups) {
    st<- (i-1)*10000 + 1
    end<-min(c(i*10000,lenTwit))
    lStrTwit<- concatenate(lStrTwit,concatenate(lapply(text_twit[st:end],"[",1)))
  }
  print("loaded twitter")
  
  text_string<-c(lStrBlogs,lStrNews)
  text_string<-c(text_string,lStrTwit)

  ng1<-quanteda::tokenize(text_string,ngrams=1L,concatenator=" ", remove_punct=TRUE, remove_numbers=TRUE,remove_symbols=TRUE)
  ng2<-quanteda::tokenize(text_string,ngrams=2L,concatenator=" ", remove_punct=TRUE, remove_numbers=TRUE,remove_symbols=TRUE)
  ng3<-quanteda::tokenize(text_string,ngrams=3L,concatenator=" ", remove_punct=TRUE, remove_numbers=TRUE,remove_symbols=TRUE)
  ng4<-quanteda::tokenize(text_string,ngrams=4L,concatenator=" ", remove_punct=TRUE, remove_numbers=TRUE,remove_symbols=TRUE)
  ng5<-quanteda::tokenize(text_string,ngrams=5L,concatenator=" ", remove_punct=TRUE, remove_numbers=TRUE,remove_symbols=TRUE)
  ng6<-quanteda::tokenize(text_string,ngrams=6L,concatenator=" ", remove_punct=TRUE, remove_numbers=TRUE,remove_symbols=TRUE)
  
  
  newdf<-list()
  totLst<-list()
  
  myDfm<-dfm(ng1)
  x<-colSums(myDfm)
  totLst[[1]]<-sum(x)
  newdf[[1]]<-as.data.table(cbind(names(x),as.numeric(x)))
  
  myDfm<-dfm(ng2)
  x<-colSums(myDfm)
  totLst[[2]]<-sum(x)
  newdf[[2]]<-as.data.table(cbind(names(x),as.numeric(x)))
  
  myDfm<-dfm(ng3)
  x<-colSums(myDfm)
  totLst[[3]]<-sum(x)
  newdf[[3]]<-as.data.table(cbind(names(x),as.numeric(x)))
  
  myDfm<-dfm(ng4)
  x<-colSums(myDfm)
  totLst[[4]]<-sum(x)
  newdf[[4]]<-as.data.table(cbind(names(x),as.numeric(x)))
  
  
  myDfm<-dfm(ng5)
  x<-colSums(myDfm)
  totLst[[5]]<-sum(x)
  newdf[[5]]<-as.data.table(cbind(names(x),as.numeric(x)))
  
  myDfm<-dfm(ng6)
  x<-colSums(myDfm)
  totLst[[6]]<-sum(x)
  newdf[[6]]<-as.data.table(cbind(names(x),as.numeric(x)))
  retList<-list()
  retList[[1]]<-newdf
  retList[[2]]<-totLst
  retList
}


dofind<-function(phase,n,tables,discount) {
##for each phase entered
tableSearch<-tables[[n]]
phase<-tolower(phase)
## convert to lower case  (either replace " " to "_" or remove when creating phase table)
searchterm<-concatenate(c(phase," [a-z]+$"),collapse="")
tableSearch$V2<-as.numeric(tableSearch$V2)*discount
print(head(tableSearch))
print(searchterm)
res<-tableSearch[grep(searchterm,tableSearch$V1),]
##search for phase (at end of n gram)
if(nrow(res) >= 1) {
  res[order(-rank(V2))]
} else {
  z<-strsplit(phase," ")
  numlen<-length(z[[1]])
  if(numlen>1) {
  ##removing table from memory
    tablesearch<-"" 
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

joins<-function(x) {z<-strsplit(x," ");len<-length(z[[1]]);x2<-concatenate(as.vector(z[[1]][1:len-1]));x2}
vec<-as.vector(lapply(vt,joins))
dt1<-as.data.table(cbind(aa,vec))



