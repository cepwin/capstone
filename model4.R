require(Matrix)
require(quanteda)
require(readtext)
require(data.table)
require(tm)
require(sqldf)
require(RSQLite)

##need to create a structure that is n-1 gram, ngram #ngram/#n-1 gram

joins<-function(x) {z<-strsplit(as.character(x)," ");len<-length(z[[1]]);x2<-concatenate(as.vector(z[[1]][1:len-1]))}


addPrev<-function(dt) {
  vec<-as.vector(lapply(dt$V1,joins))
  cbind(vec,dt)
}


doLoad<-function(start,end) {
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
  lStrBlogs<-""
  lstrNews<-""
  lStrTwit<-""
  newdf<-list()
  totLst<-list()
  
  for(i in start:end ) {
    gc()
    ng<-quanteda::tokenize(text_string,ngrams=i,concatenator=" ", remove_punct=TRUE, remove_twitter=TRUE,remove_numbers=TRUE,remove_symbols=TRUE)
    print("tokenized")
    myDfm<-dfm(ng)
    x<-colSums(myDfm)
    totLst[[i-1]]<-sum(x)
    tempDt<-as.data.table(cbind(as.vector(lapply(as.character(names(x)),joins)),names(x),as.numeric(x)))
    print("created initial data table")
##    names(tempDt)<-c("npgram","ngram","score")
##   tempDt$npgram<-as.character(tempDt$npgram)
##    setkey(tempDt,"npgram")
##    print("set key")
##    newdf[[i-1]]<-tempDt
    print(c("created dt ",i))
    fwrite(tempDt,paste("dt",i-1,sep = ""))
    print("table written")
    ng<-""
    tempDt<-""
    myDfm<-""
  }

}

readDts<-function() {
  newdf<-list()
  for(i in 1:7) {
    new1<-as.data.table(fread(paste("dt",i,sep = "")))
    setkey(new1,V1)
    newdf[[i]]<-new1
  }
  newdf  
}

putInSQL<-function() {
  conn<-dbConnect(SQLite(),'ngramdb.db')
  for(i in 1:9 ) {
    new1<-as.data.table(fread(paste("dt",i,sep = "")))
    dbWriteTable(conn, paste("dt",i,sep = ""), new1)
  }
  dbDisconnect(conn)
}

##this assumes newdf is in the environment
rundofind<-function(phase) {
  conn<-dbConnect(SQLite(),"ngramdb.db")
  res<-dofind2(phase,1,conn)
  print("got res")
  fst<-res$V2
  dbDisconnect(conn)
 fstt<-quanteda::tokenize(fst[[1]],ngrams=1)
 len<-length(fstt[[1]])
 fstt[[1]][len]
}

dofind2<-function(phase,discount,conn) {
  ##for each phase entered
  phase<-tolower(phase)
  z<-quanteda::tokenize(phase,ngrams=1,remove_punct=TRUE, remove_twitter=TRUE,remove_numbers=TRUE,remove_symbols=TRUE)
  numlen<-length(z[[1]])
  print(numlen)

  tableName<-paste("dt",numlen,sep="")
  res<-dbGetQuery(conn,paste("select * from ", "dt4" ," where V1='",phase,"' order by V3 desc limit 1",sep=""))
  if(nrow(res) == 1 && is.na(res$V3)) {
    "phase not found"
  }
  if(nrow(res) == 1) {
    res
  } else {
    ##  z<-strsplit(phase," ")
    ##  numlen<-length(z[[1]])
    if(numlen>1) {
      phase<-concatenate(as.vector(z[[1]][2:numlen]))
      res<-dofind(phase,discount*0.4,conn)
    } else {
      "phase not found"
    }
  }
  
}

dofind<-function(phase,tables,discount) {
  ##for each phase entered
  phase<-tolower(phase)
  z<-quanteda::tokenize(phase,ngrams=1,remove_punct=TRUE, remove_twitter=TRUE,remove_numbers=TRUE,remove_symbols=TRUE)
  numlen<-length(z[[1]])
  print(numlen)
  tableSearch<-tables[[numlen]]
  
  ## convert to lower case  (either replace " " to "_" or remove when creating phase table)
  ##searchterm<-concatenate(c(phase," [a-z]+$"),collapse="")
  tableSearch$V3<-as.numeric(tableSearch$V3)*discount
  print(head(tableSearch))
  print(phase)
  res<-tableSearch[phase]
  ##search for phase (at end of n gram)
  if(nrow(res) == 1 && is.na(res$V3)) {
    "phase not found"
  }
  if(nrow(res) > 1) {
    res[order(-rank(V3))]
  } else {
  ##  z<-strsplit(phase," ")
  ##  numlen<-length(z[[1]])
    if(numlen>1) {
      ##removing table from memory
      tablesearch<-"" 
      phase<-concatenate(as.vector(z[[1]][2:numlen]))
      res<-dofind(phase,tables,discount*0.4)
    } else {
      "phase not found"
    }
  }
  
  ##if it's found then return probabilities 
  ##  p(a/b) = p(A)*p(b)/p(b) (the prob of item with next word )
  ## if not found do it again with (n-1 gram)*d (discount)...if not found at all then return not found
}
##create a v3 that is the n-1 gram...ie if you have "this is the", v3 would be "this is"...tokenize 1:n-1

##write structure to sqldf

##query on sqldf

##create dt for 1-6

