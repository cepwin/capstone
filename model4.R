library(Matrix)
require(quanteda)
require(readtext)
require(data.table)
require(tm)
require(sqldf)
require(RSQLite)
require(ngram)

##need to create a structure that is n-1 gram, ngram #ngram/#n-1 gram

joins<-function(x) {z<-strsplit(as.character(x)," ");len<-length(z[[1]]);x2<-concatenate(as.vector(z[[1]][1:len-1]))}
lastToken<-function(x) {z<-strsplit(as.character(x)," ");len<-length(z[[1]]);x2<-as.vector(z[[1]][len])}


addPrev<-function(dt) {
  vec<-as.vector(lapply(dt$V1,joins))
  cbind(vec,dt)
}


doLoad<-function(start,end) {
  text_blogs = readLines("./final/en_US/en_US.blogs.txt")
  text_news = readLines("./final/en_US/en_US.news.txt")
  text_twit = readLines("./final/en_US/en_US.twitter.txt")
  lenBlogs <-length(text_blogs)
  groups<-(lenBlogs*.4)/10000 + 1
  lStrBlogs<-""
  for(i in 1:groups) {
    st<- (i-1)*10000 + 1
    end<-min(c(i*10000,lenBlogs))
    lStrBlogs<- concatenate(lStrBlogs,concatenate(lapply(text_blogs[st:end],"[",1)))
  }
  print("loaded blogs")
  
  lenNews <-length(text_news)
  groups<-(lenNews*.7)/10000 + 1
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

doWordsLoad<-function() {
  text_blogs = readLines("./final/en_US/en_US.blogs.txt")
  text_news = readLines("./final/en_US/en_US.news.txt")
  text_twit = readLines("./final/en_US/en_US.twitter.txt")
  lenBlogs <-length(text_blogs)
  groups<-(lenBlogs*.5)/10000 + 1
  lStrBlogs<-""
  for(i in 1:groups) {
    st<- (i-1)*10000 + 1
    end<-min(c(i*10000,lenBlogs))
    lStrBlogs<- concatenate(lStrBlogs,concatenate(lapply(text_blogs[st:end],"[",1)))
  }
  print("loaded blogs")
  
  lenNews <-length(text_news)
  groups<-(lenNews*.7)/10000 + 1
  lStrNews<-""
  for(i in 1:groups) {
    st<- (i-1)*10000 + 1
    end<-min(c(i*10000,lenNews))
    lStrNews<- concatenate(lStrNews,concatenate(lapply(text_news[st:end],"[",1)))
  }
  print("loaded news")
  
  lenTwit <-length(text_twit)
  groups<-(lenTwit)/10000 + 1
  lStrTwit<-""
  for(i in 1:groups) {
    st<- (i-1)*10000 + 1
    end<-min(c(i*10000,lenTwit))
    lStrTwit<- concatenate(lStrTwit,concatenate(lapply(text_twit[st:end],"[",1)))
  }
  print("loaded twitter")
  text_string<-c(lStrBlogs,lStrNews)
  text_string<-c(text_string,lStrTwit)
  lStrBlogs=""
  lStrNews=""
  lStrTwit=""
  newdf<-list()
  totLst<-list()
  ng<-quanteda::tokenize(text_string,ngrams=1,concatenator=" ", remove_punct=TRUE, remove_twitter=TRUE,remove_numbers=TRUE,remove_symbols=TRUE)
  myDfm<-dfm(ng)
  ng<-""
   ### myDfm<-rbind(myDfm1,myDfm2,myDfm3)
  x<-colSums(myDfm)
  v1<-as.vector(names(x))
  v2<-as.vector(1:length(v1))
  dtwords<-as.data.table(cbind(v2,v1))
  fwrite(dtwords,"dtwords")
}

doLoad2<-function() {
  ##text_blogs = readLines("./final/en_US/en_US.blogs.txt")
  text_news = readLines("./final/en_US/en_US.news.txt")
  text_twit = readLines("./final/en_US/en_US.twitter.txt")
  lenNews <-length(text_news)
 ## groups<-(lenNews*.7)/10000 + 1
  totgroups<-(lenNews/10000) + 1
  groups<-((lenNews*.8)/10000) + 1
  lStrNews<-""
  for(i in 1:groups) {
    st<- (i-1)*10000 + 1
    end<-min(c(i*10000,lenNews))
    lStrNews<- concatenate(lStrNews,concatenate(lapply(text_news[st:end],"[",1)))
  }
  lStrNewsTest<-""
  for(i in (groups+1):totgroups) {
    st<- (i-1)*10000 + 1
    end<-min(c(i*10000,lenNews))
    lStrNewsTest<- concatenate(lStrNewsTest,concatenate(lapply(text_news[st:end],"[",1)))
  }
  ng<-quanteda::tokenize(lStrNewsTest,ngrams=5,concatenator=" ", remove_punct=TRUE, remove_twitter=TRUE,remove_numbers=TRUE,remove_symbols=TRUE)
  fwrite(ng,"testout")
  lStrNewsTest=""
  print("loaded news")
  
  lenTwit <-length(text_twit)
  totgroups<-(lenTwit/10000) + 1
  groups<-((lenTwit*.8)/10000) + 1
  lStrTwit<-""
  for(i in 1:groups) {
    st<- (i-1)*10000 + 1
    end<-min(c(i*10000,lenTwit))
    lStrTwit<- concatenate(lStrTwit,concatenate(lapply(text_twit[st:end],"[",1)))
  }
  lStrTwitTest<-""
  for(i in (groups+1):totgroups) {
    st<- (i-1)*10000 + 1
    end<-min(c(i*10000,lenTwit))
    lStrTwitTest<- concatenate(lStrTwitTest,concatenate(lapply(text_twit[st:end],"[",1)))
  }
  ng<-quanteda::tokenize(lStrTwitTest,ngrams=5,concatenator=" ", remove_punct=TRUE, remove_twitter=TRUE,remove_numbers=TRUE,remove_symbols=TRUE)
  fwrite(ng,"testout", append = TRUE)
  lStrTwitTest=""
  print("loaded twitter")
 ## text_string<-c(lStrBlogs,lStrNews)
  text_string<-c(lStrNews,lStrTwit)
 ## lStrBlogs=""
  lStrNews=""
  lStrTwit=""
  newdf<-list()
  totLst<-list()
##  ng<-quanteda::tokenize(text_string,ngrams=1,concatenator=" ", remove_punct=TRUE, remove_twitter=TRUE,remove_numbers=TRUE,remove_symbols=TRUE)
  
  for(i in 2:6 ) {
    ng<-quanteda::tokenize(text_string,ngrams=i,concatenator=" ", remove_punct=TRUE, remove_twitter=TRUE,remove_numbers=TRUE,remove_symbols=TRUE)
    print("tokenized")
    myDfm<-dfm(ng)
    ng<-""
    x<-colSums(myDfm)
    numfav<-as.integer(.3125*length(x))
    x1<-topfeatures(myDfm,numfav)
    v1<-as.vector(lapply(as.character(names(x1)),joins))
    v2<-unlist(lapply(as.character(names(x1)),lastToken))
    v3<-unlist(as.vector(as.integer(x1)))
    new1<-as.data.table(cbind(v1,v2,v3))
    fwrite(new1,paste("dt",i-1,sep = ""))
    new1<-""
    print("table written")
    ng<-""
    tempDt<-""
    myDfm<-""
  }
}

readDts<-function(max) {
  newdf<-list()
  for(i in 1:max) {
    new1<-as.data.table(fread(paste("dt",i,sep = "")))
    setkey(new1,V1)
    newdf[[i]]<-new1
  }
  newdf  
}

putInSQL2<-function() {
  conn<-dbConnect(SQLite(),'ngramdb.db')
  for(i in 1:5 ) {
    new1<-as.data.table(fread(paste("dt",i,sep = "")))
  ##  new1<-new1[order(-v3)]
  ##  rows<-nrow(new1)*0.15
  ##  new1<-new1[1:rows,]
    dbWriteTable(conn, paste("dt",i,sep = ""), new1,overwrite=TRUE, append=FALSE)
  }
  dbDisconnect(conn)
}

putInSQL3<-function() {
  conn<-dbConnect(SQLite(),'ngramdb2.db')
  dtwords<-as.data.table(fread("dtwords"))
  names(dtwords)<-c("wordid","word")
  setkey(dtwords,word)
  for(i in 1:5 ) {
    new1<-as.data.table(fread(paste("dt",i,sep = "")))
    new1<-new1[order(-v3)]
    rows<-nrow(new1)*0.18
    new1<-new1[1:rows,]
    names(new1)<-c("phrase","word","num")
    setkey(new1,word)
    new1<-new1[dtwords,nomatch=0]
    new2<-as.data.table(cbind(new1$phrase,new1$wordid,new1$num))
    dbWriteTable(conn, paste("dt",i,sep = ""), new2,overwrite=TRUE, append=FALSE)
    new2<-""
  }
  dbWriteTable(conn,"dtwords",dtwords,overwrite=TRUE,append=FALSE)
  dbDisconnect(conn)
}

putInSQL<-function() {
  conn<-dbConnect(SQLite(),'ngramdb.db')
  for(i in 1:9 ) {
    new1<-as.data.table(fread(paste("dt",i,sep = "")))
    rows<-nrow(new1)*0.15
    new1<-new1[1:rows,]
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
  res<-dbGetQuery(conn,paste("select * from ", tableName ," where V1='",phase,"' order by V3 desc limit 1",sep=""))
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

getNextWord<-function(phase) {
  conn<-dbConnect(SQLite(),"ngramdb.db")
  res<-dofind3(phase,0,conn)
  fst<-res
  dbDisconnect(conn)
  fst
}




##create dt for 1-6
dofind3<-function(phase,discount,conn) {
  ##for each phase entered
  phase<-tolower(phase)
  z<-quanteda::tokenize(phase,ngrams=1,remove_punct=TRUE, remove_twitter=TRUE,remove_numbers=TRUE,remove_symbols=TRUE)
  numlen<-length(z[[1]])
  print(paste("pre ",numlen))
  if(numlen>5) {
    phase<-concatenate(as.vector(z[[1]][(numlen-4):numlen]))
    numlen<-5
  }
  print(numlen)
  if(numlen > 0) {
    tableName<-paste("dt",numlen,sep="")
    res<-dbGetQuery(conn,paste("select v2, v3 from ",tableName ," where v1 =\"",phase,"\" order by V3 desc",sep=""))
    tot<-sum(res$v3)
    res2<-as.data.table(cbind(res,(res$v3-0.5)))
    names(res2)<-c("word","c","cST")       
    prob<-res2$cST/tot
    if(discount == 1) {
      disc<-1-sum(prob)
      prob<-(res2$c/tot)*disc
    }
    res2<-as.data.table(cbind(res2,prob))
    ##prop = c*/tot * discount (one if not rec)
    ##sort by prop and return top prop
    if(nrow(res2) >= 1) {
      res2[order(-rank(prob))]
      res2[1,]$word
    } else {
      ##  z<-strsplit(phase," ")
      ##  numlen<-length(z[[1]])
      if(numlen>1) {
        phase<-concatenate(as.vector(z[[1]][2:numlen]))
        res<-dofind3(phase,1,conn)
      } else {
        "phase not found"
      }
    }
  }
}

