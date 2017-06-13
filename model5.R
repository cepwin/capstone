library(Matrix)
library(readtext)
library(data.table)
library(tm)
library(sqldf)
library(RSQLite)
library(ngram)
library(quanteda)

joins<-function(x) {z<-strsplit(as.character(x)," ");len<-length(z[[1]]);x2<-concatenate(as.vector(z[[1]][1:len-1]))}
lastToken<-function(x) {z<-strsplit(as.character(x)," ");len<-length(z[[1]]);x2<-as.vector(z[[1]][len])}


##was .8,.8,.7, .3
##trying .4,.4,.35,.6
doLoadNew<-function() {
  text_blogs = readLines("./final/en_US/en_US.blogs.txt")
  text_news = readLines("./final/en_US/en_US.news.txt")
  text_twit = readLines("./final/en_US/en_US.twitter.txt")  
  newcorp<-quanteda::corpus(text_news)
  blogcorp<-quanteda::corpus(text_blogs)
  twitcorp<-quanteda::corpus(text_twit)
  set.seed(483084)
  newSample<-sample(text_news,length(text_news)*.4,replace=FALSE)
  blogSample<-sample(text_blogs,length(text_blogs)*.4,replace=FALSE)
  twitSample<-sample(text_twit,length(text_twit)*.35,replace=FALSE)
   newTrain<- sample(1:length(text_news),length(newSample)*.75)
   blogTrain<-sample(1:length(text_blogs),length(blogSample)*.75)
   twitTrain<-sample(1:length(text_twit),length(twitSample)*.75)
  trainNew<-newSample[newTrain]
  testNew<-newSample[-newTrain]
  trainBlog<-blogSample[blogTrain]
  testBlog<-blogSample[-blogTrain]
  trainTwit<-twitSample[twitTrain]
  testTwit<-twitSample[-twitTrain]
  trainSet<-c(trainNew,trainBlog,trainTwit)
  testSet<-c(testNew,testBlog,testTwit)
  ng<-quanteda::tokenize(testSet,ngrams=6,concatenator=" ", remove_punct=TRUE, remove_twitter=TRUE,remove_numbers=TRUE,remove_symbols=TRUE)
  tmpdf<-dfm(ng)
  fwrite(as.data.table(featnames(tmpdf)),"testout", append = FALSE)
  for(i in 2:6 ) {
    ng<-quanteda::tokenize(trainSet,ngrams=i,concatenator=" ", remove_punct=TRUE, remove_twitter=TRUE,remove_numbers=TRUE,remove_symbols=TRUE)
    print("tokenized")
    myDfm<-dfm(ng)
    ng<-""
    x<-colSums(myDfm)
 ##   numfav<-as.integer(.60*length(x))
   ## x1<-topfeatures(myDfm,numfav)
    x1<-x
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

testalg<-function() {
  testitems<-as.data.table(readLines("./testout"))
  testitems1<-sample(testitems$V1,3000,replace=FALSE)
  numCorrect<-0
  numWrong<-0
  items<-length(testitems1)
  enthC<-0
  sumnumC<-0
  enthI<-0
  sumnumI<-0
  badword<-vector()
  for(i in 2:items) {
    tok<-quanteda::tokenize(testitems1[i],ngrams=1,remove_punct=TRUE, remove_twitter=TRUE,remove_numbers=TRUE,remove_symbols=TRUE)
    ans<-tok[[1]][6]
    phrase<-concatenate(as.vector(tok[[1]][1:5]))
    res<-getNextWord(phrase)
    if(is.atomic(res)) {
      word<-res
      prob<-0
      badword<-c(badword,phrase)
      print(paste("badword: ",phrase))
      print(paste("number badwords: ", length(badword)))
    } else {
        word<-res$word
        prob<-res$prob
   }
    if(ans==word) {
      numCorrect<-numCorrect+1
      sumnumC<-sumnumC + log(prob)
      enthC<-(-1*(sumnumC/numCorrect))
    } else {
      if(prob>0) {
      numWrong<-numWrong+1
      sumnumI<-sumnumI + log(prob)
      enthI<-(-1*(sumnumI/numWrong))
    }
    }
    print(paste("answer: ",ans, " result: ",word))
    percent<-(numCorrect/(numWrong+numCorrect))*100
    
    print(paste("Num correct: ", numCorrect," Num wrong: ",numWrong,"percent correct: ",percent,"Correct logP: ",enthC,"Incorrect LogP: ",enthI))
  }
  print("the bad words were")
  print(badword)
  print(paste("the number of bad words were ", length(badword)))
}

getNextWord<-function(phase) {
  conn<-dbConnect(SQLite(),"ngramdb.db")
  res<-dofind3(phase,0,conn)
  fst<-res
  dbDisconnect(conn)
  fst
}

dofind2<-function(phase,discount,conn) {
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
    res<-dbGetQuery(conn,paste("select v2 from ",tableName ," where v1 =\"",phase,"\" order by V3 desc limit 1",sep=""))
    # if(nrow(res) == 1 && is.na(res)) {
    #  "phase not found"
    #}
    if(nrow(res) == 1) {
      res[[1]]
    } else {
      ##  z<-strsplit(phase," ")
      ##  numlen<-length(z[[1]])
      if(numlen>1) {
        phase<-concatenate(as.vector(z[[1]][2:numlen]))
        res<-dofind2(phase,discount*0.4,conn)
      } else {
        "phase not found"
      }
    }
  }
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
  print(paste("phrase: ",phase))
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
      #res2[1,]$word
      res2[1,]
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