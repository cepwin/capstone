#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(tm)
library(ngram)
library(shiny)
library(quanteda)
library(data.table)
library(sqldf)
library(RSQLite)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  ##this assumes newdf is in the environment
 getNextWord<-reactive ({
    phase<-input$phrase
    conn<-dbConnect(SQLite(),"ngramdb.db")
    res<-dofind3(phase,6,conn)
    if(is.atomic(res)) {
      fst<-res
    } else {
      fst<-res$word
    }
    dbDisconnect(conn)
    fst
  })
  
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
  
  output$nextWord<-renderText({getNextWord()})
  
})
