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
    res<-dofind2(phase,1,conn)
    fst<-res
    dbDisconnect(conn)
    fst
  })
  
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
    res<-dbGetQuery(conn,paste("select v2 from ",tableName ," where v1 ='",phase,"' order by V3 desc limit 1",sep=""))
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
  
  output$nextWord<-renderText({getNextWord()})
  
})
