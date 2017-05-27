require(Matrix)
require(quanteda)
require(readtext)
require(data.table)

##text_string = readtext("./final/en_US/en_US.blogs.txt",textfield = "text")
##text_corpus<-quanteda::corpus(text_string)
text_string = readLines("./final/en_US/en_US.blogs.txt",n=200000)
text_string2 = readLines("./final/en_US/en_US.news.txt",n=200000)
text_string3 = readLines("./final/en_US/en_US.twitter.txt",n=200000)
text_string<-c(text_string,text_string2)
text_string<-c(text_string,text_string3)
ng<-quanteda::tokenize(text_corpus,ngrams=3L,remove_punct=TRUE, remove_numbers=TRUE,remove_symbols=TRUE)
myDfm<-dfm(ng)
DT_freq<-sort(colSums(myDfm), decreasing = TRUE)
DT_freq[1:25]
x<-colSums(myDfm)
tot<-sum(x)
perc<-x/tot
newdf<-as.data.table(cbind(names(perc),as.numeric(perc)))
ng<-quanteda::tokenize(text_corpus,ngrams=4L,remove_punct=TRUE, remove_numbers=TRUE,remove_symbols=TRUE)
myDfm4<-dfm(ng)
DT_freq<-sort(colSums(myDfm4), decreasing = TRUE)
DT_freq[1:25]
x<-colSums(myDfm4)
tot<-sum(x)
perc<-x/tot
newdf4<-as.data.table(cbind(names(perc),as.numeric(perc)))
newdt[grep("in_the_years",newdt$V1),]
newdf4[grep("in_the_years",newdf4$V1),]





##text_string = readtext("./final/en_US/en_US.blogs.txt",textfield = "text")
##text_corpus<-quanteda::corpus(text_string)
text_string = readLines("./final/en_US/en_US.blogs.txt",n=200000)
text_string2 = readLines("./final/en_US/en_US.news.txt",n=200000)
text_string3 = readLines("./final/en_US/en_US.twitter.txt",n=200000)

##take % of each item for training set
text_string<-c(text_string[1:140000],text_string2[1:77000])
text_string<-c(text_string,text_string3[1:140000])
##create ngrams (1-20)
ng1<-quanteda::tokenize(text_corpus,ngrams=1L,remove_punct=TRUE, remove_numbers=TRUE,remove_symbols=TRUE)
ng2<-quanteda::tokenize(text_corpus,ngrams=2L,remove_punct=TRUE, remove_numbers=TRUE,remove_symbols=TRUE)
ng3<-quanteda::tokenize(text_corpus,ngrams=3L,remove_punct=TRUE, remove_numbers=TRUE,remove_symbols=TRUE)
ng4<-quanteda::tokenize(text_corpus,ngrams=4L,remove_punct=TRUE, remove_numbers=TRUE,remove_symbols=TRUE)
ng5<-quanteda::tokenize(text_corpus,ngrams=5L,remove_punct=TRUE, remove_numbers=TRUE,remove_symbols=TRUE)
ng6<-quanteda::tokenize(text_corpus,ngrams=6L,remove_punct=TRUE, remove_numbers=TRUE,remove_symbols=TRUE)
ng7<-quanteda::tokenize(text_corpus,ngrams=7L,remove_punct=TRUE, remove_numbers=TRUE,remove_symbols=TRUE)
ng8<-quanteda::tokenize(text_corpus,ngrams=8L,remove_punct=TRUE, remove_numbers=TRUE,remove_symbols=TRUE)
ng9<-quanteda::tokenize(text_corpus,ngrams=9L,remove_punct=TRUE, remove_numbers=TRUE,remove_symbols=TRUE)
ng10<-quanteda::tokenize(text_corpus,ngrams=10L,remove_punct=TRUE, remove_numbers=TRUE,remove_symbols=TRUE)
ng11<-quanteda::tokenize(text_corpus,ngrams=11L,remove_punct=TRUE, remove_numbers=TRUE,remove_symbols=TRUE)
ng12<-quanteda::tokenize(text_corpus,ngrams=12L,remove_punct=TRUE, remove_numbers=TRUE,remove_symbols=TRUE)
ng13<-quanteda::tokenize(text_corpus,ngrams=13L,remove_punct=TRUE, remove_numbers=TRUE,remove_symbols=TRUE)
ng14<-quanteda::tokenize(text_corpus,ngrams=14L,remove_punct=TRUE, remove_numbers=TRUE,remove_symbols=TRUE)
ng15<-quanteda::tokenize(text_corpus,ngrams=15L,remove_punct=TRUE, remove_numbers=TRUE,remove_symbols=TRUE)
ng16<-quanteda::tokenize(text_corpus,ngrams=16L,remove_punct=TRUE, remove_numbers=TRUE,remove_symbols=TRUE)
ng17<-quanteda::tokenize(text_corpus,ngrams=17L,remove_punct=TRUE, remove_numbers=TRUE,remove_symbols=TRUE)
ng18<-quanteda::tokenize(text_corpus,ngrams=18L,remove_punct=TRUE, remove_numbers=TRUE,remove_symbols=TRUE)

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

myDfm<-dfm(ng7)
x<-colSums(myDfm)
tot7<-sum(x)
perc<-x/tot
newdf[[7]]<-as.data.table(cbind(gsub("_"," ",names(x)),as.numeric(x)))

myDfm<-dfm(ng8)
x<-colSums(myDfm)
tot2<-sum(x)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     <-sum(x)
perc<-x/tot
newdf[[8]]<-as.data.table(cbind(gsub("_"," ",names(x)),as.numeric(x)))

myDfm<-dfm(ng9)
x<-colSums(myDfm)
tot3<-sum(x)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     <-sum(x)
perc<-x/tot
newdf[[9]]<-as.data.table(cbind(gsub("_"," ",names(x)),as.numeric(x)))

myDfm<-dfm(ng10)
x<-colSums(myDfm)
tot4<-sum(x)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     <-sum(x)
perc<-x/tot
newdf[[10]]<-as.data.table(cbind(gsub("_"," ",names(x)),as.numeric(x)))


myDfm<-dfm(ng11)
x<-colSums(myDfm)
tot5<-sum(x)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     <-sum(x)
perc<-x/tot
newdf[[11]]<-as.data.table(cbind(gsub("_"," ",names(x)),as.numeric(x)))

myDfm<-dfm(ng12)
x<-colSums(myDfm)
tot6<-sum(x)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     <-sum(x)
perc<-x/tot
newdf[[12]]<-as.data.table(cbind(gsub("_"," ",names(x)),as.numeric(x)))

myDfm<-dfm(ng13)
x<-colSums(myDfm)
tot4<-sum(x)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     <-sum(x)
perc<-x/tot
newdf[[13]]<-as.data.table(cbind(gsub("_"," ",names(x)),as.numeric(x)))


myDfm<-dfm(ng14)
x<-colSums(myDfm)
tot5<-sum(x)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     <-sum(x)
perc<-x/tot
newdf[[14]]<-as.data.table(cbind(gsub("_"," ",names(x)),as.numeric(x)))

myDfm<-dfm(ng15)
x<-colSums(myDfm)
tot6<-sum(x)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     <-sum(x)
perc<-x/tot
newdf[[15]]<-as.data.table(cbind(gsub("_"," ",names(x)),as.numeric(x)))

myDfm<-dfm(ng16)
x<-colSums(myDfm)
tot4<-sum(x)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     <-sum(x)
perc<-x/tot
newdf[[16]]<-as.data.table(cbind(gsub("_"," ",names(x)),as.numeric(x)))


myDfm<-dfm(ng17)
x<-colSums(myDfm)
tot5<-sum(x)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     <-sum(x)
perc<-x/tot
newdf[[17]]<-as.data.table(cbind(gsub("_"," ",names(x)),as.numeric(x)))

myDfm<-dfm(ng18)
x<-colSums(myDfm)
tot6<-sum(x)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     <-sum(x)
perc<-x/tot
newdf[[18]]<-as.data.table(cbind(gsub("_"," ",names(x)),as.numeric(x)))
