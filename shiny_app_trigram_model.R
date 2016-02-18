################################################################
#Author: S. Grunert
#Created: 17August2015
#Version: 1.3
#Description:
#This is a manual script for producing a trigram frequency model.
#################################################################


####Set preconditions.
setwd("C:/coursera/CapstoneProject")
rm(list=ls())


####Load Libraries.
library(stringr)
library(dplyr)
library(xtable)
library(sqldf)
library(tm)
library(RWeka)


####Load/Sample Document Files: stop words, blog, news, and twitter.
conshinyStopWords<- file("shinyStopWords.txt", "r")
shinyStopWords <- readLines(conshinyStopWords)
close(conshinyStopWords)

conBlogs <- file("en_US.blogs.txt", "r") 
dataBlogs <- readLines(conBlogs)
sampleBlogs <- sample(dataBlogs,10000)
close(conBlogs)
rm(dataBlogs)

conNews <- file("en_US.news.txt", "r")
dataNews <- readLines(conNews)
sampleNews <- sample(dataNews,10000)
close(conNews)
rm(dataNews)

conTwitter <- file("en_US.twitter.txt", "r")
dataTwitter <- readLines(conTwitter)
sampleTwitter <- sample(dataTwitter,10000)
close(conTwitter)
rm(dataTwitter)

sample <- c(sampleBlogs,sampleNews,sampleTwitter)
rm(sampleBlogs)
rm(sampleNews)
rm(sampleTwitter)


####Create Regular Expressions for Text Clean-up.
apos <- "[\\']|[’]"
extraChars <- "[:digit:]|[:punct:]"
extraWhite <- "([ ]{2,})"
singleLetters <- "( [a-z] )"


####Clean and Format the Text.
sample <- str_replace_all(sample, apos,"")
sample <- str_replace_all(sample, extraChars," ")
sample <- subset(sample,nchar(str_replace_all(sample, "([a-zA-Z]|[ ])","") ) < 1)
sample <- str_to_lower(sample)
sample <- paste(" ", sample)
sample <- paste(sample," ")
sample <- str_replace_all(sample, paste(shinyStopWords, collapse = " | ")," ")
sample <- str_replace_all(sample, paste(shinyStopWords, collapse = " | ")," ")
sample <- str_replace_all(sample, paste(shinyStopWords, collapse = " | ")," ")
sample <- str_replace_all(sample, singleLetters," ")
sample <- str_trim(sample, side = "both")
sample <- str_replace_all(sample, extraWhite," ")


####Pick Midrange Length Documents for N-Gram Parsing.
sampleCount <- sapply(strsplit(sample, " "), length)
sample <- sample[sampleCount>4]
sampleCount <- sapply(strsplit(sample, " "), length)
sample <- sample[sampleCount<31]


####Reduce Document Sample to Processing Size.
sample <- sample(sample,10000)


####Inspect Sample.
head(sample)
length(sample)


####Set Tokenizer Parameters.
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
ctrl3 <- list(tokenize = TrigramTokenizer,wordLengths = c(1, 45))


####Processing Sample into Frequency Table
colSample <- VCorpus(VectorSource(sample))
tdmTrigram <- TermDocumentMatrix(colSample, control = ctrl3)
rm(colSample)
tdmTrigramMatrix <- inspect(tdmTrigram)
freqTrigram <- data.frame(Word = rownames(tdmTrigramMatrix), Freq = rowSums(tdmTrigramMatrix))
rownames(freqTrigram) <- NULL
freqTrigram<- arrange(freqTrigram,desc(Freq),Word)
rm(tdmTrigramMatrix)


####Inspect Frequency Table.
head(freqTrigram,10)
#write.table(freqTrigram,file="freqTrigram.txt")


####Sum trigram count files.
aggTrigram <- aggregate (x = freqTrigram$Freq, by = list(freqTrigram$Word), sum)
names(aggTrigram) <- c("Word","Freq")
aggTrigram <- arrange(aggTrigram,desc(Freq),Word)
#write.table(aggTrigram,file="aggTrigram.txt")


####Convert to Trigram Model File Format
aggTrigram$SearchTwo <- word(aggTrigram$Word, -3)
aggTrigram$SearchOne <- word(aggTrigram$Word, -2)
aggTrigram$Prediction <- word(aggTrigram$Word, -1)
aggTrigram$TrigramFreq <- aggTrigram$Freq
aggTrigram$Freq <- NULL
aggTrigram$Word <- NULL


####Inspect and Save the Trigram Model File.
head(aggTrigram,10)
write.table(aggTrigram,file="modelTrigram.txt")


