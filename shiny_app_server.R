###################################################
#Author: S. Grunert
#Created: 19August2015
#Version: 1.1
#Description:
#This is a Shiny App for Coursera Capstone Project.
###################################################

###################################################
####-------------PREPROCESSING-----------------####
###################################################

####Load Libraries.
library(stringr)
library(dplyr)
library(xtable)
library(sqldf)

####Load Data Files.
conshinyStopWords<- file("shinyStopWords.txt", "r")
shinyStopWords <- readLines(conshinyStopWords)
close(conshinyStopWords)

conmodelUnigram<- file("modelUnigram.txt", "r")
modelUnigram <- read.table(conmodelUnigram,stringsAsFactors = FALSE)
close(conmodelUnigram)

conmodelTrigram<- file("modelTrigram.txt", "r")
modelTrigram <- read.table(conmodelTrigram,stringsAsFactors = FALSE)
close(conmodelTrigram)

####Create Regular Expressions.
apos <- "[\\']|[’]"
extraChars <- "[:digit:]|[:punct:]"
extraWhite <- "([ ]{2,})"

####Function for Cleaning the Text.
Preprocessing <- function(phraseEntry) {
  phrase <- str_replace_all(phraseEntry, apos,"")
  phrase <- str_replace_all(phrase, extraChars," ")
  phrase <- subset(phrase,nchar(str_replace_all(phrase, "([a-zA-Z]|[ ])","") ) < 1)
  phrase <- str_to_lower(phrase)
  phrase <- paste(" ", phrase)
  phrase <- paste(phrase," ")
  phrase <- str_replace_all(phrase, paste(shinyStopWords, collapse = " | ")," ")
  phrase <- str_replace_all(phrase, paste(shinyStopWords, collapse = " | ")," ")
  phrase <- str_replace_all(phrase, paste(shinyStopWords, collapse = " | ")," ")
  phrase <- str_trim(phrase, side = "both")
  phrase <- str_replace_all(phrase, extraWhite," ")
  phraseSearch <- paste(paste(word(phrase, -3),word(phrase, -2)),word(phrase, -1))
  phraseSearch
}

####Function for Making a Prediction.
Prediction <- function(phraseSearch) {
  ####Parse the Search Words.
  searchWord1 <- word(phraseSearch, -1)
  searchWord2 <- word(phraseSearch, -2)
  searchWord3 <- word(phraseSearch, -3)
  
  ####Search Trigrams.
  grepMinusOne <- modelTrigram[modelTrigram$SearchOne==searchWord1,]
  grepMinusOne$TrigramFreq <- grepMinusOne$TrigramFreq * 2
  candidates <- grepMinusOne
  grepMinusOne <- modelTrigram[modelTrigram$SearchTwo==searchWord1,]
  grepMinusOne$TrigramFreq <- grepMinusOne$TrigramFreq * 1
  candidates <-rbind(candidates,grepMinusOne)
  grepMinusTwo <- modelTrigram[modelTrigram$SearchTwo==searchWord2,]
  grepMinusTwo$TrigramFreq <- grepMinusTwo$TrigramFreq * 2
  candidates <- rbind(candidates, grepMinusTwo)
  grepMinusTwo <- modelTrigram[modelTrigram$SearchOne==searchWord2,]
  grepMinusTwo$TrigramFreq <- grepMinusTwo$TrigramFreq * 1
  candidates <- rbind(candidates, grepMinusTwo)
  grepMinusThree <- modelTrigram[modelTrigram$SearchOne==searchWord3,]
  grepMinusThree$TrigramFreq <- grepMinusThree$TrigramFreq * 0.4
  candidates <- rbind(candidates, grepMinusThree)
  grepMinusThree <- modelTrigram[modelTrigram$SearchTwo==searchWord3,]
  grepMinusThree$TrigramFreq <- grepMinusThree$TrigramFreq * 0.4
  candidates <- rbind(candidates, grepMinusThree)
  
  ####Aggregate Counts and Compile Finalists without Search Words.
  freqAgg <- aggregate(candidates$TrigramFreq, by = list(candidates$Prediction), sum)
  names(freqAgg) <- c("Prediction","Freq")
  freqAgg <- freqAgg[freqAgg$Prediction!=searchWord1,]
  freqAgg <- freqAgg[freqAgg$Prediction!=searchWord2,]
  freqAgg <- freqAgg[freqAgg$Prediction!=searchWord3,]
  finalists <- freqAgg[freqAgg$Freq == max(freqAgg$Freq),1]
  
  ####Break Ties with Unigrams.
  grepFinalists <- modelUnigram[modelUnigram$Word%in%finalists,]
  grepFinalists <- arrange(grepFinalists,desc(Freq),Word)
  
  ###Display the Prediction.
  grepFinalists[1,1]
}

Finalists <- function(phraseSearch) {
  ####Parse the Search Words.
  searchWord1 <- word(phraseSearch, -1)
  searchWord2 <- word(phraseSearch, -2)
  searchWord3 <- word(phraseSearch, -3)
  
  ####Search Trigrams.
  grepMinusOne <- modelTrigram[modelTrigram$SearchOne==searchWord1,]
  grepMinusOne$TrigramFreq <- grepMinusOne$TrigramFreq * 4
  candidates <- grepMinusOne
  grepMinusOne <- modelTrigram[modelTrigram$SearchTwo==searchWord1,]
  grepMinusOne$TrigramFreq <- grepMinusOne$TrigramFreq * 2
  candidates <-rbind(candidates,grepMinusOne)
  grepMinusTwo <- modelTrigram[modelTrigram$SearchTwo==searchWord2,]
  grepMinusTwo$TrigramFreq <- grepMinusTwo$TrigramFreq * 2
  candidates <- rbind(candidates, grepMinusTwo)
  grepMinusTwo <- modelTrigram[modelTrigram$SearchOne==searchWord2,]
  grepMinusTwo$TrigramFreq <- grepMinusTwo$TrigramFreq * 1
  candidates <- rbind(candidates, grepMinusTwo)
  grepMinusThree <- modelTrigram[modelTrigram$SearchOne==searchWord3,]
  grepMinusThree$TrigramFreq <- grepMinusThree$TrigramFreq * 0.5
  candidates <- rbind(candidates, grepMinusThree)
  grepMinusThree <- modelTrigram[modelTrigram$SearchTwo==searchWord3,]
  grepMinusThree$TrigramFreq <- grepMinusThree$TrigramFreq * 0.5
  candidates <- rbind(candidates, grepMinusThree)
  
  ####Aggregate Counts and Compile Finalists without Search Words.
  freqAgg <- aggregate(candidates$TrigramFreq, by = list(candidates$Prediction), sum)
  names(freqAgg) <- c("Word","TrigramFreq")
  freqAgg <- freqAgg[freqAgg$Word!=searchWord1,]
  freqAgg <- freqAgg[freqAgg$Word!=searchWord2,]
  freqAgg <- freqAgg[freqAgg$Word!=searchWord3,]
  freqAgg <- freqAgg[(freqAgg$TrigramFreq > 0),]
  freqAgg$TrigramFreq <- as.integer(freqAgg$TrigramFreq)
  names(modelUnigram) <- c("Word","UnigramFreq") 
  wordTable <- sqldf(
    "SELECT a.Word, a.TrigramFreq, b.UnigramFreq
  FROM freqAgg a
    INNER JOIN modelUnigram b USING (Word)")
  wordTable <- arrange(wordTable, desc(TrigramFreq), desc(UnigramFreq))
  
  ###Display the Prediction Word List.
  wordTable <- xtable(head(wordTable,5))
  wordTable
}

###################################################
####------------SHINY PROCESSING--------------#####
###################################################


shinyServer(
  function(input, output) {
    output$inputValue <- renderText({Preprocessing(input$stringentry)})
    output$prediction <- renderText({Prediction(Preprocessing(input$stringentry))})
    output$others <- renderTable({Finalists(Preprocessing(input$stringentry))})
  }
)
    

