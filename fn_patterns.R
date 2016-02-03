#######################################################################
#Processing options.

#Stepwise:
by_package <- group_by(cran, package)
pack_sum <- summarize(by_package,
                      count = n(),
                      unique = n_distinct(ip_id),
                      countries = n_distinct(country),
                      avg_bytes = mean(size))
top_countries <- filter(pack_sum, countries > 60)
result1 <- arrange(top_countries, desc(countries), avg_bytes)

#Nested:
result2 <-
  arrange(
    filter(
      summarize(
        group_by(cran,
                 package
        ),
        count = n(),
        unique = n_distinct(ip_id),
        countries = n_distinct(country),
        avg_bytes = mean(size)
      ),
      countries > 60
    ),
    desc(countries),
    avg_bytes
  )

#Chaining
result3 <-
  cran %>%
  group_by(package) %>%
  summarize(count = n(),
            unique = n_distinct(ip_id),
            countries = n_distinct(country),
            avg_bytes = mean(size)
  ) %>%
  filter(countries > 60) %>%
  arrange(desc(countries), avg_bytes)

#######################################################################
#Multi-file processing with row count.

complete <- function(directory, id = 1:332) {
  if (sum(dir("../")==directory)<1) {setwd(directory)}
  filelist <- list.files(path = "./")
  alldata<-do.call("rbind", lapply(filelist, read.csv, header = TRUE)) 
  completedata <- alldata[complete.cases(alldata), ]
  aggdata = data.frame()
  for (i in id){
    subdata <- subset(completedata, completedata$ID == i)  
    nobs <- nrow(subdata)
    aggdata <- rbind(aggdata, data.frame(i,nobs))
  }
  names(aggdata)<-c("id","nobs")
  aggdata
}

#######################################################################
#Correlation between columns many files.

corr <- function(directory, threshold = 0) {
  filecounts <- complete(directory)
  thresholdlist <- subset(filecounts$id, filecounts$nobs >= threshold )
  if (sum(dir("../")==directory)<1) {setwd(directory)}
  filelist <- list.files(path = "./")
  alldata<-do.call("rbind", lapply(filelist, read.csv, header = TRUE)) 
  completedata <- alldata[complete.cases(alldata), ]
  correlationvector <- vector()
  for (i in thresholdlist){
    subthresholddata <- subset(completedata, completedata$ID == i)
    correlationvalue <- cor(subthresholddata$nitrate, subthresholddata$sulfate)
    correlationvector <- c(correlationvector, as.numeric(correlationvalue))
    i <- i + 1
  }
  options(digits=4)
  correlationvector
}

#######################################################################
#Data means many files.

pollutantmean <- function(directory, pollutant, id = 1:332){
  if (sum(dir("../")==directory)<1) {setwd(directory)}
  filelist <- list.files(path = "./")
  alldata<-do.call("rbind", lapply(filelist, read.csv, header = TRUE)) 
  pollutantdata <-NA
  for (i in id){
    subsetdata<-subset(alldata,alldata$ID==i)
    addtodata <-NA
    if (pollutant=="nitrate") {addtodata<-subsetdata$nitrate}
    else if (pollutant=="sulfate") {addtodata<-subsetdata$sulfate}
    else {addtodata<-NA}
    pollutantdata<-append(pollutantdata, addtodata)
    i<-i+1
  }
  mean(pollutantdata,na.rm=TRUE)
}

#######################################################################
#Ranking of outcomes.

rankhospital <- function(state, outcome, rank = "best") {
  rawData<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  mortalityData <- rawData[,c(2,7,11,17,23)]
  if (!(state %in% mortalityData$State)) {stop("invalid state")} 
  if (!(outcome %in% c("heart attack", "heart failure", "pneumonia")))
  {stop("invalid outcome")}
  if (outcome == "heart attack") {workData <- mortalityData[,c(1,2,3)]}
  if (outcome == "heart failure") {workData <- mortalityData[,c(1,2,4)]}
  if (outcome == "pneumonia") {workData<- mortalityData[,c(1,2,5)]}
  names(workData) <- c("Hospital","State","Outcome")
  stateData1 <- workData[workData$State == state,]
  stateData2 <- stateData1[!(stateData1$Outcome == "Not Available"),]
  stateData2[,3] <- as.numeric(stateData2[,3])
  stateData2[order(stateData2$Outcome,stateData2$Hospital),] -> finalData
  nrow(finalData) -> x
  if (rank  == "best") {rank <- "1"}
  if (rank  == "worst") {rank <- as.character(x)}
  if (as.numeric(rank) > as.numeric(x)) {NA} -> hospital
  if (as.numeric(rank) <= as.numeric(x)) {finalData[as.numeric(rank),1]} -> hospital
  hospital
}

#######################################################################
#Trigram frequency table.

library(tm)
library(stringr)
library(dplyr)
library(RWeka)
library(sqldf)

#Obtain data
fhandle <- file("sampleCorpus.txt", "r")
sample <- readLines(fhandle)

fhandleStopWords<- file("shinyStopWords.txt", "r")
shinyStopWords <- readLines(conshinyStopWords)

#Clean data.
extraWhite <- "([ ]{2,})"

sample <- paste(" ", sample)
sample <- paste(sample," ")
sample <- str_replace_all(sample, paste(shinyStopWords, collapse = " | ")," ")
sample <- str_trim(sample, side = "both")
sample <- str_replace_all(sample, extraWhite," ")

#Tokenizer configuration.
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
ctrl3 <- list(tokenize = TrigramTokenizer,wordLengths = c(1, 30))

#Text processing into a frequency table.
colSample <- VCorpus(VectorSource(sample))
tdmTrigram <- TermDocumentMatrix(colSample, control = ctrl3)
#tdmTrigramMatrix <- inspect(removeSparseTerms(tdmTrigram, .9999))
tdmTrigramMatrix <- inspect(tdmTrigram)
freqTrigram <- data.frame(Word = rownames(tdmTrigramMatrix), Freq = rowSums(tdmTrigramMatrix))
rownames(freqTrigram) <- NULL
freqTrigram<- arrange(freqTrigram,desc(Freq),Word)
