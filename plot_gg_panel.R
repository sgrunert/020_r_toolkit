###################################################
#Author: S. Grunert
#Create: 24January2015
#Description:
#The following is for Coursera Course: exdata-010: Project 2
###Plot Question 6
###################################################

##Upload full data sets.
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


##Reduce to target data set for Baltimore City, Maryland and Los Angeles
steponeNEI <- NEI[(NEI$fips == "24510"|
                     NEI$fips == "06037"), ]


##Add in a "city" factor.
steponeNEI$city = ifelse(steponeNEI$fips == "24510","Baltimore City","Los Angeles")


##Reduce to target data set for motor vehicles.
listSCC <- SCC[(SCC$EI.Sector == "Mobile - On-Road Diesel Heavy Duty Vehicles"|
                  SCC$EI.Sector == "Mobile - On-Road Diesel Light Duty Vehicles"|
                  SCC$EI.Sector == "Mobile - On-Road Gasoline Heavy Duty Vehicles"|
                  SCC$EI.Sector == "Mobile - On-Road Gasoline Light Duty Vehicles"), ]

reducedNEI <- steponeNEI[(steponeNEI$SCC%in%listSCC$SCC),]


##Aggregate sums and name columns.
NEIYearSum <- aggregate(reducedNEI$Emissions
                        , by=list(reducedNEI$year,reducedNEI$city)
                        , FUN=sum)
names(NEIYearSum) <- c("Year","City","Emission")


##Make plot png file with ggplot2 library.
library(ggplot2)

qplot(Year,Emission,data = NEIYearSum,facets=.~City
      ,geom = c("point","line")
      , main = "PM2.5 Motor Vehicle Emission: Baltimore City vs Los Angeles County"
      , ylab = "PM2.5 Total Emission")

dev.copy(png, width = 720, height = 480, file = "plotQ6.png")
dev.off(which = 4)
