###################################################
#Author: S. Grunert
#Create: 24January2015
#Description:
#The following is for Coursera Course: exdata-010: Project 2
###Plot Question 5
###################################################

##Upload full data sets.
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


##Reduce to target data set for Baltimore City, Maryland
steponeNEI <- NEI[(NEI$fips == "24510"), ]


##Reduce to target data set for motor vehicles in  Baltimore City, Maryland
listSCC <- SCC[(SCC$EI.Sector == "Mobile - On-Road Diesel Heavy Duty Vehicles"|
                  SCC$EI.Sector == "Mobile - On-Road Diesel Light Duty Vehicles"|
                  SCC$EI.Sector == "Mobile - On-Road Gasoline Heavy Duty Vehicles"|
                  SCC$EI.Sector == "Mobile - On-Road Gasoline Light Duty Vehicles"), ]

reducedNEI <- steponeNEI[(steponeNEI$SCC%in%listSCC$SCC),]


##Aggregate sums and name columns.
NEIYearSum <- aggregate(reducedNEI$Emissions, by=list(Category=reducedNEI$year), FUN=sum)
names(NEIYearSum) <- c("Year","MotorVehicleEmission")


##Make plot png file.
par(lab = c(10, 5, 12))

with(NEIYearSum,plot(NEIYearSum$Year,as.numeric(MotorVehicleEmission)
                     , type = "o"
                     , main = "PM2.5 Motor Vehicle Emission: Baltimore City, Maryland"
                     , ylab = "PM2.5 Total Emission"
                     , xlab = "Year"
))
dev.copy(png, width = 600, height = 480, file = "plotQ5.png")
dev.off(which = 4)