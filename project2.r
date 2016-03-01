
##Exploratory project 2

## Download data
if(!file.exists("./exdata_data_NEI_data.zip")) {
  url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip" 
  destzip<- "./exdata_data_NEI_data.zip" 
  download.file(url, destzip) 
  unzip(destzip, exdir = "./.")
}
dir()

## Load Libraries
library(data.table)
library(dplyr)
library(ggplot2)

## Load data files
NEI <- as.data.table(readRDS("summarySCC_PM25.rds"))
SCC <- as.data.table(readRDS("Source_Classification_Code.rds"))

## Look at data structures and check for missing data
str(NEI)
str(SCC)
table(complete.cases(NEI))

## Question 1: Have total PM2.5 emissions decreased in the US?

TotEm <- NEI %>%
  group_by(year) %>%
  summarise(Em = sum(Emissions/10000))

png("plot1.png", width=480, height=480)
plot(TotEm$year,TotEm$Em,xlab="Year", ylab="Emission in 10000 (tons)",
     main="PM2.5 Emission from 1999-2008 in United States",type="l")

## Question 2: Have total PM2.5 emissions decreased in the Baltimore?

Balt <- NEI %>%
  filter(fips == "24510") %>%
  group_by(year) %>%
  summarise(Em = sum(Emissions))

png("plot2.png", width=480, height=480)
plot(Balt$year,Balt$Em,xlab="Year", ylab="Emission (tons)",
     main="PM2.5 Emission from 1999-2008 in Baltimore",type="l")

## Questions 3: Have total PM2.5 emissions decreased in the Baltimore by type of source?

Balt <- NEI %>%
  filter(fips == "24510") %>%
  group_by(year,type) %>%
  summarise(Em = sum(Emissions))

png("plot3.png", width=480, height=480)
ggplot(Balt, aes(x=year, y=Em, color=type)) + geom_line(size=2) +
         labs(list(title="PM2.5 Emissions in Baltimore City by Source", 
              x="Year", y="Emissions (tons)"))

## Question 4: How have emissions from coal combustion-related sources changed in the US

unique(SCC$EI.Sector) ## Check sources of emissions from coal

## Subset SCC to coal related sources and match with emission table
Coal_Subset <- SCC[grep("Coal",EI.Sector,ignore.case=TRUE), ]
Coal <- NEI[(NEI$SCC %in% Coal_Subset$SCC), ] 

## Emissions from coal related sources by year
Coal_Em <- Coal %>%
  group_by(year) %>%
  summarise(Em = sum(Emissions/10000))

png("plot4.png", width=480, height=480)
plot(Coal_Em$year,Coal_Em$Em,xlab="Year", ylab="Emission in 10000 (tons)",
     main="PM2.5 Emissions from Coal in United States from 1999-2008",type="l")

## Question 5: How have emissions from motor vehicle sources changed in Baltimore City
Baltimore <- filter(NEI, fips=="24510") ##Baltimore emission from all sources

## Subset SCC to motor vehicle sources and match with baltimore emission table
MV <- SCC[grep("Mobile - On-Road",EI.Sector,ignore.case=TRUE), ]
Balt_MV <- Baltimore[(Baltimore$SCC %in% MV$SCC), ] 

## Emissions from motor vehicle sources in Baltimore city from 1999-2008
Balt_MV <- Balt_MV %>%
  group_by(year) %>%
  summarise(Em = sum(Emissions))

png("plot5.png", width=480, height=480)
plot(Balt_MV$year,Balt_MV$Em,xlab="Year", ylab="Emission (tons)",
     main="PM2.5 Emissions from motor vehicle sources \n in Baltimore from 1999-2008",type="l")

## Question 6: Compare emissions from motor vehicle sources in Baltimore City with LA County,California

## Emissions from all sources in LA county
LA_County <- filter(NEI, fips=="06037") ##LA County emission subset

## Subset SCC to motor vehicle sources and match with LA emission table
MV <- SCC[grep("Mobile - On-Road",EI.Sector,ignore.case=TRUE), ]
LA_MV <- LA_County[(LA_County$SCC %in% MV$SCC), ] 

## Emissions from motor vehicle sources in LA County from 1999-2008
LA_MV <- LA_MV %>%
  group_by(year) %>%
  summarise(Em = sum(Emissions))

## Compare emissions from motor vehicle sources in LA County and Baltimore
png("plot6.png", width=480, height=480)
par(mfrow=c(1,2), mar=c(4,4,1.5,1),mgp=c(2,1,0),oma=c(0.5,0.5,2,0.5))
plot(LA_MV$year, LA_MV$Em, xlab="Year", ylab="Emission (tons)", 
     main="Los Angeles County",type="l")
plot(Balt_MV$year, Balt_MV$Em, col="Red", type="l",xlab="Year", ylab="Emission (tons)",
    main="Baltimore City")
mtext("Motor Vehicle PM2.5 Emissions from 1999-2008", outer=TRUE, cex=1)
