## Peer-graded Assignment: Course Project 2
## plot6.png

## Dataset Columns
# fips: A five-digit number (represented as a string) indicating the U.S. county
# SCC: The name of the source as indicated by a digit string (see source code classification table)
# Pollutant: A string indicating the pollutant
# Emissions: Amount of PM2.5 emitted, in tons
# type: The type of source (point, non-point, on-road, or non-road)
# year: The year of emissions recorded

## Question
# Compare emissions from motor vehicle sources in Baltimore City with emissions 
# from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
# Which city has seen greater changes over time in motor vehicle emissions?

rm(list = ls())

## Download the file (should be commented after 1st run if slow bandwidth)
fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(fileUrl,destfile="./exdata_data_NEI_data.zip")
unzip(zipfile="./exdata_data_NEI_data.zip",exdir=".")

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library("plyr")
intersect(names(NEI),names(SCC)) # should intersect on SCC

## Process the data
## Motor Vehicle sources
motorVehicleIdx <- grepl("[Vv]ehicle",SCC$EI.Sector)
motorVehicleScc <- SCC[motorVehicleIdx,]
m1 <- join_all(list(NEI,motorVehicleScc),type = "right") # filter non-vehicle results (faster)
m1 <- m1[!is.na(m1$Emissions),] # remove NA values
m1 <- subset(m1, fips=="24510" | fips=="06037")
nrow(m1) # 2099

spcNEI1 <- aggregate(m1$Emissions,by=list(m1$year,m1$fips),sum)
colnames(spcNEI1) <- c("year","fips","emissions")
df2 <- transform(spcNEI1, county=ifelse(spcNEI1$fips=="24510","Baltimore","Los.Angeles"))

## Plot graph
library("ggplot2")
png(file = "plot6.png") ## Open writing device; plot in working directory
par(mar = c(4,4,2,2))

## use ggplot instead
g <- ggplot(df2, aes(year,emissions))
g + geom_point() + facet_grid(. ~ county) + geom_smooth(method = "lm") +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "Year", y = expression("PM " * 2.5)) + 
    labs(title = "Motor Vehicle Emissions by Year")

dev.off() ## Close the file device