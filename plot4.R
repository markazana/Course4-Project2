## Peer-graded Assignment: Course Project 2
## plot4.png

## Dataset Columns
# fips: A five-digit number (represented as a string) indicating the U.S. county
# SCC: The name of the source as indicated by a digit string (see source code classification table)
# Pollutant: A string indicating the pollutant
# Emissions: Amount of PM2.5 emitted, in tons
# type: The type of source (point, non-point, on-road, or non-road)
# year: The year of emissions recorded

## Question
# Across the United States, how have emissions from coal combustion-related sources 
# changed from 1999-2008?

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
coalIdx <- grepl("[Cc]oal",SCC$Short.Name)
coalScc <- SCC[coalIdx,]
m1 <- join_all(list(NEI,coalScc),type = "right") # filter non-coal results (faster)
m1 <- m1[!is.na(m1$Emissions),] # remove NA values
nrow(m1) # 53400
spNEI1 <- ddply(m1,.(year),summarize,emissions=sum(Emissions))

## Plot graph
library("ggplot2")
png(file = "plot4.png") ## Open writing device; plot in working directory
par(mar = c(4,4,2,2))

with(spNEI1, plot(year, emissions, type = "l",xlab = "Year", ylab="PM2.5", main = "Coal Emissions by year"))

dev.off() ## Close the file device