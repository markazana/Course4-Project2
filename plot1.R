## Peer-graded Assignment: Course Project 2
## plot1.png

## Dataset Columns
# fips: A five-digit number (represented as a string) indicating the U.S. county
# SCC: The name of the source as indicated by a digit string (see source code classification table)
# Pollutant: A string indicating the pollutant
# Emissions: Amount of PM2.5 emitted, in tons
# type: The type of source (point, non-point, on-road, or non-road)
# year: The year of emissions recorded

## Question
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
# Using the base plotting system, make a plot showing the total PM2.5 emission from 
# all sources for each of the years 1999, 2002, 2005, and 2008.

rm(list = ls())

## Download the file (should be commented after 1st run if slow bandwidth)
fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(fileUrl,destfile="./exdata_data_NEI_data.zip")
unzip(zipfile="./exdata_data_NEI_data.zip",exdir=".")

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Process the data
spNEI <- split(NEI$Emissions,NEI$year)
spcNEI <- lapply(spNEI,sum)
m1 <- as.matrix(spcNEI)
colnames(m1) <- "emissions"

## Plot graph
png(file = "plot1.png") ## Open writing device; plot in working directory
par(mar = c(4,4,2,2))

barplot(t(m1),xlab = "Year", ylab="PM2.5", main = "Emissions by year") # barplot

#library(plyr)
#spNEI1 <- ddply(NEI,.(year),summarize,emissions=sum(Emissions))
#with(spNEI1, plot(year, emissions, type = "l",xlab = "Year", ylab="PM2.5", main = "Emissions by year"))

dev.off() ## Close the file device