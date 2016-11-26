## Peer-graded Assignment: Course Project 2
## plot2.png

## Dataset Columns
# fips: A five-digit number (represented as a string) indicating the U.S. county
# SCC: The name of the source as indicated by a digit string (see source code classification table)
# Pollutant: A string indicating the pollutant
# Emissions: Amount of PM2.5 emitted, in tons
# type: The type of source (point, non-point, on-road, or non-road)
# year: The year of emissions recorded

## Question
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") 
# from 1999 to 2008? Use the base plotting system to make a plot answering this question.

rm(list = ls())

## Download the file (should be commented after 1st run if slow bandwidth)
fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(fileUrl,destfile="./exdata_data_NEI_data.zip")
unzip(zipfile="./exdata_data_NEI_data.zip",exdir=".")

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Process the data
baltimore <- subset(NEI, fips=="24510")
spbaltimore <- split(baltimore$Emissions,baltimore$year)
spcbaltimore <- lapply(spbaltimore, sum)

library(plyr)
spNEI1 <- ddply(baltimore,.(year),summarize,emissions=sum(Emissions))

# m1 <- as.matrix(spcbaltimore)
# colnames(m1) <- "emissions"
# barplot(t(m1),xlab = "Year", ylab="PM2.5", main = "Emissions by year in Baltimore") # barplot

## Plot graph
png(file = "plot2.png") ## Open writing device; plot in working directory
par(mar = c(4,4,2,2))

with(spNEI1, plot(year, emissions, type = "l",xlab = "Year", ylab="PM2.5", main = "Emissions by year in Baltimore"))

dev.off() ## Close the file device
