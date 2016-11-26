## Peer-graded Assignment: Course Project 2
## plot3.png

## Dataset Columns
# fips: A five-digit number (represented as a string) indicating the U.S. county
# SCC: The name of the source as indicated by a digit string (see source code classification table)
# Pollutant: A string indicating the pollutant
# Emissions: Amount of PM2.5 emitted, in tons
# type: The type of source (point, non-point, on-road, or non-road)
# year: The year of emissions recorded

## Question
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) 
# variable, which of these four sources have seen decreases in emissions from 1999-2008 
# for Baltimore City? Which have seen increases in emissions from 1999-2008? 
# Use the ggplot2 plotting system to make a plot answer this question.

rm(list = ls())

## Download the file (should be commented after 1st run if slow bandwidth)
fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(fileUrl,destfile="./exdata_data_NEI_data.zip")
unzip(zipfile="./exdata_data_NEI_data.zip",exdir=".")

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) 
# variable, which of these four sources have seen decreases in emissions from 1999-2008 
# for Baltimore City? Which have seen increases in emissions from 1999-2008? 
# Use the ggplot2 plotting system to make a plot answer this question.

## Process the data
# http://stackoverflow.com/questions/8942670/split-dataframe-using-two-columns-of-data-and-apply-common-transformation-on-lis
baltimore <- subset(NEI, fips=="24510")
df2 <- aggregate(baltimore$Emissions, by=list(baltimore$type,baltimore$year),sum)
colnames(df2) <- c("type","year","emissions")

## Plot graph
library("ggplot2")
png(file = "plot3.png") ## Open writing device; plot in working directory
par(mar = c(4,4,2,2))

## Only POINT type is generally trending upwards
qplot(year, emissions, color = type, data=df2) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Year", y = expression("PM " * 2.5)) + 
  labs(title = "Baltimore Air Quality")

dev.off() ## Close the file device