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
spbaltimore <- split(baltimore$Emissions,list(baltimore$type,baltimore$year))
spcbaltimore <- lapply(spbaltimore, sum)
str(spcbaltimore)
pm25.nonroad <- grep("^NON-ROAD.",names(spcbaltimore))
pm25.nonpoint <- grep("^NONPOINT.",names(spcbaltimore))
pm25.onroad <- grep("^ON-ROAD.",names(spcbaltimore))
pm25.point <- grep("^POINT.",names(spcbaltimore))

xlab1 <- tolower(c("YEAR","NON-ROAD","NONPOINT","ON-ROAD","POINT"))
xlab2 <- make.names(xlab1)
xlab3 <- c("year","type","pm2.5")
years <- c(1999,2002,2005,2008)

## build a data frame for plot system
m2 <- matrix(nrow=length(spcbaltimore),ncol=3,dimnames = list(1:length(spcbaltimore),xlab3))
df2 <- as.data.frame(m2,stringsAsFactors = FALSE)
for(j in seq_along(years)) {
  ## NON-ROAD
  df2[j,xlab3[1]] <- years[j]
  df2[j,xlab3[2]] <- xlab2[2]
  df2[j,xlab3[3]] <- spcbaltimore[[pm25.nonroad[j]]]
  
  ## NONPOINT
  df2[j+4,xlab3[1]] <- years[j]
  df2[j+4,xlab3[2]] <- xlab2[3]
  df2[j+4,xlab3[3]] <- spcbaltimore[[pm25.nonpoint[j]]]
  
  ## ON-ROAD
  df2[j+8,xlab3[1]] <- years[j]
  df2[j+8,xlab3[2]] <- xlab2[4]
  df2[j+8,xlab3[3]] <- spcbaltimore[[pm25.onroad[j]]]
  
  ## POINT
  df2[j+12,xlab3[1]] <- years[j]
  df2[j+12,xlab3[2]] <- xlab2[5]
  df2[j+12,xlab3[3]] <- spcbaltimore[[pm25.point[j]]]
}
# str(df2)

## Plot graph
library("ggplot2")
png(file = "plot3.png") ## Open writing device; plot in working directory
par(mar = c(4,4,2,2))

## Only POINT type is generally trending upwards
qplot(year, pm2.5, color = type, data=df2) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Year", y = expression("PM " * 2.5)) + 
  labs(title = "Baltimore Air Quality")

dev.off() ## Close the file device