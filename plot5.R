## Peer-graded Assignment: Course Project 2
## plot5.png

## Dataset Columns
# fips: A five-digit number (represented as a string) indicating the U.S. county
# SCC: The name of the source as indicated by a digit string (see source code classification table)
# Pollutant: A string indicating the pollutant
# Emissions: Amount of PM2.5 emitted, in tons
# type: The type of source (point, non-point, on-road, or non-road)
# year: The year of emissions recorded

## Question
# How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

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
motorVehicleIdx <- grepl("[Mm]otor [Vv]ehicle",SCC$Short.Name)
motorVehicleScc <- SCC[motorVehicleIdx,]
m1 <- join_all(list(NEI,motorVehicleScc),type = "right") # filter non-vehicle results (faster)
m1 <- m1[!is.na(m1$Emissions),] # remove NA values
nrow(m1) # 2073

## In Baltimore
baltimore <- subset(m1, fips=="24510")
spbaltimore <- split(baltimore$Emissions,baltimore$year)
spcbaltimore <- lapply(spbaltimore, sum)
str(spcbaltimore)

xlab3 <- c("year","emissions")
years <- c(1999,2002,2005,2008)

m2 <- matrix(nrow=length(spcbaltimore),ncol=2,dimnames = list(1:length(spcbaltimore),xlab3))
df2 <- as.data.frame(m2,stringsAsFactors = FALSE)

for(j in seq_along(years)) {
  key <- years[j]
  value <- spcbaltimore[[as.character(key)]]
  df2[j,1] = years[j]
  df2[j,2] = ifelse(length(value)==0,0,value)
}

## Plot graph
png(file = "plot5.png") ## Open writing device; plot in working directory
par(mar = c(4,4,2,2))

with(df2, plot(year, emissions, type = "l",
    xlab = "Year", ylab=expression("PM " * 2.5), main = "Polution due to Vehicles in Baltimore"))

dev.off() ## Close the file device