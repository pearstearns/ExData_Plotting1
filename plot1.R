#Our Requirements for this script
require(dplyr, lubridate)

#Downloading and Unzipping, and Reading the file for Maximum Reproducability
file <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download.file(file, destfile = "pcomp.zip")
unzip("pcomp.zip")
pClean <- read.table("household_power_consumption.txt", sep = ";", header = TRUE, na.strings = "?", stringsAsFactors = FALSE)

#Filters for one of our days, concatenates the 'Date' and 'Time' columns into one called 'datetime',
#makes 'datetime' become a date object instead of a character, and turns the subset into a tbl 
partA <- filter(pClean, Date == "1/2/2007")
partA$datetime <- strptime(partA$Time, "%H:%M:%S")
partA$datetime <- update(partA$datetime, year = 2007, month = 2, day = 1)
partA$datetime <- as.POSIXct(partA$datetime)
partA <- tbl_df(partA)

#Second verse, same as the first!
partB <- filter(pClean, Date == "2/2/2007")
partB$datetime <- strptime(partB$Time, "%H:%M:%S")
partB$datetime <- update(partB$datetime, year = 2007, month = 2, day = 2)
partB$datetime <- as.POSIXct(partB$datetime)
partB <- tbl_df(partB) 

#Joining the two subsets into a whole
pJoin <- full_join(partA, partB)

#Makes our graph
png(filename = "plot1.png",
    width = 480, height = 480, units = "px")
hist(pJoin$Global_active_power, col = "Red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)")
dev.off()
