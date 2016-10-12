require(dplyr)
require(lubridate)
pClean <- read.table("household_power_consumption.txt", sep = ";", header = TRUE, na.strings = "?", stringsAsFactors = FALSE)
partA <- filter(pClean, Date == "1/2/2007")
partA$datetime <- strptime(partA$Time, "%H:%M:%S")
partA$datetime <- update(partA$datetime, year = 2007, month = 2, day = 1)
partA$datetime <- as.POSIXct(partA$datetime)
partA <- tbl_df(partA)
partB <- filter(pClean, Date == "2/2/2007")
partB$datetime <- strptime(partB$Time, "%H:%M:%S")
partB$datetime <- update(partB$datetime, year = 2007, month = 2, day = 2)
partB$datetime <- as.POSIXct(partB$datetime)
partB <- tbl_df(partB) 
pJoin <- full_join(partA, partB)
png(filename = "plot13.png",
    width = 480, height = 480, units = "px")
plot(Sub_metering_1 ~ datetime, data = pJoin, type = "n", xlab = "", ylab = "Energy sub metering")
lines(Sub_metering_1 ~ datetime, data = pJoin)
lines(Sub_metering_2 ~ datetime, data = pJoin, col = "red")
lines(Sub_metering_3 ~ datetime, data = pJoin, col = "blue")
legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty = c(1,1), col = c("black", "red", "blue"))
dev.off()