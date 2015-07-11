# this function retrieve the data from household_power_consumption file and of three sub meterings in three different colors
plot3 <- function(){
    

    #retrieve the raw data from the file

    datRaw <- read.table("household_power_consumption.txt",sep=";", stringsAsFactors = FALSE,header = TRUE, na.strings="?")

    #change the date format
    datRaw$Date <- as.Date(datRaw$Date, format="%d/%m/%Y")

    #filter the data between Feburary 1st and 2nd of 2007 only
    get.rows <- datRaw$Date >= as.Date("2007-02-1") & datRaw$Date <= as.Date("2007-02-2")
    filteredDat <- datRaw[get.rows,]

    # remove rows whose Global_active_power column value is NA
    filteredDat <- filteredDat[complete.cases(filteredDat[,c(7:9)]),]
    
    #combine the first and second column to date/time format and save it to the first column 
    filteredDat$Date <- strptime (paste(filteredDat$Date, filteredDat$Time), "%Y-%m-%d %H:%M:%S")

    #plot three lines
    plot(filteredDat$Date, filteredDat$Sub_metering_1, type="n", pch=20, ylab="Energy sub metering", xlab="")
    lines(filteredDat$Date, filteredDat$Sub_metering_1,col="black")
    lines(filteredDat$Date,filteredDat$Sub_metering_2,col="red")
    lines(filteredDat$Date,filteredDat$Sub_metering_3, col="blue")
    
    #add legend at top right corner
    legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=c(1,1,1), lwd=c(2.5,2.5,2.5),col=c("black", "red","blue"))

}
