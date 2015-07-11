# this function retrieve the data from household_power_consumption file and four graphs in one panel
plot4 <- function(){
    

    #retrieve the raw data from the file

    datRaw <- read.table("household_power_consumption.txt",sep=";", stringsAsFactors = FALSE,header = TRUE, na.strings="?")

    #change the date format
    datRaw$Date <- as.Date(datRaw$Date, format="%d/%m/%Y")

    #filter the data between Feburary 1st and 2nd of 2007 only
    get.rows <- datRaw$Date >= as.Date("2007-02-1") & datRaw$Date <= as.Date("2007-02-2")
    filteredDatOrig <- datRaw[get.rows,]

   
    #set up number of diagrams in the screen
    par(mfcol=c(2,2))
    
    # remove rows whose Global_active_power column value is NA
    filteredDat <- filteredDatOrig[complete.cases(filteredDatOrig[,filteredDatOrig$Global_active_power]),]

    #combine the first and second column to date/time format and save it to the first column 
    filteredDat$Date <- strptime (paste(filteredDat$Date, filteredDat$Time), "%Y-%m-%d %H:%M:%S")
  
    #plot the global power vs time
    plot(filteredDat$Date, filteredDat$Global_active_power, type="n", pch=20, ylab="Global Active Power (killowatts)", xlab="")
    lines(filteredDat$Date, filteredDat$Global_active_power, pch=20)
    
    # remove rows whose submettering columns have null value column value is NA
    filteredDat <- filteredDatOrig[complete.cases(filteredDatOrig[,c(7:9)]),]
    
    #combine the first and second column to date/time format and save it to the first column 
    filteredDat$Date <- strptime (paste(filteredDat$Date, filteredDat$Time), "%Y-%m-%d %H:%M:%S")
    
    #draw three line graphs representing three sub meterings
    plot(filteredDat$Date, filteredDat$Sub_metering_1, type="n", pch=20, ylab="Energy sub metering", xlab="")
    lines(filteredDat$Date, filteredDat$Sub_metering_1,col="black")
    lines(filteredDat$Date,filteredDat$Sub_metering_2,col="red")
    lines(filteredDat$Date,filteredDat$Sub_metering_3, col="blue")
    legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), cex=.7, bty='n',lwd=c(2.5,2.5,2.5),col=c("black", "red","blue"))

    # remove rows whose voltage column value is NA
    filteredDat <- filteredDatOrig[complete.cases(filteredDatOrig[,filteredDatOrig$voltage]),]
    #combine the first and second column to date/time format and save it to the first column 
    filteredDat$Date <- strptime (paste(filteredDat$Date, filteredDat$Time), "%Y-%m-%d %H:%M:%S")
    
    # draw time vs voltage diagram
    plot(filteredDat$Date, filteredDat$Voltage, type="n", pch=20, ylab="Voltage", xlab="datetime")
    lines(filteredDat$Date,filteredDat$Voltage)
    
    # remove rows whose Global_reactive_power column value is NA
    filteredDat <- filteredDatOrig[complete.cases(filteredDatOrig[,filteredDatOrig$Global_reactive_power]),] #combine the first and second column to date/time format and save it to the first column 
   
    #combine the first and second column to date/time format and save it to the first column 
     filteredDat$Date <- strptime (paste(filteredDat$Date, filteredDat$Time), "%Y-%m-%d %H:%M:%S")
    
    
    # draw the reactive power usage vs time
    plot(filteredDat$Date, filteredDat$Global_reactive_power, type="n", pch=20, ylab="Global_reactive_power", xlab="datetime")
    lines(filteredDat$Date,filteredDat$Global_reactive_power)

}
