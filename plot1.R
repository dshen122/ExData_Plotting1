# this function retrieve the data from household_power_consumption file and plot the active power usage between Feburary 
# first and second in 2007.
plot1 <- function(){
    

    #retrieve the raw data from the file

    datRaw <- read.table("household_power_consumption.txt",sep=";", stringsAsFactors = FALSE,header = TRUE, na.strings="?")

    #change the date format
    datRaw$Date <- as.Date(datRaw$Date, format="%d/%m/%Y")

    #filter the data between Feburary 1st and 2nd of 2007 only
    get.rows <- datRaw$Date >= as.Date("2007-02-1") & datRaw$Date <= as.Date("2007-02-2")
    filteredDat <- datRaw[get.rows,]

    # remove rows whose Global_active_power column value is NA
    filteredDat <- filteredDat[complete.cases(filteredDat[,filteredDat$Global_active_power]),]
    
    #plot the histogram of the data
    hist(filteredDat$Global_active_power, col="red", main="Global Active Power", xlab="Global Active Power (kilowatts)", ylab="Frequency")

}
