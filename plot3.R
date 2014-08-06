## plot3.R
##          This creates plot3.png image file, a plot of:
##              Energy sub metering
##          mapped over two days usage.
##
##          The data, from which it was constucted, is contained within the downloaded zip file.
##
## Note:
##          "household_power_consumption.zip" is downloaded to "Project 1" directory
##              created from the current working directory or passed-parameter-string. 
##
##          The date and time of the download is reported to stdout
##
##          The download fileURL works on Windows Vista OS.
##
##          For the MAC OS use: 
##              fileURL = https://d396qusza40orc.cloudfront.net/getdata/projectfiles/UCI%20HAR%20Dataset.zip"
##              download.file(fileURL, "household_power_consumption.zip", method=curl)
##
plot3 <- function( directory = NA ) {
    ##  directory       IN:     char vector 
    ##                  DESC:   start directory
    
    if ( !is.na( directory )) {
        mainDir <- directory
    }
    else {
        mainDir <- getwd()
    }
    
    newProjectDir <- "./Project 1"
    if( !file.exists( newProjectDir )) {
        dir.create( file.path( mainDir, newProjectDir ))
    }
    setwd( file.path( mainDir, newProjectDir ))
    
    zipFileName <- "household_power_consumption.zip"
    if( !file.exists( zipFileName )) {
        fileURL = "http://d396qusza40orc.cloudfront.net/exdata/data/household_power_consumption.zip"
        download.file( fileURL, zipFileName )
        
        # RECORD TIME dataset downloaded
        dateDownloaded <- date()
        write("downloaded zipfile: household_power_consumption.zip", "")
        write(date(), "")
        
        # unzip file
        unzip( zipFileName )
    }
    
    ## load library
    library(sqldf)
    
    filename <-"household_power_consumption.txt"
    
    ## date format in file: d/m/yyyy for single digit months and days
    ## read file and select rows based on strings "1/2/2007" and "2/2/2007" 
    household_power_consump1 <- read.csv.sql(filename, sep=";",sql = 'select * from file where Date = "1/2/2007"')
    household_power_consump2 <- read.csv.sql(filename, sep=";",sql = 'select * from file where Date = "2/2/2007"')
    power_cons <- rbind(household_power_consump1, household_power_consump2)
        
    # Start PNG device driver to save output to plot3.png
    png(filename="./plot3.png", height=480, width=480,  bg="white")
    
    ## isolate sub_metering columns
    meter_cols1 <- power_cons[ , "Sub_metering_1"]
    meter_cols2 <- power_cons[ , "Sub_metering_2"]
    meter_cols3 <- power_cons[ , "Sub_metering_3"]

    # Calculate range from 0 to max value of cars and trucks
    g_range <- range(0, meter_cols1, meter_cols2, meter_cols3)
    max_x <- length( meter_cols1)
    
    ## create plot
    plot3 <- plot(meter_cols1, type="l", col="black", xlab="", ylab="Energy sub metering", main="", ylim=c(0,g_range[2]), axes=FALSE) 

    # Graph meter_cols2 with red line
    lines(meter_cols2, type="l", lty=1, col="red")
    
    # Graph meter_cols3 with blue line
    lines(meter_cols3, type="l", lty=1, col="blue")
    
    # Make x axis using Thu,Fri,Sat labels
    axis(1, at=c(1, max_x/2, max_x), lab=c("Thu", "Fri", "Sat"))
    
    # Make y axis with perpendicular labels that display ticks at 
    # every 10 marks. 10*0:g_range[2] is equivalent to c(0,10,20,30). 
    axis(2, las=3, at=10*0:g_range[2])    
    
    legend("topright", cex=.8, lty=1, col=c("black", "red", "blue"), legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
    
    # Create box around plot
    box()
    
    # Turn off device driver (to flush output to png)
    dev.off()
}
    