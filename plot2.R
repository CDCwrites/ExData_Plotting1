## plot2.R
##          This creates plot2.png image file, a plot of:
##              Global_active_power
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
plot2 <- function( directory = NA ) {
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
    
    # get Global_active_power values
    plot_cons <- as.numeric(power_cons[, "Global_active_power"])
    max_y = max(plot_cons)
    max_x = length(plot_cons)
    
    # Start PNG device driver to save output to plot2.png
    png(filename="./plot2.png", height=480, width=480,  bg="white")
        
    ## create plot
    plot2 <- plot(plot_cons, type="l", col="black", xlab="", ylab="Global Active Power (kilowatts)", main="", ylim=c(0,max_y), xlim=c(0,max_x), freq=FALSE, axes=FALSE) 

    # Make x axis using Thu,Fri,Sat labels
    axis(1, at=c(1, max_x/2, max_x), lab=c("Thu", "Fri", "Sat"))
    
    # Make y axis with perpendicular labels that display ticks at 
    # every 4 marks. 2*0:max_y-2 is equivalent to c(0,2,4,6). 
    axis(2, las=3, at=2*0:max_y-2)
    
    # Create box around plot
    box()
    
    # Turn off device driver (to flush output to png)
    dev.off()
    
}