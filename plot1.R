## plot1.R
##          This creates plot1.png image file, a plot of:
##              the frequency of Global_active_power
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
plot1 <- function( directory = NA ) {
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
    
    # Start PNG device driver to save output to plot1.png
    png(filename="./plot1.png", height=480, width=480,  bg="white")
    
#     ## adjust margins so that y-axis labels will fit orig<-c(5,4,4,2)
#      par(mar=c(4,4,2,2))
    
    ## create plot
    plot1 <- hist(plot_cons, col="red", cex=0.8, xlab="Global Active Power (kilowatts)", ylab="Frequency", main="Global Active Power", breaks=19, xlim=c(0,6), ylim=c(0,1250), freq=TRUE, axes=FALSE)    
  
    ## set tick marks for x-axis to [0,2,4,6]
    axis(1, las=1, at=c(0,2, 4,6))
    ## set tick marks for y-axis to [0,200,400,600,800,1000,1200] and perpendicular to axis
    axis(2, las=3, at=c(0,200,400,600,800,1000,1200))
    
    # Turn off device driver (to flush output to png)
    dev.off()
    
}
