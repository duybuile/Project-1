plot3 <- function(){
  
  #Load data and return the data path
  loadData <- function(){
    
    temp <- tempfile()
    td = tempdir()
    fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
    download.file(fileURL, temp)
    fname <- unzip(temp, list=TRUE)$Name[1]
    unzip(temp, files=fname, exdir=td, overwrite=TRUE)
    fpath <- file.path(td, fname)
    unlink(temp)
    unlink(td)
    fpath
  }
  
  #Retrieve data using sql (faster)
  getdata2 <- function(fpath){
    
    library(sqldf)
    sql <- "select * from file where Date in ('1/2/2007','2/2/2007')"
    data <- read.csv.sql(file = fpath, sql = sql ,sep=";", header=TRUE)
    data <- dateConversion(data)
    data
  }
  
  #Convert Date and Time
  dateConversion <- function(data){
    
    data$DateTime <- paste(data$Date, data$Time)
    data$Date <- strptime(data$Date, format='%d/%m/%Y')
    data$DateTime <- strptime(data$DateTime, format='%d/%m/%Y %H:%M:%S')
    data
  }
  
  #Load data path and retrieve data
  fpath <- loadData()
  data <- getdata2(fpath)
  
  #Print the result to the device
  library(datasets)
  png(filename = "plot3.png", width = 480, height = 480)
  with(data, plot(x=data$DateTime, y=data$Sub_metering_1, xlab = "",ylab = "Energy sub metering",type="l"))
  with(data, lines(x=data$DateTime, y=data$Sub_metering_2, col = "red"))
  with(data, lines(x=data$DateTime, y=data$Sub_metering_3, col = "blue"))
  legend("topright", pch = 1, col = c("black","blue", "red"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  dev.off()
  
}