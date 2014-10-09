plot1 <- function(){

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

  #Retrieve data using read.tables (slow but no warning). 
  #This function is not used here. It is only for testing
  getdata <- function(fpath){
    
    data <- read.table(file=fpath, sep=";", header=TRUE, stringsAsFactors=FALSE)
    data <- dateConversion(data)
    data <- subset (data, data$Date >= '2007-02-01' & data$Date <= '2007-02-02')
    data$Global_active_power <- as.numeric(data$Global_active_power)
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
  #fpath <- "data/household_power_consumption.txt"
  #data <- getdata(fpath)
  data <- getdata2(fpath)
  
  #Print the result to the device
  library(datasets) #histogram library
  png(filename = "plot1.png", width = 480, height = 480)
  hist(data$Global_active_power,  xlab = "Global Active Power (kilowatts)", 
       main="Global Active Power", col="red")
  dev.off()
  
}