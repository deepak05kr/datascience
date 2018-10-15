#Data Cleaning and Preparation - Hints

#Identify the data quality issues and clean the data so that you can use it for analysis.
#Ensure that the dates and time are in the proper format. Derive new variables which will be useful for analysis.

uberrequestdata <- read.csv("Uber Request Data.csv",stringsAsFactors = FALSE)

#Total Count of records in the file
totalCountofRecords <- nrow(uberrequestdata)

#Count of records when there was no drop timestamp may be due to Cancellation or No car available
noDroptime <- sum(is.na(uberrequestdata$Drop.timestamp==TRUE))

#Count records when driver was not available
noDriverAvailable <- sum(is.na(uberrequestdata$Driver.id)==TRUE)

#Count of records when cab was cancelled
cancelledTripCount <- noDroptime - noDriverAvailable

#Splitting Request.timestamp into Data and Time and adding 2 new columns in the data
RequestHour <- format(as.POSIXct(strptime(uberrequestdata$Request.timestamp,"%d/%m/%y %H:%M",tz="")) ,format = "%H:%M:%S")
RequestDate <- format(as.POSIXct(strptime(uberrequestdata$Request.timestamp,"%d/%m/%y %H:%M",tz="")) ,format = "%d/%m/%Y")
uberrequestdata$RequestDate <- RequestDate
uberrequestdata$RequestHour <- RequestHour


#Splitting Request.timestamp into Data and Time and adding 2 new columns in the data
DropHour <- format(as.POSIXct(strptime(uberrequestdata$Drop.timestamp,"%d/%m/%y %H:%M",tz="")) ,format = "%H:%M:%S")
DropDate <- format(as.POSIXct(strptime(uberrequestdata$Drop.timestamp,"%d/%m/%y %H:%M",tz="")) ,format = "%d/%m/%Y")
uberrequestdata$DropDate <- DropDate
uberrequestdata$DropHour <- DropHour


# What do you think is the reason for this issue for the supply-demand gap? Write the answer in less than 100 
#words. You may accompany the write-up with plot(s).

#ANS: With Vairous plots and graphs and further visulization of the same through various data available we 
#cound understand that   In the early Morning there is more number of requests are coming from city for Airport. 
#Due to less number of requests from airport in the morning Drivers has to wait for the next ride.
#In the evening there is more number of requests coming from Airport to City but less number of cars are 
#available due to less communtation from City to Airport

#4. Recommend some ways to resolve the supply-demand gap.
#Answer: Uber should do partnership with some other cab servcies if they can fulfill each others requiremnt.
