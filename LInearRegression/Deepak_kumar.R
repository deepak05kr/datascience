library(plyr)
library(stringr)
library(car)
library(MASS)

carPriceInput=read.csv('CarPrice_Assignment.csv')
summary(carPriceInput)
str(carPriceInput)
#checking if data contains null values or duplicate rows
nullValueCount <- sum(is.na(carPriceInput))
nullValueCount
duplicateRowCount <- sum(duplicated(carPriceInput))
duplicateRowCount
#spitted the car name and model
carPriceInput$car_name <- str_split(carPriceInput$CarName, " ", simplify = TRUE)[,1]
carPriceInput$car_model <- str_split(carPriceInput$CarName, " ", simplify = TRUE)[,2]

carPriceInput$car_name=gsub("toyouta","toyota",carPriceInput$car_name)
carPriceInput$car_name=gsub("porcshce","porsche",carPriceInput$car_name)
carPriceInput$car_name=gsub("maxda","mazda",carPriceInput$car_name)
carPriceInput$car_name=gsub("vokswagen","volkswagen",carPriceInput$car_name)

#removing unnecessary columns
carPriceInput <- carPriceInput[,-1]
carPriceInput <- carPriceInput[,-2]

#0 for gas and 1 for diseal 
str(carPriceInput$fueltype)
summary(factor(carPriceInput$fueltype))
levels(carPriceInput$fueltype)<-c(1,0)
carPriceInput$fueltype<- as.numeric(levels(carPriceInput$fueltype))[carPriceInput$fueltype]
#0 for turbo 1 for std
str(carPriceInput$aspiration)
summary(factor(carPriceInput$fueltype))
levels(carPriceInput$aspiration)<-c(1,0)
carPriceInput$aspiration<- as.numeric(levels(carPriceInput$aspiration))[carPriceInput$aspiration]
#1 for four 0 for two
str(carPriceInput$doornumber)
summary(factor(carPriceInput$doornumber))
levels(carPriceInput$doornumber)<-c(1,0)
carPriceInput$doornumber<- as.numeric(levels(carPriceInput$doornumber))[carPriceInput$doornumber]
#1 for front 0 for rear
str(carPriceInput$enginelocation)
summary(factor(carPriceInput$enginelocation))
levels(carPriceInput$enginelocation)<-c(1,0)
carPriceInput$enginelocation<- as.numeric(levels(carPriceInput$enginelocation))[carPriceInput$enginelocation]

set.seed(100)

#training Data and test data creation
trainingData= sample(1:nrow(carPriceInput), 0.7*nrow(carPriceInput))

train = carPriceInput[trainingData,]

test = carPriceInput[-trainingData,]

#remove insignificant variables and model creation
model0 <- lm(price~.,data=carPriceInput)
summary(model0)
step <- stepAIC(model,direction = "both")



model1=lm(price ~ symboling + aspiration + doornumber + carbody + drivewheel + 
            wheelbase + carlength + carwidth + curbweight + enginetype + 
            cylindernumber + fuelsystem + boreratio + stroke + compressionratio + 
            horsepower + peakrpm + highwaympg + car_name + car_model,data=train)
summary(model1)
#will remove symboling Adjusted R-squared:  0.9789 
carPriceInput <- carPriceInput[ , !(names(carPriceInput) %in% c("symboling"))]


model2=lm(price ~  aspiration + doornumber + carbody + drivewheel + 
            wheelbase + carlength + carwidth + curbweight + enginetype + 
            cylindernumber + fuelsystem + boreratio + stroke+ compressionratio + 
            horsepower + peakrpm + highwaympg + car_name + car_model,data=train)
summary(model2)
#will remove stroke Adjusted R-squared:  0.979 
carPriceInput <- carPriceInput[ , !(names(carPriceInput) %in% c("stroke"))]

model3=lm(price ~  aspiration + doornumber + carbody + drivewheel + 
            wheelbase + carlength + carwidth + curbweight + enginetype + 
            cylindernumber + fuelsystem + boreratio + compressionratio + 
            horsepower + peakrpm + highwaympg + car_name + car_model,data=train)
summary(model3)
#will remove doornumber Adjusted R-squared:   0.9769
carPriceInput <- carPriceInput[ , !(names(carPriceInput) %in% c("doornumber"))]

model4=lm(price ~  aspiration + carbody + drivewheel + 
            wheelbase + carlength + carwidth + curbweight + enginetype + 
            cylindernumber + fuelsystem + boreratio + compressionratio + 
            horsepower + peakrpm + highwaympg + car_name + car_model,data=train)
summary(model4)
#will remove carlength  Adjusted R-squared:   0.9776
carPriceInput <- carPriceInput[ , !(names(carPriceInput) %in% c("carlength"))]

model5=lm(price ~  aspiration + carbody + drivewheel + 
            wheelbase  + carwidth + curbweight + enginetype + 
            cylindernumber + fuelsystem + boreratio + compressionratio + 
            horsepower + peakrpm + highwaympg + car_name + car_model,data=train)
summary(model5)
#will remove  carbody Adjusted R-squared:   0.9783
carPriceInput <- carPriceInput[ , !(names(carPriceInput) %in% c("carbody"))]

model6=lm(price ~  aspiration  + drivewheel + 
            wheelbase  + carwidth + curbweight + enginetype + 
            cylindernumber + fuelsystem + boreratio + compressionratio + 
            horsepower + peakrpm + highwaympg + car_name + car_model,data=train)
summary(model6)
#will remove  carbodyhatchback Adjusted R-squared:   0.9783
carPriceInput <- carPriceInput[ , !(names(carPriceInput) %in% c("carbodyhatchback"))]

model7=lm(price ~  aspiration + drivewheel + 
            wheelbase  + carwidth + curbweight + enginetype + 
            cylindernumber + fuelsystem + boreratio + compressionratio + 
            horsepower + peakrpm + highwaympg + car_name + car_model,data=train)
summary(model7)
#will remove  highwaympg Adjusted R-squared:   0.9783
carPriceInput <- carPriceInput[ , !(names(carPriceInput) %in% c("highwaympg"))]


model8=lm(price ~  aspiration  + drivewheel + 
            wheelbase  + carwidth + curbweight + enginetype + 
            cylindernumber + fuelsystem + boreratio + compressionratio + 
            horsepower + peakrpm +  car_name + car_model,data=train)
summary(model8)
#will remove  compressionratio Adjusted R-squared:   0.9746
carPriceInput <- carPriceInput[ , !(names(carPriceInput) %in% c("compressionratio"))]


model9=lm(price ~  aspiration  + drivewheel + 
            wheelbase  + carwidth + curbweight + enginetype + 
            cylindernumber + fuelsystem + boreratio +  
            horsepower + peakrpm +  car_name + car_model,data=train)
summary(model9)
#will remove car_model  Adjusted R-squared:   0.9754
carPriceInput <- carPriceInput[ , !(names(carPriceInput) %in% c("car_model"))]

model10=lm(price ~  aspiration  + drivewheel + 
            wheelbase  + carwidth + curbweight + enginetype + 
            cylindernumber + fuelsystem + boreratio +  
            horsepower + peakrpm +  car_name ,data=train)
summary(model10)
#will remove car_model  Adjusted R-squared:   0.9657
carPriceInput <- carPriceInput[ , !(names(carPriceInput) %in% c("car_model"))]

model11=lm(price ~  aspiration  + drivewheel + 
             wheelbase  + carwidth + curbweight + enginetype + 
             cylindernumber + fuelsystem + boreratio +  
             horsepower + peakrpm +  car_name ,data=train)
summary(model11)
#will remove  fuelsystem Adjusted R-squared:   0.9657
carPriceInput <- carPriceInput[ , !(names(carPriceInput) %in% c("fuelsystem"))]

model12=lm(price ~  aspiration  + drivewheel + 
             wheelbase  + carwidth + curbweight + enginetype + 
             cylindernumber  + boreratio +  
             horsepower + peakrpm +  car_name ,data=train)
summary(model12)
#will remove horsepower  Adjusted R-squared:   0.967
carPriceInput <- carPriceInput[ , !(names(carPriceInput) %in% c("horsepower"))]

model13=lm(price ~  aspiration  + drivewheel + 
             wheelbase  + carwidth + curbweight + enginetype + 
             cylindernumber  + boreratio +  
             horsepower + peakrpm +  car_name ,data=train)
summary(model13)
#will remove peakrpm  Adjusted R-squared:   0.967
carPriceInput <- carPriceInput[ , !(names(carPriceInput) %in% c("peakrpm"))]


model14=lm(price ~  aspiration  + drivewheel + 
             wheelbase  + carwidth + curbweight + enginetype + 
             cylindernumber  + boreratio +  
             horsepower + car_name ,data=train)
summary(model14)
#will remove drivewheel  Adjusted R-squared:   0.9673
carPriceInput <- carPriceInput[ , !(names(carPriceInput) %in% c("drivewheel"))]


model15=lm(price ~  aspiration  + 
             wheelbase  + carwidth + curbweight + enginetype + 
             cylindernumber  + boreratio +  
             horsepower +  car_name ,data=train)
summary(model15)
#will remove wheelbase  Adjusted R-squared:   0.9676
carPriceInput <- carPriceInput[ , !(names(carPriceInput) %in% c("wheelbase"))]

model16=lm(price ~  aspiration  + carwidth + curbweight + enginetype + 
             cylindernumber  + boreratio +  
             horsepower +  car_name ,data=train)
summary(model16)
#will remove boreratio  Adjusted R-squared:   0.9675
carPriceInput <- carPriceInput[ , !(names(carPriceInput) %in% c("boreratio"))]


model17=lm(price ~  aspiration  + carwidth + curbweight + enginetype + 
             cylindernumber  + horsepower +  car_name ,data=train)
summary(model17)
#will remove horsepower  Adjusted R-squared:   0.9677
carPriceInput <- carPriceInput[ , !(names(carPriceInput) %in% c("horsepower"))]


model17=lm(price ~  aspiration  + carwidth + curbweight + enginetype + 
             cylindernumber +  car_name ,data=train)
summary(model17)
#will remove   Adjusted R-squared:   0.9677
carPriceInput <- carPriceInput[ , !(names(carPriceInput) %in% c(""))]

#There are 6 variable which is more effective in pricing of the car  
#(aspiration,carwidth,curbweight,enginetype,cylindernumber,  car_name)


