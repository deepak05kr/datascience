

###############################################################################################
#### Koyck Model : Camera Accessory 
###############################################################################################
###############################################################################################

library(AER)
library(lmtest)
library(dLagM)
library(dynlm)
library(wavethresh)
library(MASS)
library(AER)
library(car)
library(lmtest)
library(sandwich)
library(survival)
library(Hmisc)
library(lattice)
library(Formula)
library( ggplot2)
library(Hmisc)

Camera_Accessory_data <- capstonewithoutna_withinperiod_Filter_grpbyOdate_media_CA 


# we already have got attributes from linear model which is userful so We are using attributes from linear model for further analysis

# weeknumber + units + discount +  Percent +  CameraBattery +    CameraTripod + Flash +  TV + Digital + Sponsorship 

trainindices <- sample(1:nrow(Camera_Accessory_data),0.6*nrow(Camera_Accessory_data))
#training data set
train = Camera_Accessory_data[trainindices,]
# testing data set
test = Camera_Accessory_data[-trainindices,]

model.koyck1 = koyckDlm(x = Camera_Accessory_data$weeknumber, y = Camera_Accessory_data$gmv)
summary(model.koyck1,diagnostics=TRUE) 
checkresiduals(model.koyck1$model, test = F)

forcastedVal1 <-dLagM::forecast(model = model.koyck1  , x = c(1:21), h = 21 , interval = FALSE,level = 0.95 ,nSim = 100)
predictedVal <- forcastedVal1$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))


model.koyck3 = koyckDlm(x = Camera_Accessory_data$units, y = Camera_Accessory_data$gmv)
summary(model.koyck3,diagnostics=TRUE) 
checkresiduals(model.koyck3$model, test = F)

forcastedVal3 <-dLagM::forecast(model = model.koyck3  , x = c(1:21), h = 21 , interval = FALSE,level = 0.95 ,nSim = 100)
predictedVal <- forcastedVal3$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))


model.koyck2 = koyckDlm(x = Camera_Accessory_data$CameraBattery, y = Camera_Accessory_data$gmv)
summary(model.koyck2,diagnostics=TRUE) 
checkresiduals(model.koyck2$model, test = F)

forcastedVal2 <-dLagM::forecast(model = model.koyck2  , x = c(1:21), h = 21 , interval = FALSE,level = 0.95 ,nSim = 100)
predictedVal <- forcastedVal2$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))

model.koyck4 = koyckDlm(x = Camera_Accessory_data$Sponsorship, y = Camera_Accessory_data$gmv)
summary(model.koyck4,diagnostics=TRUE) 
checkresiduals(model.koyck4$model, test = F)

forcastedVal4 <-dLagM::forecast(model = model.koyck4  , x = c(1:21), h = 21 , interval = FALSE,level = 0.95 ,nSim = 100)
predictedVal <- forcastedVal4$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))

model.koyck5 = koyckDlm(x = Camera_Accessory_data$TV, y = Camera_Accessory_data$gmv)
summary(model.koyck5,diagnostics=TRUE) 
checkresiduals(model.koyck4$model, test = F)

forcastedVal5 <-dLagM::forecast(model = model.koyck5  , x = c(1:21), h = 21 , interval = FALSE,level = 0.95 ,nSim = 100)
predictedVal <- forcastedVal5$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))

model.koyck6 = koyckDlm(x = Camera_Accessory_data$Content_Marketing, y = Camera_Accessory_data$gmv)
summary(model.koyck6,diagnostics=TRUE) 
checkresiduals(model.koyck6$model, test = F)

forcastedVal6 <-dLagM::forecast(model = model.koyck6  , x = c(1:21), h = 21 , interval = FALSE,level = 0.95 ,nSim = 100)
predictedVal <- forcastedVal6$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))

model.koyck7 = koyckDlm(x = Camera_Accessory_data$gmv, y = Camera_Accessory_data$Affiliates)
summary(model.koyck7,diagnostics=TRUE) 
checkresiduals(model.koyck7$model, test = F)

forcastedVal7 <-dLagM::forecast(model = model.koyck7  , x = c(1:21), h = 21 , interval = FALSE,level = 0.95 ,nSim = 100)
predictedVal <- forcastedVal7$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))


model.koyck8 = koyckDlm(x = Camera_Accessory_data$gmv, y = Camera_Accessory_data$Digital)
summary(model.koyck8,diagnostics=TRUE) 
checkresiduals(model.koyck8$model, test = F)

forcastedVal8 <-dLagM::forecast(model = model.koyck8  , x = c(1:21), h = 21 , interval = FALSE,level = 0.95 ,nSim = 100)
predictedVal <- forcastedVal8$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))





###############################################################################################
#### Koyck Model : HOME AUDIO 
###############################################################################################
###############################################################################################



Home_Audio_Data <- capstonewithoutna_withinperiod_Filter_grpbyOdate_media_HA


# we already have got attributes from linear model which is userful so We are using attributes from linear model for further analysis

##  units + Affiliates + SEM

trainindices <- sample(1:nrow(Home_Audio_Data),0.6*nrow(Home_Audio_Data))
#training data set
train = Home_Audio_Data[trainindices,]
# testing data set
test = Home_Audio_Data[-trainindices,]

model.koyck1 = koyckDlm(x = Home_Audio_Data$units, y = Home_Audio_Data$gmv)
summary(model.koyck1,diagnostics=TRUE) 
checkresiduals(model.koyck1$model, test = F)

forcastedVal1 <-dLagM::forecast(model = model.koyck1  , x = c(1:20), h = 20 , interval = FALSE,level = 0.95 ,nSim = 100)
predictedVal <- forcastedVal1$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))

model.koyck2 = koyckDlm(x = Home_Audio_Data$Affiliates, y = Home_Audio_Data$gmv)
summary(model.koyck2,diagnostics=TRUE) 
checkresiduals(model.koyck2$model, test = F)

forcastedVal2 <-dLagM::forecast(model = model.koyck2  , x = c(1:20), h = 20 , interval = FALSE,level = 0.95 ,nSim = 100)
predictedVal <- forcastedVal2$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))


model.koyck3 = koyckDlm(x = Home_Audio_Data$SEM, y = Home_Audio_Data$gmv)
summary(model.koyck3,diagnostics=TRUE) 
checkresiduals(model.koyck3$model, test = F)

forcastedVal3 <-dLagM::forecast(model = model.koyck3  , x = c(1:20), h = 20 , interval = FALSE,level = 0.95 ,nSim = 100)
predictedVal <- forcastedVal3$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))





###############################################################################################
#### Koyck Model : Gaming Accessory 
###############################################################################################
###############################################################################################



GA_data <- capstonewithoutna_withinperiod_Filter_grpbyOdate_media_GA


# we already have got attributes from linear model which is userful so We are using attributes from linear model for further analysis

# gmv ~ weeknumber + product_mrp + units +  NPS +  GamingHeadset + GamingMouse + GamingMousePad + JoystickGamingWheel +  TVOutCableAccessory + TV + Sponsorship + Content_Marketing + Online_marketing 

trainindices <- sample(1:nrow(Gaming_Accessory_data),0.6*nrow(Gaming_Accessory_data))
#training data set
train = Gaming_Accessory_data[trainindices,]
# testing data set
test = Gaming_Accessory_data[-trainindices,]

model.koyck1 = koyckDlm(x = GA_data$GamingMousePad, y = GA_data$gmv)
summary(model.koyck1,diagnostics=TRUE) 
checkresiduals(model.koyck1$model, test = F)

forcastedVal1 <-dLagM::forecast(model = model.koyck1  , x = c(1:21), h = 21 , interval = FALSE,level = 0.95 ,nSim = 100)
predictedVal <- forcastedVal1$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))


model.koyck3 = koyckDlm(x = GA_data$weeknumber, y = GA_data$gmv)
summary(model.koyck3,diagnostics=TRUE) 
checkresiduals(model.koyck3$model, test = F)

forcastedVal3 <-dLagM::forecast(model = model.koyck3  , x = c(1:21), h = 21 , interval = FALSE,level = 0.95 ,nSim = 100)
predictedVal <- forcastedVal3$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))


model.koyck2 = koyckDlm(x = GA_data$JoystickGamingWheel, y = GA_data$gmv)
summary(model.koyck2,diagnostics=TRUE) 
checkresiduals(model.koyck2$model, test = F)

forcastedVal2 <-dLagM::forecast(model = model.koyck2  , x = c(1:21), h = 21 , interval = FALSE,level = 0.95 ,nSim = 100)
predictedVal <- forcastedVal2$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))

model.koyck4 = koyckDlm(x = GA_data$TVOutCableAccessory, y = GA_data$gmv)
summary(model.koyck4,diagnostics=TRUE) 
checkresiduals(model.koyck4$model, test = F)

forcastedVal4 <-dLagM::forecast(model = model.koyck4  , x = c(1:21), h = 21 , interval = FALSE,level = 0.95 ,nSim = 100)
predictedVal <- forcastedVal4$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))

model.koyck5 = koyckDlm(x = GA_data$TV, y = GA_data$gmv)
summary(model.koyck5,diagnostics=TRUE) 
checkresiduals(model.koyck4$model, test = F)

forcastedVal5 <-dLagM::forecast(model = model.koyck5  , x = c(1:21), h = 21 , interval = FALSE,level = 0.95 ,nSim = 100)
predictedVal <- forcastedVal5$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))

model.koyck6 = koyckDlm(x = GA_data$Content_Marketing, y = GA_data$gmv)
summary(model.koyck6,diagnostics=TRUE) 
checkresiduals(model.koyck6$model, test = F)

forcastedVal6 <-dLagM::forecast(model = model.koyck6  , x = c(1:21), h = 21 , interval = FALSE,level = 0.95 ,nSim = 100)
predictedVal <- forcastedVal6$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))

model.koyck7 = koyckDlm(x = GA_data$gmv, y = GA_data$Affiliates)
summary(model.koyck7,diagnostics=TRUE) 
checkresiduals(model.koyck7$model, test = F)

forcastedVal7 <-dLagM::forecast(model = model.koyck7  , x = c(1:21), h = 21 , interval = FALSE,level = 0.95 ,nSim = 100)
predictedVal <- forcastedVal7$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))



