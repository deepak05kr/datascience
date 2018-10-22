
################################################################################################
#### Auto Regressive Distributed Lag Models Camera Accessory
################################################################################################

library(forecast)
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

## using  discount + CameraTripod + Flash + CameraBattery

trainindices <- sample(1:nrow(Camera_Accessory_data),0.6*nrow(Camera_Accessory_data))
#training data set
train = Camera_Accessory_data[trainindices,]
# testing data set
test = Camera_Accessory_data[-trainindices,]

nrow(train)
nrow(test)

gmv_CameraAccessory <- ts(train$gmv,frequency = 12)
discount_CameraAccessory <- ts(train$discount,frequency = 12)
CameraTripod_CameraAccessory <- ts(train$CameraTripod,frequency = 12)
Flash_CameraAccessory <- ts(train$Flash,frequency = 12)
CameraBattery_CameraAccessory <- ts(train$CameraBattery,frequency = 12)


gmv_Flash = ts.intersect(gmv_CameraAccessory , Flash_CameraAccessory )
gmv_CameraBattery = ts.intersect(gmv_CameraAccessory ,CameraBattery_CameraAccessory )

colnames(gmv_Flash) =  c("GMV","Flash")
colnames(gmv_CameraBattery) =  c("GMV","CameraBattery")

ardlmodel1 = ardlDlm(y=as.vector(gmv_CameraAccessory),x=as.vector(CameraBattery_CameraAccessory),p = 1,q = 1)
checkresiduals(ardlmodel1$model, test = F)
summary(ardlmodel1)

forcasted_ardl1 <-dLagM::forecast(ardlmodel1 ,x = c(1:21), h = 21 , interval = FALSE, level = 0.95 , nSim = 500)
predictedVal <-forcasted_ardl1$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))

ardlmodel2 = ardlDlm(y=as.vector(gmv_CameraAccessory),x=as.vector(CameraBattery_CameraAccessory),p = 2,q = 2)
checkresiduals(ardlmodel2$model, test = F)
summary(ardlmodel2)

forcasted_ardl2 <-dLagM::forecast(ardlmodel2 ,x = c(1:21), h = 21 , interval = FALSE, level = 0.95 , nSim = 500)
predictedVal <-forcasted_ardl2$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))

ardlmodel3 = ardlDlm(y=as.vector(gmv_CameraAccessory),x=as.vector(CameraBattery_CameraAccessory),p = 3,q = 3)
checkresiduals(ardlmodel3$model, test = F)
summary(ardlmodel3)

forcasted_ardl3 <-dLagM::forecast(ardlmodel3 ,x = c(1:21), h = 21 , interval = FALSE, level = 0.95 , nSim = 500)
predictedVal <-forcasted_ardl3$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))

# below model ardlmodel4  has hightes R square
ardlmodel4 = ardlDlm(y=as.vector(gmv_CameraAccessory),x=as.vector(Flash_CameraAccessory),p = 1,q = 1)
checkresiduals(ardlmodel4$model, test = F)
summary(ardlmodel4)

forcasted_ardl4 <-dLagM::forecast(ardlmodel4 ,x = c(1:21), h = 21 , interval = FALSE, level = 0.95 , nSim = 500)
predictedVal <-forcasted_ardl4$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))

ardlmodel5 = ardlDlm(y=as.vector(gmv_CameraAccessory),x=as.vector(Flash_CameraAccessory),p = 2,q = 2)
checkresiduals(ardlmodel5$model, test = F)
summary(ardlmodel5)

forcasted_ardl5 <-dLagM::forecast(ardlmodel5 ,x = c(1:21), h = 21 , interval = FALSE, level = 0.95 , nSim = 500)
predictedVal <-forcasted_ardl5$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))


ardlmodel6 = ardlDlm(y=as.vector(gmv_CameraAccessory),x=as.vector(Flash_CameraAccessory),p = 3,q = 3)
checkresiduals(ardlmodel6$model, test = F)
summary(ardlmodel6)

forcasted_ardl6 <-dLagM::forecast(ardlmodel6 ,x = c(1:21), h = 21 , interval = FALSE, level = 0.95 , nSim = 500)
predictedVal <-forcasted_ardl6$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))


ardlmodel7 = ardlDlm(y=as.vector(gmv_CameraAccessory),x=as.vector(discount_CameraAccessory),p = 1,q = 1)
checkresiduals(ardlmodel7$model, test = F)
summary(ardlmodel7)

forcasted_ardl7 <-dLagM::forecast(ardlmodel7 ,x = c(1:21), h = 21 , interval = FALSE, level = 0.95 , nSim = 500)
predictedVal <-forcasted_ardl7$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))

ardlmodel8 = ardlDlm(y=as.vector(gmv_CameraAccessory),x=as.vector(discount_CameraAccessory),p = 2,q = 2)
checkresiduals(ardlmodel8$model, test = F)
summary(ardlmodel8)

forcasted_ardl8 <-dLagM::forecast(ardlmodel8 ,x = c(1:21), h = 21 , interval = FALSE, level = 0.95 , nSim = 500)
predictedVal <-forcasted_ardl8$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))

ardlmodel9 = ardlDlm(y=as.vector(gmv_CameraAccessory),x=as.vector(discount_CameraAccessory),p = 3,q = 3)
checkresiduals(ardlmodel9$model, test = F)
summary(ardlmodel9)

forcasted_ardl9 <-dLagM::forecast(ardlmodel9 ,x = c(1:21), h = 21 , interval = FALSE, level = 0.95 , nSim = 500)
predictedVal <-forcasted_ardl9$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))

ardlmodel10 = ardlDlm(y=as.vector(gmv_CameraAccessory),x=as.vector(CameraTripod_CameraAccessory),p = 1,q = 1)
checkresiduals(ardlmodel10$model, test = F)
summary(ardlmodel10)

forcasted_ardl10 <-dLagM::forecast(ardlmodel10 ,x = c(1:21), h = 21 , interval = FALSE, level = 0.95 , nSim = 500)
predictedVal <-forcasted_ardl10$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))

ardlmodel11 = ardlDlm(y=as.vector(gmv_CameraAccessory),x=as.vector(CameraTripod_CameraAccessory),p = 2,q = 2)
checkresiduals(ardlmodel11$model, test = F)
summary(ardlmodel11)

forcasted_ardl11 <-dLagM::forecast(ardlmodel11 ,x = c(1:21), h = 21 , interval = FALSE, level = 0.95 , nSim = 500)
predictedVal <-forcasted_ardl11$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))


ardlmodel12 = ardlDlm(y=as.vector(gmv_CameraAccessory),x=as.vector(CameraTripod_CameraAccessory),p = 3,q = 3)
checkresiduals(ardlmodel12$model, test = F)
summary(ardlmodel12)

forcasted_ardl12 <-dLagM::forecast(ardlmodel12 ,x = c(1:21), h = 21 , interval = FALSE, level = 0.95 , nSim = 500)
predictedVal <-forcasted_ardl12$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))


modeldatadf = data.frame(Model=character(),MASE=numeric(),
                         BIC= numeric(),AIC=numeric())
modeldatadf = rbind(modeldatadf,cbind(Model="ARDL q1",MASE=MASE(ardlmodel1),
                                      BIC = BIC(ardlmodel1$model),
                                      AIC = AIC(ardlmodel1$model)))

modeldatadf = rbind(modeldatadf,cbind(Model="ARDL q2",MASE=MASE(ardlmodel2),
                                      BIC = BIC(ardlmodel2$model),
                                      AIC = AIC(ardlmodel2$model)))

modeldatadf = rbind(modeldatadf,cbind(Model="ARDL q3",MASE=MASE(ardlmodel3),
                                      BIC = BIC(ardlmodel3$model),
                                      AIC = AIC(ardlmodel3$model)))
modeldatadf = rbind(modeldatadf,cbind(Model="ARDL q4",MASE=MASE(ardlmodel4),
                                      BIC = BIC(ardlmodel4$model),
                                      AIC = AIC(ardlmodel4$model)))

modeldatadf = rbind(modeldatadf,cbind(Model="ARDL q5",MASE=MASE(ardlmodel5),
                                      BIC = BIC(ardlmodel5$model),
                                      AIC = AIC(ardlmodel5$model)))

modeldatadf = rbind(modeldatadf,cbind(Model="ARDL q6",MASE=MASE(ardlmodel6),
                                      BIC = BIC(ardlmodel6$model),
                                      AIC = AIC(ardlmodel6$model)))

modeldatadf = rbind(modeldatadf,cbind(Model="ARDL q7",MASE=MASE(ardlmodel7),
                                      BIC = BIC(ardlmodel7$model),
                                      AIC = AIC(ardlmodel7$model)))

modeldatadf = rbind(modeldatadf,cbind(Model="ARDL q8",MASE=MASE(ardlmodel8),
                                      BIC = BIC(ardlmodel8$model),
                                      AIC = AIC(ardlmodel8$model)))
modeldatadf = rbind(modeldatadf,cbind(Model="ARDL q9",MASE=MASE(ardlmodel9),
                                      BIC = BIC(ardlmodel9$model),
                                      AIC = AIC(ardlmodel9$model)))
modeldatadf = rbind(modeldatadf,cbind(Model="ARDL q10",MASE=MASE(ardlmodel10),
                                      BIC = BIC(ardlmodel10$model),
                                      AIC = AIC(ardlmodel10$model)))
modeldatadf = rbind(modeldatadf,cbind(Model="ARDL q11",MASE=MASE(ardlmodel11),
                                      BIC = BIC(ardlmodel11$model),
                                      AIC = AIC(ardlmodel11$model)))
modeldatadf = rbind(modeldatadf,cbind(Model="ARDL q12",MASE=MASE(ardlmodel12),
                                      BIC = BIC(ardlmodel12$model),
                                      AIC = AIC(ardlmodel12$model)))
rownames(modeldatadf) <- c() 
knitr::kable(modeldatadf, caption = "Model Parameters ")




################################################################################################
#### Auto Regressive Distributed Lag Model HOME AUDIO
################################################################################################

library(forecast)
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

Home_Audio_data <- capstonewithoutna_withinperiod_Filter_grpbyOdate_media_HA

# we already have got attributes from linear model which is userful so We are using attributes from linear model for further analysis

##  units + Affiliates + SEM

trainindices <- sample(1:nrow(Home_Audio_data),0.6*nrow(Home_Audio_data))
#training data set
train = Home_Audio_data[trainindices,]
# testing data set
test = Home_Audio_data[-trainindices,]

nrow(train)
nrow(test)

gmv_homeAudio <- ts(train$gmv,frequency = 12)
sem_homeAudio <- ts(train$SEM,frequency = 12)
affiliates_homeAudio <- ts(train$Affiliates,frequency = 12)
units_homeAudio <- ts(train$units,frequency = 12)
productmrp_HomeAudio <- ts(train$product_mrp,frequency = 12)


gmv_units = ts.intersect(gmv_homeAudio , units_homeAudio )
gmv_productmrp = ts.intersect(gmv_homeAudio ,productmrp_HomeAudio )

colnames(gmv_units) =  c("GMV","Units")
colnames(gmv_productmrp) =  c("GMV","ProductMrp")

ardlmodel1 = ardlDlm(y=as.vector(gmv_homeAudio),x=as.vector(productmrp_HomeAudio),p = 1,q = 1)
checkresiduals(ardlmodel1$model, test = F)
summary(ardlmodel1)

forcasted_ardl1 <-dLagM::forecast(ardlmodel1 ,x = c(1:20), h = 20 , interval = FALSE, level = 0.95 , nSim = 500)
predictedVal <-forcasted_ardl1$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))

ardlmodel2 = ardlDlm(y=as.vector(gmv_homeAudio),x=as.vector(productmrp_HomeAudio),p = 2,q = 2)
checkresiduals(ardlmodel2$model, test = F)
summary(ardlmodel2)

forcasted_ardl2 <-dLagM::forecast(ardlmodel2 ,x = c(1:20), h = 20 , interval = FALSE, level = 0.95 , nSim = 500)
predictedVal <-forcasted_ardl2$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))

ardlmodel3 = ardlDlm(y=as.vector(gmv_homeAudio),x=as.vector(productmrp_HomeAudio),p = 3,q = 3)
checkresiduals(ardlmodel3$model, test = F)
summary(ardlmodel3)

forcasted_ardl3 <-dLagM::forecast(ardlmodel3 ,x = c(1:20), h = 20 , interval = FALSE, level = 0.95 , nSim = 500)
predictedVal <-forcasted_ardl3$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))

ardlmodel4 = ardlDlm(y=as.vector(gmv_homeAudio),x=as.vector(units_homeAudio),p = 1,q = 1)
checkresiduals(ardlmodel4$model, test = F)
summary(ardlmodel4)

# below model has very high R square value
forcasted_ardl4 <-dLagM::forecast(ardlmodel4 ,x = c(1:20), h = 20 , interval = FALSE, level = 0.95 , nSim = 500)
predictedVal <-forcasted_ardl4$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))


ardlmodel5 = ardlDlm(y=as.vector(gmv_homeAudio),x=as.vector(units_homeAudio),p = 2,q = 2)
checkresiduals(ardlmodel5$model, test = F)
summary(ardlmodel5)

forcasted_ardl5 <-dLagM::forecast(ardlmodel5 ,x = c(1:20), h = 20 , interval = FALSE, level = 0.95 , nSim = 500)
predictedVal <-forcasted_ardl5$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))

## below model has very high R square value
ardlmodel6 = ardlDlm(y=as.vector(gmv_homeAudio),x=as.vector(units_homeAudio),p = 3,q = 3)
checkresiduals(ardlmodel6$model, test = F)
summary(ardlmodel6)

forcasted_ardl6 <-dLagM::forecast(ardlmodel6 ,x = c(1:20), h = 20 , interval = FALSE, level = 0.95 , nSim = 500)
predictedVal <-forcasted_ardl6$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))

ardlmodel7 = ardlDlm(y=as.vector(gmv_homeAudio),x=as.vector(sem_homeAudio),p = 1,q = 1)
checkresiduals(ardlmodel7$model, test = F)
summary(ardlmodel7)

forcasted_ardl7 <-dLagM::forecast(ardlmodel7 ,x = c(1:20), h = 20 , interval = FALSE, level = 0.95 , nSim = 500)
predictedVal <-forcasted_ardl7$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))

ardlmodel8 = ardlDlm(y=as.vector(gmv_homeAudio),x=as.vector(sem_homeAudio),p = 2,q = 2)
checkresiduals(ardlmodel8$model, test = F)
summary(ardlmodel8)

forcasted_ardl8 <-dLagM::forecast(ardlmodel8 ,x = c(1:20), h = 20 , interval = FALSE, level = 0.95 , nSim = 500)
predictedVal <-forcasted_ardl8$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))

ardlmodel9 = ardlDlm(y=as.vector(gmv_homeAudio),x=as.vector(sem_homeAudio),p = 3,q = 3)
checkresiduals(ardlmodel9$model, test = F)
summary(ardlmodel9)

forcasted_ardl9 <-dLagM::forecast(ardlmodel9 ,x = c(1:20), h = 20 , interval = FALSE, level = 0.95 , nSim = 500)
predictedVal <-forcasted_ardl9$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))


ardlmodel10 = ardlDlm(y=as.vector(gmv_homeAudio),x=as.vector(affiliates_homeAudio),p = 1,q = 1)
checkresiduals(ardlmodel10$model, test = F)
summary(ardlmodel10)

forcasted_ardl10 <-dLagM::forecast(ardlmodel10 ,x = c(1:20), h = 20 , interval = FALSE, level = 0.95 , nSim = 500)
predictedVal <-forcasted_ardl10$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))

ardlmodel11 = ardlDlm(y=as.vector(gmv_homeAudio),x=as.vector(affiliates_homeAudio),p = 2,q = 2)
checkresiduals(ardlmodel11$model, test = F)
summary(ardlmodel11)

forcasted_ardl11 <-dLagM::forecast(ardlmodel11 ,x = c(1:20), h = 20 , interval = FALSE, level = 0.95 , nSim = 500)
predictedVal <-forcasted_ardl11$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))

ardlmodel12 = ardlDlm(y=as.vector(gmv_homeAudio),x=as.vector(affiliates_homeAudio),p = 3,q = 3)
checkresiduals(ardlmodel12$model, test = F)
summary(ardlmodel12)

forcasted_ardl12 <-dLagM::forecast(ardlmodel12 ,x = c(1:20), h = 20 , interval = FALSE, level = 0.95 , nSim = 500)
predictedVal <-forcasted_ardl12$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))


modeldatadf = data.frame(Model=character(),MASE=numeric(),
                         BIC= numeric(),AIC=numeric())
modeldatadf = rbind(modeldatadf,cbind(Model="ARDL q1",MASE=MASE(ardlmodel1),
                                        BIC = BIC(ardlmodel1$model),
                                        AIC = AIC(ardlmodel1$model)))

modeldatadf = rbind(modeldatadf,cbind(Model="ARDL q2",MASE=MASE(ardlmodel2),
                                      BIC = BIC(ardlmodel2$model),
                                      AIC = AIC(ardlmodel2$model)))

modeldatadf = rbind(modeldatadf,cbind(Model="ARDL q3",MASE=MASE(ardlmodel3),
                                      BIC = BIC(ardlmodel3$model),
                                      AIC = AIC(ardlmodel3$model)))
modeldatadf = rbind(modeldatadf,cbind(Model="ARDL q4",MASE=MASE(ardlmodel4),
                                        BIC = BIC(ardlmodel4$model),
                                        AIC = AIC(ardlmodel4$model)))

modeldatadf = rbind(modeldatadf,cbind(Model="ARDL q5",MASE=MASE(ardlmodel5),
                                      BIC = BIC(ardlmodel5$model),
                                      AIC = AIC(ardlmodel5$model)))

modeldatadf = rbind(modeldatadf,cbind(Model="ARDL q6",MASE=MASE(ardlmodel6),
                                      BIC = BIC(ardlmodel6$model),
                                      AIC = AIC(ardlmodel6$model)))

modeldatadf = rbind(modeldatadf,cbind(Model="ARDL q7",MASE=MASE(ardlmodel7),
                                      BIC = BIC(ardlmodel7$model),
                                      AIC = AIC(ardlmodel7$model)))

modeldatadf = rbind(modeldatadf,cbind(Model="ARDL q8",MASE=MASE(ardlmodel8),
                                      BIC = BIC(ardlmodel8$model),
                                      AIC = AIC(ardlmodel8$model)))
modeldatadf = rbind(modeldatadf,cbind(Model="ARDL q9",MASE=MASE(ardlmodel9),
                                      BIC = BIC(ardlmodel9$model),
                                      AIC = AIC(ardlmodel9$model)))
modeldatadf = rbind(modeldatadf,cbind(Model="ARDL q10",MASE=MASE(ardlmodel10),
                                      BIC = BIC(ardlmodel10$model),
                                      AIC = AIC(ardlmodel10$model)))
modeldatadf = rbind(modeldatadf,cbind(Model="ARDL q11",MASE=MASE(ardlmodel11),
                                      BIC = BIC(ardlmodel11$model),
                                      AIC = AIC(ardlmodel11$model)))
modeldatadf = rbind(modeldatadf,cbind(Model="ARDL q12",MASE=MASE(ardlmodel12),
                                      BIC = BIC(ardlmodel12$model),
                                      AIC = AIC(ardlmodel12$model)))
rownames(modeldatadf) <- c() 
knitr::kable(modeldatadf, caption = "Model Parameters ")






















################################################################################################
#### Auto Regressive Distributed Lag Model Gaming Accessory
################################################################################################

library(forecast)
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

Gaming_Accessory_data <- capstonewithoutna_withinperiod_Filter_grpbyOdate_media_GA

# we already have got attributes from linear model which is userful so We are using attributes from linear model for further analysis

# gmv ~ weeknumber + product_mrp + units +  NPS +  GamingHeadset + GamingMouse + GamingMousePad + JoystickGamingWheel +  TVOutCableAccessory + TV + Sponsorship + Content_Marketing + Online_marketing 

## using   GamingMousePad + JoystickGamingWheel + Content_Marketing + GamingHeadset

trainindices <- sample(1:nrow(Gaming_Accessory_data),0.6*nrow(Gaming_Accessory_data))
#training data set
train = Gaming_Accessory_data[trainindices,]
# testing data set
test = Gaming_Accessory_data[-trainindices,]

nrow(train)
nrow(test)

gmv_GamingAccessory <- ts(train$gmv,frequency = 12)
GamingMousePad_GamingAccessory <- ts(train$discount,frequency = 12)
payment_GamingAccessory <- ts(train$PaymentType,frequency = 12)


ardlmodel1 = ardlDlm(y=as.vector(gmv_GamingAccessory),x=as.vector(GamingMousePad_GamingAccessory),p = 1,q = 1)
checkresiduals(ardlmodel7$model, test = F)
summary(ardlmodel1)

forcasted_ardl1 <-dLagM::forecast(ardlmodel1 ,x = c(1:21), h = 21 , interval = FALSE, level = 0.95 , nSim = 500)
predictedVal <-forcasted_ardl1$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))

ardlmodel2 = ardlDlm(y=as.vector(gmv_GamingAccessory),x=as.vector(GamingMousePad_GamingAccessory),p = 2,q = 2)
checkresiduals(ardlmodel8$model, test = F)
summary(ardlmodel2)

forcasted_ardl2 <-dLagM::forecast(ardlmodel2 ,x = c(1:21), h = 21 , interval = FALSE, level = 0.95 , nSim = 500)
predictedVal <-forcasted_ardl2$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))

ardlmodel3 = ardlDlm(y=as.vector(gmv_GamingAccessory),x=as.vector(GamingMousePad_GamingAccessory),p = 3,q = 3)
checkresiduals(ardlmodel9$model, test = F)
summary(ardlmodel3)

forcasted_ardl <-dLagM::forecast(ardlmodel3 ,x = c(1:21), h = 21 , interval = FALSE, level = 0.95 , nSim = 500)
predictedVal <-forcasted_ardl$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))

ardlmodel4 = ardlDlm(y=as.vector(gmv_GamingAccessory),x=as.vector(payment_GamingAccessory),p = 1,q = 1)
checkresiduals(ardlmodel9$model, test = F)
summary(ardlmodel4)

forcasted_ard4 <-dLagM::forecast(ardlmodel4 ,x = c(1:21), h = 21 , interval = FALSE, level = 0.95 , nSim = 500)
predictedVal <-forcasted_ardl4$forecasts
test$predicted_gmv <- predictedVal
ggplot(test, aes(test$gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))



modeldatadf = data.frame(Model=character(),MASE=numeric(),
                         BIC= numeric(),AIC=numeric())
modeldatadf = rbind(modeldatadf,cbind(Model="ARDL q1",MASE=MASE(ardlmodel1),
                                      BIC = BIC(ardlmodel1$model),
                                      AIC = AIC(ardlmodel1$model)))

modeldatadf = rbind(modeldatadf,cbind(Model="ARDL q2",MASE=MASE(ardlmodel2),
                                      BIC = BIC(ardlmodel2$model),
                                      AIC = AIC(ardlmodel2$model)))

modeldatadf = rbind(modeldatadf,cbind(Model="ARDL q3",MASE=MASE(ardlmodel3),
                                      BIC = BIC(ardlmodel3$model),
                                      AIC = AIC(ardlmodel3$model)))
rownames(modeldatadf) <- c() 
knitr::kable(modeldatadf, caption = "Model Parameters ")


































































































































































































