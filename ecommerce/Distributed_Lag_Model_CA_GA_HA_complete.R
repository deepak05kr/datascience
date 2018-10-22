#Import required libraries
library(dLagM)
library(dynlm)
library(forecast)

###############################################################################################
#                              DISTRIBUTED LAG MODEL FOR Camera Accessory_data
###############################################################################################
camera_accessory_data  <- capstonewithoutna_withinperiod_Filter_grpbyOdate_media_CA 

colnames(camera_accessory_data)
summary(camera_accessory_data)
str(camera_accessory_data)
camera_accessory_data <- camera_accessory_data[,-c(2,8,15:18,22,25,27,28,30,38,42,43)]

trainindices <- sample(1:nrow(camera_accessory_data),0.6*nrow(camera_accessory_data))
#training data set
train = camera_accessory_data[trainindices,]
# testing data set
test = camera_accessory_data[-trainindices,]

str(train)
#Using finiteDLMauto find the right value for Q
fdlm1<-finiteDLMauto(formula = gmv ~ weeknumber + units + discount + NPS + Percent + CameraAccessory + CameraBag + CameraBattery + CameraBatteryCharger + CameraBatteryGrip + 
                       CameraMount + CameraRemoteControl + CameraTripod + Filter + Flash + Telescope + IsmoreListPrice + isSaleday + TV + Digital + Sponsorship + 
                       Online_marketing + Affiliates + SEM, data = train, q.min = 1, q.max = 10, k.order = NULL,model.type = c("dlm","poly"), 
                       error.type = c("MASE","AIC","BIC","radj"), trace = FALSE)

summary(fdlm1)

#From "finiteDLMauto" we could make out q with 1 is the right value

dlm2a <- dlm(formula = gmv ~ weeknumber + units + discount +  Percent +  CameraBattery +    CameraTripod + Flash +  TV + Digital + Sponsorship , data = train,q=1)
summary(dlm2a)
vif(dlm2a$model)

# removing Percent 
dlm3  <- dlm(formula = gmv ~ weeknumber + units + discount +  CameraBattery +    CameraTripod + Flash +  TV + Digital + Sponsorship , data = train,q=1)
summary(dlm3)
vif(dlm3$model)

# removing discount 
dlm4  <- dlm(formula = gmv ~ weeknumber + units + CameraBattery +    CameraTripod + Flash +  TV + Digital + Sponsorship , data = train,q=1)
summary(dlm4)
vif(dlm4$model)

# removing weeknumber 
dlm5  <- dlm(formula = gmv ~ units + CameraBattery +    CameraTripod + Flash +  TV + Digital + Sponsorship , data = train,q=1)
summary(dlm5)
vif(dlm5$model)

# removing CameraBattery 
dlm6  <- dlm(formula = gmv ~ units + CameraTripod + Flash +  TV + Digital + Sponsorship , data = train,q=1)
summary(dlm6)
vif(dlm6$model)

# removing Digital 
dlm7  <- dlm(formula = gmv ~ units + CameraTripod + Flash +  TV + Sponsorship , data = train,q=1)
summary(dlm7)
vif(dlm7$model)

# removing Sponsorship 
dlm8  <- dlm(formula = gmv ~ units + CameraTripod + Flash +  TV , data = train,q=1)
summary(dlm8)
vif(dlm8$model)

# removing CameraTripod 
dlm9  <- dlm(formula = gmv ~ units + Flash +  TV , data = train,q=1)
summary(dlm9)
vif(dlm9$model)

# removing CameraTripod 
dlm10  <- dlm(formula = gmv ~ units + Flash , data = train,q=1)
summary(dlm10)
vif(dlm10$model)

#cHECK THE RESIDUALS
bgtest(dlm10$model)
checkresiduals(dlm10$model$residuals, test = F)


# camera_accessory_data  <- capstonewithoutna_withinperiod_Filter_grpbyOdate_media_CA 
# camera_accessory_data <- camera_accessory_data[,-c(2,8,15:18,22,25,27,28,30,38,42,43)]
# trainindices <- sample(1:nrow(camera_accessory_data),0.6*nrow(camera_accessory_data))
# train = camera_accessory_data[trainindices,]
# test = camera_accessory_data[-trainindices,]
dlm2a <- dlm(formula = gmv ~ weeknumber + units + discount +  Percent +  CameraBattery +    CameraTripod + Flash +  TV + Digital + Sponsorship , data = train,q=1)

testselect<-test%>%dplyr::select(weeknumber,units,discount,Percent,CameraBattery,CameraTripod,Flash,TV,Digital,Sponsorship)

transposetestselect<-t(testselect)

dlmforecast<-dLagM::forecast(dlm2a , transposetestselect , h = 21 , interval = FALSE, level = 0.95 , nSim = 500)
predictedVal <-dlmforecast$forecasts
test$predicted_gmv <- predictedVal
# testing of r sqare between actual and predicted 
cameraAccessory_rsquare <- cor(test$predicted_gmv,test$gmv)^2
cameraAccessory_rsquare
cameraAccessory_r <- cor(test$predicted_gmv,test$gmv)
cameraAccessory_r
ggplot(test, aes(gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))


###############################################################################################
#                              DISTRIBUTED LAG MODEL FOR GAMING Accessory_data
###############################################################################################

GA_data <- capstonewithoutna_withinperiod_Filter_grpbyOdate_media_GA
GA_data <- GA_data[,-c(2,8,10,14,20,34,35)]
GA_data[,1:23] <- data.frame(scale(GA_data[,1:23], center = TRUE))

trainindices <- sample(1:nrow(GA_data),0.6*nrow(GA_data))
#training data set
train = GA_data[trainindices,]
# testing data set
test = GA_data[-trainindices,]

#Using finiteDLMauto find the right value for Q
# fdlm<-finiteDLMauto(formula = gmv ~ weeknumber + units + discount + NPS + Percent + CameraAccessory + CameraBag + CameraBattery + CameraBatteryCharger + CameraBatteryGrip + 
#                        CameraMount + CameraRemoteControl + CameraTripod + Filter + Flash + Telescope + IsmoreListPrice + isSaleday + TV + Digital + Sponsorship + 
#                        Online_marketing + Affiliates + SEM, data = train, q.min = 1, q.max = 10, k.order = NULL,model.type = c("dlm","poly"), 
#                      error.type = c("MASE","AIC","BIC","radj"), trace = FALSE)
# 
# summary(fdlm)

dlmGA2a <- dlm(formula = gmv ~ weeknumber + product_mrp + units +  NPS +  GamingHeadset + GamingMouse + GamingMousePad + JoystickGamingWheel +  TVOutCableAccessory + 
                  TV + Sponsorship + Content_Marketing + Online_marketing ,data=train,q=1)


summary(dlmGA2a)
vif(dlmGA2a$model)

# removing weeknumber 
dlmGA3 <- dlm(formula = gmv ~ product_mrp + units +  NPS +  GamingHeadset + GamingMouse + GamingMousePad + JoystickGamingWheel +  TVOutCableAccessory + 
                 TV + Sponsorship + Content_Marketing + Online_marketing ,data=train,q=1)

summary(dlmGA3)
vif(dlmGA3$model)

# removing Content_Marketing 
dlmGA4 <- dlm(formula = gmv ~ product_mrp + units +  NPS +  GamingHeadset + GamingMouse + GamingMousePad + JoystickGamingWheel +  TVOutCableAccessory + 
                TV + Sponsorship + Online_marketing ,data=train,q=1)

summary(dlmGA4)
vif(dlmGA4$model)

# removing units 
dlmGA5 <- dlm(formula = gmv ~ product_mrp + NPS +  GamingHeadset + GamingMouse + GamingMousePad + JoystickGamingWheel +  TVOutCableAccessory + 
                TV + Sponsorship + Online_marketing ,data=train,q=1)

summary(dlmGA5)
vif(dlmGA5$model)

# removing Sponsorship 
dlmGA6 <- dlm(formula = gmv ~ product_mrp + NPS +  GamingHeadset + GamingMouse + GamingMousePad + JoystickGamingWheel +  TVOutCableAccessory + 
                TV + Online_marketing ,data=train,q=1)

summary(dlmGA6)
vif(dlmGA6$model)

# removing Online_marketing 
dlmGA7 <- dlm(formula = gmv ~ product_mrp + NPS +  GamingHeadset + GamingMouse + GamingMousePad + JoystickGamingWheel +  TVOutCableAccessory + 
                TV ,data=train,q=1)

summary(dlmGA7)
vif(dlmGA7$model)

# removing GamingHeadset 
dlmGA8 <- dlm(formula = gmv ~ product_mrp + NPS + GamingMouse + GamingMousePad + JoystickGamingWheel +  TVOutCableAccessory + 
                TV ,data=train,q=1)

summary(dlmGA8)
vif(dlmGA8$model)

# removing product_mrp 
dlmGA9 <- dlm(formula = gmv ~ NPS + GamingMouse + GamingMousePad + JoystickGamingWheel +  TVOutCableAccessory + TV ,data=train,q=1)

summary(dlmGA9)
vif(dlmGA9$model)

# removing TV 
dlmGA10 <- dlm(formula = gmv ~ NPS + GamingMouse + GamingMousePad + JoystickGamingWheel +  TVOutCableAccessory ,data=train,q=1)

summary(dlmGA10)
vif(dlmGA10$model)

# removing NPS 
dlmGA11 <- dlm(formula = gmv ~ GamingMouse + GamingMousePad + JoystickGamingWheel +  TVOutCableAccessory ,data=train,q=1)

summary(dlmGA11)
vif(dlmGA11$model)

# removing GamingMouse 
dlmGA12 <- dlm(formula = gmv ~ GamingMousePad + JoystickGamingWheel +  TVOutCableAccessory ,data=train,q=1)

summary(dlmGA12)
vif(dlmGA12$model)

# removing GamingMousePad 
dlmGA13 <- dlm(formula = gmv ~ JoystickGamingWheel +  TVOutCableAccessory ,data=train,q=1)

summary(dlmGA13)
vif(dlmGA13$model)


bgtest(dlmGA13$model)
checkresiduals(dlmGA13$model$residuals, test = F)

#GA Forecast
testselectGA <-test%>%dplyr::select(weeknumber,product_mrp,units,NPS,GamingHeadset,GamingMouse,GamingMousePad,JoystickGamingWheel,TVOutCableAccessory,TV,
                                    Sponsorship,Content_Marketing,Online_marketing)

transposetestselect<-t(testselectGA)

dlmforecastGA <-dLagM::forecast(dlmGA2a , transposetestselect , h = 21 , interval = FALSE, level = 0.95 , nSim = 500)

predictedVal <-dlmforecastGA$forecasts
test$predicted_gmv <- predictedVal
# testing of r sqare between actual and predicted 
GA_rsquare <- cor(test$predicted_gmv,test$gmv)^2
GA_rsquare
GA_r <- cor(test$predicted_gmv,test$gmv)
GA_r
ggplot(test, aes(gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))

###############################################################################################
#                              DISTRIBUTED LAG MODEL FOR HOME AUDIO_data
###############################################################################################

Home_Audio_data <- capstonewithoutna_withinperiod_Filter_grpbyOdate_media_HA
Home_Audio_data <- Home_Audio_data[,-c(2,8,15,16,17,30)]
Home_Audio_data[,1:24] <- data.frame(scale(Home_Audio_data[,1:24], center = TRUE))

trainindices <- sample(1:nrow(Home_Audio_data),0.6*nrow(Home_Audio_data))
#training data set
train = Home_Audio_data[trainindices,]
# testing data set
test = Home_Audio_data[-trainindices,]

dlmHA2a <- dlm(formula = gmv ~ weeknumber + product_mrp + units + Affiliates + SEM, data=train,q=1)

#HA CHECK RESIDUALS
bgtest(dlmHA2a$model)
checkresiduals(dlmHA2a$model$residuals, test = F)

#HA FORECAST 
testselectHA <-test%>%dplyr::select(weeknumber,product_mrp,units, Affiliates,SEM)
nrow(testselectHA)

transposetestselect<-t(testselectHA)

dlmforecastHA <-dLagM::forecast(dlmHA2a , transposetestselect , h = 20 , interval = FALSE, level = 0.95 , nSim = 500)

predictedVal <-dlmforecastHA$forecasts
test$predicted_gmv <- predictedVal
# testing of r sqare between actual and predicted 
GA_rsquare <- cor(test$predicted_gmv,test$gmv)^2
GA_rsquare
GA_r <- cor(test$predicted_gmv,test$gmv)
GA_r

ggplot(test, aes(gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))

