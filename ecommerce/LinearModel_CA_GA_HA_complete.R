###############################################################################################
###############################################################################################
###############################################################################################
## Linear Model CAMERA ACCESSORY
###############################################################################################


camera_accessory_data  <- capstonewithoutna_withinperiod_Filter_grpbyOdate_media_CA 

colnames(camera_accessory_data)
summary(camera_accessory_data)
str(camera_accessory_data)
camera_accessory_data <- camera_accessory_data[,-c(2,8,15:18,22,25,27,28,30,38,42,43)]

#log of all the variables
# camera_accessory_data <- data.frame(log(abs(camera_accessory_data[,1:23])))
# camera_accessory_data[,1:23] <- data.frame(scale(camera_accessory_data[,1:23], center = TRUE))
#removing below columns because it has infinite values
# camera_accessory_data <- camera_accessory_data[,-c(8:12,17,18,20:23)]

trainindices <- sample(1:nrow(camera_accessory_data),0.6*nrow(camera_accessory_data))
#training data set
train = camera_accessory_data[trainindices,]
# testing data set
test = camera_accessory_data[-trainindices,]

str(train)


Linearmodel <- lm(gmv~.,data=train)

summary(Linearmodel)

Linearmodel2 <- stepAIC(Linearmodel,direction = "both")
summary(Linearmodel2)
vif(Linearmodel2)

Linearmodel3 <- lm(formula = gmv ~ weeknumber + units + discount + NPS + Percent + CameraAccessory + CameraBag + CameraBattery + CameraBatteryCharger + CameraBatteryGrip + CameraMount + CameraRemoteControl + CameraTripod + Filter + Flash + Telescope + IsmoreListPrice + isSaleday + TV + Digital + Sponsorship + Online_marketing + Affiliates + SEM, data = train)

summary(Linearmodel3)
vif(Linearmodel3)

# removing CameraRemoteControl 
Linearmodel4 <-   lm(formula = gmv ~ weeknumber + units + discount + NPS + Percent + CameraAccessory + CameraBag + CameraBattery + CameraBatteryCharger + CameraBatteryGrip + CameraMount +  CameraTripod + Filter + Flash + Telescope + IsmoreListPrice + isSaleday + TV + Digital + Sponsorship + Online_marketing + Affiliates + SEM, data = train)

summary(Linearmodel4)
vif(Linearmodel4)

# removing  isSaleday 
Linearmodel5 <- lm(formula = gmv ~ weeknumber + units + discount + NPS + Percent + CameraAccessory + CameraBag + CameraBattery + CameraBatteryCharger + CameraBatteryGrip + CameraMount +  CameraTripod + Filter + Flash + Telescope + IsmoreListPrice + TV + Digital + Sponsorship + Online_marketing + Affiliates + SEM, data = train)

summary(Linearmodel5)
vif(Linearmodel5)


# removing  CameraBag 
Linearmodel6 <- lm(formula = gmv ~ weeknumber + units + discount + NPS + Percent + CameraAccessory +  CameraBattery + CameraBatteryCharger + CameraBatteryGrip + CameraMount +  CameraTripod + Filter + Flash + Telescope + IsmoreListPrice + TV + Digital + Sponsorship + Online_marketing + Affiliates + SEM, data = train)

summary(Linearmodel6)
vif(Linearmodel6)

# removing  CameraMount 
Linearmodel7 <- lm(formula = gmv ~ weeknumber + units + discount + NPS + Percent + CameraAccessory +  CameraBattery + CameraBatteryCharger + CameraBatteryGrip +  CameraTripod + Filter + Flash + Telescope + IsmoreListPrice + TV + Digital + Sponsorship + Online_marketing + Affiliates + SEM, data = train)

summary(Linearmodel7)
vif(Linearmodel7)

# removing  IsmoreListPrice 
Linearmodel8 <- lm(formula = gmv ~ weeknumber + units + discount + NPS + Percent + CameraAccessory +  CameraBattery + CameraBatteryCharger + CameraBatteryGrip +  CameraTripod + Filter + Flash + Telescope +  TV + Digital + Sponsorship + Online_marketing + Affiliates + SEM, data = train)

summary(Linearmodel8)
vif(Linearmodel8)

# removing   Affiliates 
Linearmodel9 <- lm(formula = gmv ~ weeknumber + units + discount + NPS + Percent + CameraAccessory +  CameraBattery + CameraBatteryCharger + CameraBatteryGrip +  CameraTripod + Filter + Flash + Telescope +  TV + Digital + Sponsorship + Online_marketing + SEM, data = train)

summary(Linearmodel9)
vif(Linearmodel9)

# removing    Filter 
Linearmodel10 <- lm(formula = gmv ~ weeknumber + units + discount + NPS + Percent + CameraAccessory +  CameraBattery + CameraBatteryCharger + CameraBatteryGrip +  CameraTripod + Flash + Telescope +  TV + Digital + Sponsorship + Online_marketing + SEM, data = train)

summary(Linearmodel10)
vif(Linearmodel10)

# removing   SEM
Linearmodel11 <- lm(formula = gmv ~ weeknumber + units + discount + NPS + Percent + CameraAccessory +  CameraBattery + CameraBatteryCharger + CameraBatteryGrip +  CameraTripod + Flash + Telescope +  TV + Digital + Sponsorship + Online_marketing , data = train)

summary(Linearmodel11)
vif(Linearmodel11)


# removing   CameraBatteryGrip 
Linearmodel12 <- lm(formula = gmv ~ weeknumber + units + discount + NPS + Percent + CameraAccessory +  CameraBattery + CameraBatteryCharger +   CameraTripod + Flash + Telescope +  TV + Digital + Sponsorship + Online_marketing , data = train)

summary(Linearmodel12)
vif(Linearmodel12)

# removing CameraAccessory 
Linearmodel13 <- lm(formula = gmv ~ weeknumber + units + discount + NPS + Percent +  CameraBattery + CameraBatteryCharger +   CameraTripod + Flash + Telescope +  TV + Digital + Sponsorship + Online_marketing , data = train)

summary(Linearmodel13)
vif(Linearmodel13)

# removing  CameraBatteryCharger 
Linearmodel14 <- lm(formula = gmv ~ weeknumber + units + discount + NPS + Percent +  CameraBattery +    CameraTripod + Flash + Telescope +  TV + Digital + Sponsorship + Online_marketing , data = train)

summary(Linearmodel14)
vif(Linearmodel14)

# removing  NPS 
Linearmodel15 <- lm(formula = gmv ~ weeknumber + units + discount +  Percent +  CameraBattery +    CameraTripod + Flash + Telescope +  TV + Digital + Sponsorship + Online_marketing , data = train)

summary(Linearmodel15)
vif(Linearmodel15)

# removing  Online_marketing
Linearmodel16 <- lm(formula = gmv ~ weeknumber + units + discount +  Percent +  CameraBattery +    CameraTripod + Flash + Telescope +  TV + Digital + Sponsorship , data = train)

summary(Linearmodel16)
vif(Linearmodel16)

# removing  Telescope 
Linearmodel17 <- lm(formula = gmv ~ weeknumber + units + discount +  Percent +  CameraBattery +    CameraTripod + Flash +  TV + Digital + Sponsorship , data = train)

summary(Linearmodel17)
vif(Linearmodel17)


predictedVal <- predict(Linearmodel17,test)
test$predicted_gmv <- predictedVal

# testing of r sqare between actual and predicted 
cameraAccessory_rsquare <- cor(test$predicted_gmv,test$gmv)^2
cameraAccessory_rsquare

cameraAccessory_r <- cor(test$predicted_gmv,test$gmv)
cameraAccessory_r

Linear_model <- Linearmodel17

elasticity_Funtion <- function(parameter) {
  x <- as.numeric(Linear_model$coefficients[parameter])
  return(x)
}

cameraAccessory_list <- list()

for(i in 2:length(Linear_model$coefficients)) {
  cameraAccessory_list[i-1] <- elasticity_Funtion(names(Linear_model$coefficients)[i])
}

cameraAccessory_Elasticity <- data.frame(names(Linear_model$coefficients[2:length(Linear_model$coefficients)]))
cameraAccessory_Elasticity <- cbind(cameraAccessory_Elasticity,do.call(rbind.data.frame, cameraAccessory_list))
colnames(cameraAccessory_Elasticity) <- c("Attributes","ElasticityValue")

cameraAccessory_Elasticity$direction <- ifelse(cameraAccessory_Elasticity$ElasticityValue > 0, "Positive", "Negative")

ggplot(data=cameraAccessory_Elasticity, aes(x=reorder(Attributes,ElasticityValue),y=ElasticityValue)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("cameraAccessory - Linear Model") +xlab("Attributes")


ggplot(test, aes(gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))

###############################################################################################
## Linear Model Gaming Accessories
###############################################################################################


GA_data <- capstonewithoutna_withinperiod_Filter_grpbyOdate_media_GA

colnames(GA_data)
summary(GA_data)
str(GA_data)
GA_data <- GA_data[,-c(2,8,10,14,20,34,35)]

#log of all the variables
#GA_data <- data.frame(log(abs(GA_data[,1:23])))
GA_data[,1:23] <- data.frame(scale(GA_data[,1:23], center = TRUE))
#removing below columns because it has infinite values
#GA_data <- GA_data[,-c(10,11,13,14:17,19,20,21)]

trainindices <- sample(1:nrow(GA_data),0.6*nrow(GA_data))
#training data set
train = GA_data[trainindices,]
# testing data set
test = GA_data[-trainindices,]

Linearmodel <- lm(gmv~.,data=train)

summary(Linearmodel)

Linearmodel2 <- stepAIC(Linearmodel,direction = "both")
summary(Linearmodel2)
vif(Linearmodel2)

Linearmodel3 <- lm(formula = gmv ~ weeknumber + product_mrp + units + discount + NPS + Percent + GamePad + GamingAccessoryKit + GamingAdapter + GamingHeadset + GamingKeyboard + GamingMemoryCard + GamingMouse + GamingMousePad + JoystickGamingWheel + MotionController + TVOutCableAccessory + PaymentType + IsmoreListPrice + TV + Sponsorship + Content_Marketing + Online_marketing + Affiliates, data = train)

summary(Linearmodel3)
vif(Linearmodel3)

# removing  PaymentType 
Linearmodel4 <- lm(formula = gmv ~ weeknumber + product_mrp + units + discount + NPS + Percent + GamePad + GamingAccessoryKit + GamingAdapter + GamingHeadset + GamingKeyboard + GamingMemoryCard + GamingMouse + GamingMousePad + JoystickGamingWheel + MotionController + TVOutCableAccessory + IsmoreListPrice + TV + Sponsorship + Content_Marketing + Online_marketing + Affiliates, data = train)

summary(Linearmodel4)
vif(Linearmodel4)

# removing  Affiliates
Linearmodel5 <- lm(formula = gmv ~ weeknumber + product_mrp + units + discount + NPS + Percent + GamePad + GamingAccessoryKit + GamingAdapter + GamingHeadset + GamingKeyboard + GamingMemoryCard + GamingMouse + GamingMousePad + JoystickGamingWheel + MotionController + TVOutCableAccessory + IsmoreListPrice + TV + Sponsorship + Content_Marketing + Online_marketing , data = train)

summary(Linearmodel5)
vif(Linearmodel5)

# removing   IsmoreListPrice 
Linearmodel6 <- lm(formula = gmv ~ weeknumber + product_mrp + units + discount + NPS + Percent + GamePad + GamingAccessoryKit + GamingAdapter + GamingHeadset + GamingKeyboard + GamingMemoryCard + GamingMouse + GamingMousePad + JoystickGamingWheel + MotionController + TVOutCableAccessory + TV + Sponsorship + Content_Marketing + Online_marketing , data = train)

summary(Linearmodel6)
vif(Linearmodel6)

# removing    GamePad 
Linearmodel7 <- lm(formula = gmv ~ weeknumber + product_mrp + units + discount + NPS + Percent + GamingAccessoryKit + GamingAdapter + GamingHeadset + GamingKeyboard + GamingMemoryCard + GamingMouse + GamingMousePad + JoystickGamingWheel + MotionController + TVOutCableAccessory + TV + Sponsorship + Content_Marketing + Online_marketing , data = train)

summary(Linearmodel7)
vif(Linearmodel7)

# removing     GamingKeyboard 
Linearmodel8 <- lm(formula = gmv ~ weeknumber + product_mrp + units + discount + NPS + Percent + GamingAccessoryKit + GamingAdapter + GamingHeadset + GamingMemoryCard + GamingMouse + GamingMousePad + JoystickGamingWheel + MotionController + TVOutCableAccessory + TV + Sponsorship + Content_Marketing + Online_marketing , data = train)

summary(Linearmodel8)
vif(Linearmodel8)

# removing   GamingAccessoryKit 
Linearmodel9 <- lm(formula = gmv ~ weeknumber + product_mrp + units + discount + NPS + Percent +  GamingAdapter + GamingHeadset + GamingMemoryCard + GamingMouse + GamingMousePad + JoystickGamingWheel + MotionController + TVOutCableAccessory + TV + Sponsorship + Content_Marketing + Online_marketing , data = train)

summary(Linearmodel9)
vif(Linearmodel9)

# removing     MotionController 
Linearmodel10 <- lm(formula = gmv ~ weeknumber + product_mrp + units + discount + NPS + Percent + GamingAdapter+ GamingHeadset + GamingMemoryCard + GamingMouse + GamingMousePad + JoystickGamingWheel +  TVOutCableAccessory + TV + Sponsorship + Content_Marketing + Online_marketing , data = train)

summary(Linearmodel10)
vif(Linearmodel10)

# removing    GamingAdapter
Linearmodel11 <- lm(formula = gmv ~ weeknumber + product_mrp + units + discount + NPS + Percent +  GamingHeadset + GamingMemoryCard + GamingMouse + GamingMousePad + JoystickGamingWheel +  TVOutCableAccessory + TV + Sponsorship + Content_Marketing + Online_marketing , data = train)

summary(Linearmodel11)
vif(Linearmodel11)

# removing   GamingMemoryCard 
Linearmodel12<- lm(formula = gmv ~ weeknumber + product_mrp + units + discount + NPS + Percent +  GamingHeadset + GamingMouse + GamingMousePad + JoystickGamingWheel +  TVOutCableAccessory + TV + Sponsorship + Content_Marketing + Online_marketing , data = train)

summary(Linearmodel12)
vif(Linearmodel12)


# removing  discount
Linearmodel13<- lm(formula = gmv ~ weeknumber + product_mrp + units +  NPS + Percent +  GamingHeadset + GamingMouse + GamingMousePad + JoystickGamingWheel +  TVOutCableAccessory + TV + Sponsorship + Content_Marketing + Online_marketing , data = train)

summary(Linearmodel13)
vif(Linearmodel13)

# removing  Percent 
Linearmodel14<- lm(formula = gmv ~ weeknumber + product_mrp + units +  NPS +  GamingHeadset + GamingMouse + GamingMousePad + JoystickGamingWheel +  TVOutCableAccessory + TV + Sponsorship + Content_Marketing + Online_marketing , data = train)

summary(Linearmodel14)
vif(Linearmodel14)

predictedVal <- predict(Linearmodel14,test)
test$predicted_gmv <- predictedVal

# testing of r sqare between actual and predicted 
GamingAccessory_rsquare <- cor(test$predicted_gmv,test$gmv)^2
GamingAccessory_rsquare

GamingAccessory_r <- cor(test$predicted_gmv,test$gmv)
GamingAccessory_r

Linear_model <- Linearmodel14

elasticity_Funtion <- function(parameter) {
  x <- as.numeric(Linear_model$coefficients[parameter])
  return(x)
}

GamingAccessory_list <- list()

for(i in 2:length(Linear_model$coefficients)) {
  GamingAccessory_list[i-1] <- elasticity_Funtion(names(Linear_model$coefficients)[i])
}

GamingAccessory_Elasticity <- data.frame(names(Linear_model$coefficients[2:length(Linear_model$coefficients)]))
GamingAccessory_Elasticity <- cbind(GamingAccessory_Elasticity,do.call(rbind.data.frame, GamingAccessory_list))
colnames(GamingAccessory_Elasticity) <- c("Attributes","ElasticityValue")

GamingAccessory_Elasticity$direction <- ifelse(GamingAccessory_Elasticity$ElasticityValue > 0, "Positive", "Negative")

ggplot(data=GamingAccessory_Elasticity, aes(x=reorder(Attributes,ElasticityValue),y=ElasticityValue)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("GamingAccessory - Linear Model") +xlab("Attributes")


ggplot(test, aes(gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))

###############################################################################################
## Linear Model HOME AUDIO
###############################################################################################

Home_Audio_data <- capstonewithoutna_withinperiod_Filter_grpbyOdate_media_HA

colnames(Home_Audio_data)
summary(Home_Audio_data)
str(Home_Audio_data)
Home_Audio_data <- Home_Audio_data[,-c(2,8,15,16,17,30)]

Home_Audio_data[,1:24] <- data.frame(scale(Home_Audio_data[,1:24], center = TRUE))


# ## Treatement of zero values
# Home_Audio_data$TV[which(Home_Audio_data$TV==0.0)] <- 0.01
# Home_Audio_data$Radio[which(Home_Audio_data$Radio==0)] <- 0.01
# Home_Audio_data$Content_Marketing[which(Home_Audio_data$Content_Marketing==0)] <- 0.01
# Home_Audio_data$Percent[which(Home_Audio_data$Percent==0)] <- 0.01
# Home_Audio_data$discount[which(Home_Audio_data$discount==0)] <- 0.01
# 
# #logn of all the variables
# Home_Audio_data <- data.frame(log(abs(Home_Audio_data[,1:24])))
# 
# #removing below columns because it has infinite values
# Home_Audio_data <- Home_Audio_data[,-c(9,13:15)]

Linearmodel <- lm(gmv~.,data=train)

summary(Linearmodel)

Linearmodel2 <- stepAIC(Linearmodel,direction = "both")
summary(Linearmodel2)
vif(Linearmodel2)

Linearmodel3 <- lm(formula = gmv ~ weeknumber + product_mrp + units + discount + NPS + Percent + GamePad + GamingAccessoryKit + GamingAdapter + GamingHeadset + GamingKeyboard + GamingMemoryCard + GamingMouse + GamingMousePad + JoystickGamingWheel + MotionController + TVOutCableAccessory + PaymentType + IsmoreListPrice + TV + Sponsorship + Content_Marketing + Online_marketing + Affiliates, data = train)


trainindices <- sample(1:nrow(Home_Audio_data),0.6*nrow(Home_Audio_data))
#training data set
train = Home_Audio_data[trainindices,]
# testing data set
test = Home_Audio_data[-trainindices,]

Linearmodel <- lm(gmv~.,data=train)

summary(Linearmodel)

Linearmodel2 <- stepAIC(Linearmodel,direction = "both")
summary(Linearmodel2)
vif(Linearmodel2)

Linearmodel3 <- lm(formula = gmv ~ weeknumber + product_mrp + units + discount + NPS + FMRadio + VoiceRecorder + PaymentType + isSaleday + TV + Digital + Sponsorship + Content_Marketing + Online_marketing + 
                             Affiliates + SEM, data = train)
                             
                             
summary(Linearmodel3)
vif(Linearmodel3)

# removing FMRadio 
Linearmodel4 <- lm(formula = gmv ~ weeknumber + product_mrp + units + discount + NPS +  VoiceRecorder + PaymentType + isSaleday + TV + Digital + Sponsorship + Content_Marketing + Online_marketing + 
                             Affiliates + SEM, data = train)
summary(Linearmodel4)
vif(Linearmodel4)

# removing TV 
Linearmodel5 <-  lm(formula = gmv ~ weeknumber + product_mrp + units + discount + NPS +  VoiceRecorder + PaymentType + isSaleday + Digital + Sponsorship + Content_Marketing + Online_marketing + 
                              Affiliates + SEM, data = train)
summary(Linearmodel5)
vif(Linearmodel5)

# removing Sponsorship 
Linearmodel6 <- lm(formula = gmv ~ weeknumber + product_mrp + units + discount + NPS +  VoiceRecorder + PaymentType + isSaleday + Digital + Content_Marketing + Online_marketing + 
                             Affiliates + SEM, data = train)
summary(Linearmodel6)
vif(Linearmodel6)

# removing VoiceRecorder  
Linearmodel7 <- lm(formula = gmv ~ weeknumber + product_mrp + units + discount + NPS + PaymentType + isSaleday + Digital + Content_Marketing + Online_marketing + 
                             Affiliates + SEM, data = train)
summary(Linearmodel7)
vif(Linearmodel7)

# removing Online_marketing 
Linearmodel8 <-lm(formula = gmv ~ weeknumber + product_mrp + units + discount + NPS + PaymentType + isSaleday + Digital + Content_Marketing + Affiliates + SEM, data = train)

summary(Linearmodel8)
vif(Linearmodel8)

# removing PaymentType 
Linearmodel9 <- lm(formula = gmv ~ weeknumber + product_mrp + units + discount + NPS + isSaleday + Digital + Content_Marketing + Affiliates + SEM, data = train)

summary(Linearmodel9)
vif(Linearmodel9)

# removing NPS
Linearmodel10 <- lm(formula = gmv ~ weeknumber + product_mrp + units + discount + isSaleday + Digital + Content_Marketing + Affiliates + SEM, data = train)

summary(Linearmodel10)
vif(Linearmodel10)

# removing  Digital
Linearmodel11 <- lm(formula = gmv ~ weeknumber + product_mrp + units + discount + isSaleday + Content_Marketing + Affiliates + SEM, data = train)

summary(Linearmodel11)
vif(Linearmodel11)

# removing  Content_Marketing
Linearmodel12 <- lm(formula = gmv ~ weeknumber + product_mrp + units + discount + isSaleday + Affiliates + SEM, data = train)

summary(Linearmodel12)
vif(Linearmodel12)

# removing  isSaleday
Linearmodel13 <- lm(formula = gmv ~ weeknumber + product_mrp + units + discount + Affiliates + SEM, data = train)

summary(Linearmodel13)
vif(Linearmodel13)

# removing  discount
Linearmodel14 <- lm(formula = gmv ~ weeknumber + product_mrp + units + Affiliates + SEM, data = train)

summary(Linearmodel14)
vif(Linearmodel14)

predictedVal <- predict(Linearmodel14,test)
test$predicted_gmv <- predictedVal


# testing of r between actual and predicted 
Home_Audio_r <- cor(test$predicted_gmv,test$gmv)
Home_Audio_r
# testing of r sqare between actual and predicted 
Home_Audio_rsquare <- cor(test$predicted_gmv,test$gmv)^2
Home_Audio_rsquare

Linear_model <- Linearmodel14


elasticity_Funtion <- function(parameter) {
  x <- as.numeric(Linear_model$coefficients[parameter])
  return(x)
}

Home_Audio_list <- list()

for(i in 2:length(Linear_model$coefficients)) {
  Home_Audio_list[i-1] <- elasticity_Funtion(names(Linear_model$coefficients)[i])
}

Home_Audio_Elasticity <- data.frame(names(Linear_model$coefficients[2:length(Linear_model$coefficients)]))
Home_Audio_Elasticity <- cbind(Home_Audio_Elasticity,do.call(rbind.data.frame, Home_Audio_list))
colnames(Home_Audio_Elasticity) <- c("Attributes","ElasticityValue")

Home_Audio_Elasticity$direction <- ifelse(Home_Audio_Elasticity$ElasticityValue > 0, "Positive", "Negative")

ggplot(data=Home_Audio_Elasticity, aes(x=reorder(Attributes,ElasticityValue),y=ElasticityValue)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Home Audio - Linear Model") +xlab("Attributes")


ggplot(test, aes(gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))


