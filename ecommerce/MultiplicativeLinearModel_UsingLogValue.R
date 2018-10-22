###############################################################################################
###############################################################################################
###############################################################################################
## Multiplicative Model CAMERA ACCESSORY
###############################################################################################


camera_accessory_data  <- capstonewithoutna_withinperiod_Filter_grpbyOdate_media_CA 

colnames(camera_accessory_data)
summary(camera_accessory_data)
str(camera_accessory_data)
camera_accessory_data <- camera_accessory_data[,-c(2,8)]

## Treatement of zero values
camera_accessory_data$TV[which(camera_accessory_data$TV==0.0)] <- 0.01
camera_accessory_data$Radio[which(camera_accessory_data$Radio==0)] <- 0.01
camera_accessory_data$Content_Marketing[which(camera_accessory_data$Content_Marketing==0)] <- 0.01
camera_accessory_data$Percent[which(camera_accessory_data$Percent==0)] <- 0.01
camera_accessory_data$Teleconverter[which(camera_accessory_data$Teleconvertere==0.0)] <- 0.01
camera_accessory_data$Other[which(camera_accessory_data$Other==0)] <- 0.01
camera_accessory_data$Softbox[which(camera_accessory_data$Softbox==0)] <- 0.01
camera_accessory_data$discount[which(camera_accessory_data$discount==0)] <- 0.01
camera_accessory_data$CameraEyeCup[which(camera_accessory_data$CameraEyeCup==0)] <- 0.01
camera_accessory_data$CameraFilmRolls[which(camera_accessory_data$CameraFilmRolls==0)] <- 0.01
camera_accessory_data$CameraHousing[which(camera_accessory_data$CameraHousing==0)] <- 0.01
camera_accessory_data$CameraMicrophone[which(camera_accessory_data$CameraMicrophone==0)] <- 0.01
camera_accessory_data$ReflectorUmbrella[which(camera_accessory_data$ReflectorUmbrella==0)] <- 0.01
camera_accessory_data$Teleconverter[which(camera_accessory_data$Teleconverter==0)] <- 0.01

#log of all the variables
camera_accessory_data <- data.frame(log(abs(camera_accessory_data[,1:23])))
camera_accessory_data[,1:23] <- data.frame(scale(camera_accessory_data[,1:23], center = TRUE))
#removing below columns because it has infinite values
camera_accessory_data <- camera_accessory_data[,-c(8:12,17,18,20:23)]

trainindices <- sample(1:nrow(camera_accessory_data),0.6*nrow(camera_accessory_data))
#training data set
train = camera_accessory_data[trainindices,]
# testing data set
test = camera_accessory_data[-trainindices,]

str(train)


multiplicativemodel <- lm(gmv~.,data=train)

summary(multiplicativemodel)

multiplicativemodel2 <- stepAIC(multiplicativemodel,direction = "both")
summary(multiplicativemodel2)
vif(multiplicativemodel2)

multiplicativemodel3 <- lm(gmv ~ product_mrp + units + discount + NPS + Percent + CameraEyeCup + CameraFilmRolls + CameraHousing, data=train)
summary(multiplicativemodel3)
vif(multiplicativemodel3)

# removing NPS
multiplicativemodel4 <- lm(gmv ~ product_mrp + units + discount  + Percent + CameraEyeCup + CameraFilmRolls + CameraHousing, data=train)
summary(multiplicativemodel4)
vif(multiplicativemodel4)

# removing   CameraFilmRolls
multiplicativemodel5 <- lm(gmv ~ product_mrp + units + discount  + Percent + CameraEyeCup + CameraHousing, data=train)
summary(multiplicativemodel5)
vif(multiplicativemodel5)


# removing  CameraHousing
multiplicativemodel6 <-  lm(gmv ~ product_mrp + units + discount  + Percent + CameraEyeCup , data=train)
summary(multiplicativemodel6)
vif(multiplicativemodel6)

predictedVal <- predict(multiplicativemodel6,test)
test$predicted_gmv <- predictedVal

# testing of r sqare between actual and predicted 
cameraAccessory_r <- cor(test$predicted_gmv,test$gmv)^2
cameraAccessory_r

multiplicative_model <- multiplicativemodel6

elasticity_Funtion <- function(parameter) {
  x <- as.numeric(multiplicative_model$coefficients[parameter])
  return(x)
}

cameraAccessory_list <- list()

for(i in 2:length(multiplicative_model$coefficients)) {
  cameraAccessory_list[i-1] <- elasticity_Funtion(names(multiplicative_model$coefficients)[i])
}

cameraAccessory_Elasticity <- data.frame(names(multiplicative_model$coefficients[2:length(multiplicative_model$coefficients)]))
cameraAccessory_Elasticity <- cbind(cameraAccessory_Elasticity,do.call(rbind.data.frame, cameraAccessory_list))
colnames(cameraAccessory_Elasticity) <- c("Attributes","ElasticityValue")

cameraAccessory_Elasticity$direction <- ifelse(cameraAccessory_Elasticity$ElasticityValue > 0, "Positive", "Negative")

ggplot(data=cameraAccessory_Elasticity, aes(x=reorder(Attributes,ElasticityValue),y=ElasticityValue)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("cameraAccessory - Multiplicative Model") +xlab("Attributes")


ggplot(test, aes(gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))

###############################################################################################
## Multiplicative Model Gaming Accessories
###############################################################################################


GA_data <- capstonewithoutna_withinperiod_Filter_grpbyOdate_media_GA

colnames(GA_data)
summary(GA_data)
str(GA_data)
GA_data <- GA_data[,-c(2,8)]

## Treatement of zero values
GA_data$TV[which(GA_data$TV==0.0)] <- 0.01
GA_data$Radio[which(GA_data$Radio==0)] <- 0.01
GA_data$Content_Marketing[which(GA_data$Content_Marketing==0)] <- 0.01
GA_data$Percent[which(GA_data$Percent==0)] <- 0.01
GA_data$Other[which(GA_data$Other==0)] <- 0.01
GA_data$discount[which(GA_data$discount==0)] <- 0.01
GA_data$GameControlMount[which(GA_data$GameControlMount==0)] <- 0.01
GA_data$GamingChargingStation[which(GA_data$GamingChargingStation==0)] <- 0.01
GA_data$GamingSpeaker[which(GA_data$GamingSpeaker==0)] <- 0.01

#log of all the variables
GA_data <- data.frame(log(abs(GA_data[,1:23])))
#removing below columns because it has infinite values
GA_data <- GA_data[,-c(10,11,13,14:17,19,20,21)]

trainindices <- sample(1:nrow(GA_data),0.6*nrow(GA_data))
#training data set
train = GA_data[trainindices,]
# testing data set
test = GA_data[-trainindices,]

multiplicativemodel <- lm(gmv~.,data=train)

summary(multiplicativemodel)

multiplicativemodel2 <- stepAIC(multiplicativemodel,direction = "both")
summary(multiplicativemodel2)
vif(multiplicativemodel2)

multiplicativemodel3 <- lm(formula = gmv ~ weeknumber + product_mrp + discount + NPS + GameControlMount + IsmoreListPrice, data = train)

summary(multiplicativemodel3)
vif(multiplicativemodel3)

# removing GameControlMount 
multiplicativemodel4 <- lm(formula = gmv ~ weeknumber + product_mrp + discount + NPS +  IsmoreListPrice, data = train)

summary(multiplicativemodel4)
vif(multiplicativemodel4)

predictedVal <- predict(multiplicativemodel4,test)
test$predicted_gmv <- predictedVal

# testing of r sqare between actual and predicted 
GamingAccessory_r <- cor(test$predicted_gmv,test$gmv)^2
GamingAccessory_r

multiplicative_model <- multiplicativemodel4

elasticity_Funtion <- function(parameter) {
  x <- as.numeric(multiplicative_model$coefficients[parameter])
  return(x)
}

GamingAccessory_list <- list()

for(i in 2:length(multiplicative_model$coefficients)) {
  GamingAccessory_list[i-1] <- elasticity_Funtion(names(multiplicative_model$coefficients)[i])
}

GamingAccessory_Elasticity <- data.frame(names(multiplicative_model$coefficients[2:length(multiplicative_model$coefficients)]))
GamingAccessory_Elasticity <- cbind(GamingAccessory_Elasticity,do.call(rbind.data.frame, GamingAccessory_list))
colnames(GamingAccessory_Elasticity) <- c("Attributes","ElasticityValue")

GamingAccessory_Elasticity$direction <- ifelse(GamingAccessory_Elasticity$ElasticityValue > 0, "Positive", "Negative")

ggplot(data=GamingAccessory_Elasticity, aes(x=reorder(Attributes,ElasticityValue),y=ElasticityValue)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("GamingAccessory - Multiplicative Model") +xlab("Attributes")


ggplot(test, aes(gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))

###############################################################################################
## Multiplicative Model HOME AUDIO
###############################################################################################


Home_Audio_data <- capstonewithoutna_withinperiod_Filter_grpbyOdate_media_HA

colnames(Home_Audio_data)
summary(Home_Audio_data)
str(Home_Audio_data)
Home_Audio_data <- Home_Audio_data[,-c(2,8)]

## Treatement of zero values
Home_Audio_data$TV[which(Home_Audio_data$TV==0.0)] <- 0.01
Home_Audio_data$Radio[which(Home_Audio_data$Radio==0)] <- 0.01
Home_Audio_data$Content_Marketing[which(Home_Audio_data$Content_Marketing==0)] <- 0.01
Home_Audio_data$Percent[which(Home_Audio_data$Percent==0)] <- 0.01
Home_Audio_data$discount[which(Home_Audio_data$discount==0)] <- 0.01

#logn of all the variables
Home_Audio_data <- data.frame(log(abs(Home_Audio_data[,1:23])))

#removing below columns because it has infinite values
Home_Audio_data <- Home_Audio_data[,-c(9,13:15)]

trainindices <- sample(1:nrow(Home_Audio_data),0.6*nrow(Home_Audio_data))
#training data set
train = Home_Audio_data[trainindices,]
# testing data set
test = Home_Audio_data[-trainindices,]

multiplicativemodel <- lm(gmv~.,data=train)

summary(multiplicativemodel)

multiplicativemodel2 <- stepAIC(multiplicativemodel,direction = "both")
summary(multiplicativemodel2)
vif(multiplicativemodel2)

multiplicativemodel3 <- lm(formula = gmv ~ product_mrp + units + discount + NPS + Percent + Dock + HiFiSystem + VoiceRecorder + PaymentType + isSaleday + TV + Digital + Sponsorship, data = train)

summary(multiplicativemodel3)
vif(multiplicativemodel3)

# removing HiFiSystem 
multiplicativemodel4 <- lm(formula = gmv ~ product_mrp + units + discount + NPS + Percent + Dock +  VoiceRecorder + PaymentType + isSaleday + TV + Digital + Sponsorship, data = train)

summary(multiplicativemodel4)
vif(multiplicativemodel4)

# removing Digital 
multiplicativemodel5 <-  lm(formula = gmv ~ product_mrp + units + discount + NPS + Percent + Dock +  VoiceRecorder + PaymentType + isSaleday + TV +  Sponsorship, data = train)

summary(multiplicativemodel5)
vif(multiplicativemodel5)

# removing TV 
multiplicativemodel6 <- lm(formula = gmv ~ product_mrp + units + discount + NPS + Percent + Dock +  VoiceRecorder + PaymentType + isSaleday +   Sponsorship, data = train)

summary(multiplicativemodel6)
vif(multiplicativemodel6)

# removing NPS  
multiplicativemodel7 <- lm(formula = gmv ~ product_mrp + units + discount + Percent + Dock +  VoiceRecorder + PaymentType + isSaleday +   Sponsorship, data = train)

summary(multiplicativemodel7)
vif(multiplicativemodel7)

# removing isSaleday 
multiplicativemodel8 <-lm(formula = gmv ~ product_mrp + units + discount + Percent + Dock +  VoiceRecorder + PaymentType +    Sponsorship, data = train)

summary(multiplicativemodel8)
vif(multiplicativemodel8)

# removing Percent 
multiplicativemodel9 <- lm(formula = gmv ~ product_mrp + units + discount +  Dock +  VoiceRecorder + PaymentType +    Sponsorship, data = train)

summary(multiplicativemodel9)
vif(multiplicativemodel9)

# removing sponshership
multiplicativemodel10 <- lm(formula = gmv ~ product_mrp + units + discount +  Dock +  VoiceRecorder + PaymentType , data = train)

summary(multiplicativemodel10)
vif(multiplicativemodel10)

# removing  PaymentType
multiplicativemodel11 <- lm(formula = gmv ~ product_mrp + units + discount +  Dock +  VoiceRecorder  , data = train)

summary(multiplicativemodel11)
vif(multiplicativemodel11)


predictedVal <- predict(multiplicativemodel11,test)
test$predicted_gmv <- predictedVal

# testing of r sqare between actual and predicted 
Home_Audio_r <- cor(test$predicted_gmv,test$gmv)^2
Home_Audio_r

multiplicative_model <- multiplicativemodel11

elasticity_Funtion <- function(parameter) {
  x <- as.numeric(multiplicative_model$coefficients[parameter])
  return(x)
}

Home_Audio_list <- list()

for(i in 2:length(multiplicative_model$coefficients)) {
  Home_Audio_list[i-1] <- elasticity_Funtion(names(multiplicative_model$coefficients)[i])
}

Home_Audio_Elasticity <- data.frame(names(multiplicative_model$coefficients[2:length(multiplicative_model$coefficients)]))
Home_Audio_Elasticity <- cbind(Home_Audio_Elasticity,do.call(rbind.data.frame, Home_Audio_list))
colnames(Home_Audio_Elasticity) <- c("Attributes","ElasticityValue")

Home_Audio_Elasticity$direction <- ifelse(Home_Audio_Elasticity$ElasticityValue > 0, "Positive", "Negative")

ggplot(data=Home_Audio_Elasticity, aes(x=reorder(Attributes,ElasticityValue),y=ElasticityValue)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Home Audio - Multiplicative Model") +xlab("Attributes")


ggplot(test, aes(gmv)) + geom_line(aes(y = test$gmv, colour = "actual GMV")) + geom_line(aes(y = test$predicted_gmv, colour = "test GMV"))


