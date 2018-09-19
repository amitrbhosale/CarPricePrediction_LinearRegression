#install.packages("car")
#install.packages("MASS")
# Load the required libraries
library("stringr")
library("tidyr")
library("dplyr")
library("car")
library("MASS")

# Set the working directory to the relevant location.
#setwd("D:/IIITB/Predictive Analytics - Case Study")
CarPrice <- read.csv("CarPrice_Assignment.csv", stringsAsFactors = FALSE)

View(CarPrice)

nrow(CarPrice) # 205 Observations
ncol(CarPrice) # 26 variables
# Split the character string CarName into two parts with a seperator as " "
list <- data.frame(str_split(CarPrice$CarName,pattern =  " ",simplify = TRUE,n=2))
colnames(list) <- c("car company","car model")

CarPrice <- cbind(CarPrice,list)

#Removing the old Column
CarPrice$CarName <- NULL 

#Convert to upper case

CarPrice$`car company` <- toupper(CarPrice$`car company`)

# Convert VW and VOKSWAGEN to VOLKSWAGEN
CarPrice$`car company` <- str_replace_all(CarPrice$`car company`,"VW","VOLKSWAGEN")
CarPrice$`car company` <- str_replace_all(CarPrice$`car company`,"VOKSWAGEN","VOLKSWAGEN")

# Convert MAXDA to MAZDA
CarPrice$`car company` <- str_replace_all(CarPrice$`car company`,"MAXDA","MAZDA")

# Convert PORCSHCE to PORSCHE
CarPrice$`car company` <- str_replace_all(CarPrice$`car company`,"PORCSHCE","PORSCHE")

# Convert TOYOUTA to TOYOTA
CarPrice$`car company` <- str_replace_all(CarPrice$`car company`,"TOYOUTA","TOYOTA")


#Convert to Factor
CarPrice$`car company` <- as.factor(CarPrice$`car company`)
summary(CarPrice$`car company`)

# Convert Categorical variables to factors
CarPrice$symboling <- as.factor(CarPrice$symboling)
CarPrice$fueltype <- as.factor(CarPrice$fueltype)
CarPrice$aspiration <- as.factor(CarPrice$aspiration)
CarPrice$doornumber <- as.factor(CarPrice$doornumber)
CarPrice$carbody <- as.factor(CarPrice$carbody)
CarPrice$drivewheel <- as.factor(CarPrice$drivewheel)
CarPrice$enginelocation <- as.factor(CarPrice$enginelocation)
CarPrice$enginetype <- as.factor(CarPrice$enginetype)
CarPrice$cylindernumber <- as.factor(CarPrice$cylindernumber)
CarPrice$fuelsystem <- as.factor(CarPrice$fuelsystem)

summary(CarPrice$fuelsystem)
#Remove Car Model
CarPrice$`car model` <- NULL


CarPrice$car_ID <- NULL # Removing the column as it is a normal serial number for records
str(CarPrice)

# Converting the two level factors to levels as 1 and 0
levels(CarPrice$fueltype) <- c(1,0)
CarPrice$fueltype <- as.numeric(levels(CarPrice$fueltype))[CarPrice$fueltype]

levels(CarPrice$aspiration) <- c(1,0)
CarPrice$aspiration <- as.numeric(levels(CarPrice$aspiration))[CarPrice$aspiration]

levels(CarPrice$doornumber) <- c(1,0)
CarPrice$doornumber <- as.numeric(levels(CarPrice$doornumber))[CarPrice$doornumber]

levels(CarPrice$enginelocation) <- c(1,0)
CarPrice$enginelocation <- as.numeric(levels(CarPrice$enginelocation))[CarPrice$enginelocation]

str(CarPrice)

# Create the dummy variable for symboling variable
dummy_1 <- data.frame(model.matrix( ~symboling, data = CarPrice))
#View(dummy_1)
dummy_1 <- dummy_1[,-1]

# Combine the dummy variables and the numeric columns of CarPrice dataset, in a new dataset called CarPrice_1
CarPrice_1 <- cbind(CarPrice[,-1], dummy_1)
View(CarPrice_1)

str(CarPrice_1)
# Create the dummy variable for carbody variable
dummy_2 <- data.frame(model.matrix( ~carbody, data = CarPrice_1))
#View(dummy_2)
dummy_2 <- dummy_2[,-1]

# Combine the dummy variables and the numeric columns of CarPrice_1 dataset, in a new dataset called CarPrice_2
CarPrice_2 <- cbind(CarPrice_1[,-4], dummy_2)
View(CarPrice_2)

str(CarPrice_2)
# Create the dummy variable for drivewheel variable
dummy_3 <- data.frame(model.matrix( ~drivewheel, data = CarPrice_2))
#View(dummy_3)
dummy_3 <- dummy_3[,-1]

# Combine the dummy variables and the numeric columns of CarPrice_2 dataset, in a new dataset called CarPrice_3
CarPrice_3 <- cbind(CarPrice_2[,-4], dummy_3)
View(CarPrice_3)

str(CarPrice_3)
# Create the dummy variable for enginetype variable
dummy_4 <- data.frame(model.matrix( ~enginetype, data = CarPrice_3))
#View(dummy_4)
dummy_4 <- dummy_4[,-1]

# Combine the dummy variables and the numeric columns of CarPrice_3 dataset, in a new dataset called CarPrice_4
CarPrice_4 <- cbind(CarPrice_3[,-10], dummy_4)
View(CarPrice_4)

str(CarPrice_4)
# Create the dummy variable for cylindernumber variable
dummy_5 <- data.frame(model.matrix( ~cylindernumber, data = CarPrice_4))
#View(dummy_5)
dummy_5 <- dummy_5[,-1]

# Combine the dummy variables and the numeric columns of CarPrice_4 dataset, in a new dataset called CarPrice_5
CarPrice_5 <- cbind(CarPrice_4[,-10], dummy_5)
View(CarPrice_5)

str(CarPrice_5)
# Create the dummy variable for fuelsystem variable
dummy_6 <- data.frame(model.matrix( ~fuelsystem, data = CarPrice_5))
#View(dummy_6)
dummy_6 <- dummy_6[,-1]

# Combine the dummy variables and the numeric columns of CarPrice_5 dataset, in a new dataset called CarPrice_6
CarPrice_6 <- cbind(CarPrice_5[,-11], dummy_6)
View(CarPrice_6)

str(CarPrice_6)
# Create the dummy variable for car cpmpany variable
dummy_7 <- data.frame(model.matrix( ~`car company`, data = CarPrice_6))
#View(dummy_7)
dummy_7 <- dummy_7[,-1]

# Combine the dummy variables and the numeric columns of CarPrice_6 dataset, in a new dataset called CarPrice_7
CarPrice_7 <- cbind(CarPrice_6[,-19], dummy_7)
View(CarPrice_7)

# New variable Power to wt. ratio, taking the count to 70 variables
CarPrice_7$powertowt <- round(CarPrice_7$horsepower / CarPrice_7$curbweight,2 )

str(CarPrice_7)
cormat <- cor(CarPrice_7)
View(cormat)
write.csv(cormat,file = "cormat.csv",row.names = TRUE)

# separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(CarPrice_7), 0.7*nrow(CarPrice_7))
train = CarPrice_7[trainindices,]
test = CarPrice_7[-trainindices,]

model_1 <- lm(price~.,data = train)
summary(model_1)


step <- stepAIC(model_1, direction="both")

# Using the model attributes as per StepAIC recommendation.
model_2 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
                enginesize + boreratio + stroke + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                enginetyperotor + cylindernumberfour + cylindernumberthree + 
                fuelsystem2bbl + fuelsystemmpfi + X.car.company.BMW + X.car.company.BUICK + 
                X.car.company.DODGE + X.car.company.HONDA + X.car.company.JAGUAR + 
                X.car.company.MAZDA + X.car.company.MITSUBISHI + X.car.company.NISSAN + 
                X.car.company.PLYMOUTH + X.car.company.RENAULT + X.car.company.SAAB + 
                X.car.company.TOYOTA + X.car.company.VOLKSWAGEN + powertowt + 
                symboling1, data = train)

summary(model_2)
vif(model_2)

# Removing column X.car.company.SAAB due to high p value and high VIF

model_3 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
                enginesize + boreratio + stroke + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                enginetyperotor + cylindernumberfour + cylindernumberthree + 
                fuelsystem2bbl + fuelsystemmpfi + X.car.company.BMW + X.car.company.BUICK + 
                X.car.company.DODGE + X.car.company.HONDA + X.car.company.JAGUAR + 
                X.car.company.MAZDA + X.car.company.MITSUBISHI + X.car.company.NISSAN + 
                X.car.company.PLYMOUTH + X.car.company.RENAULT + 
                X.car.company.TOYOTA + X.car.company.VOLKSWAGEN + powertowt + 
                symboling1, data = train)

summary(model_3)
vif(model_3)


# Removing stroke for high p value and high VIF values

model_4 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
                enginesize + boreratio + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                enginetyperotor + cylindernumberfour + cylindernumberthree + 
                fuelsystem2bbl + fuelsystemmpfi + X.car.company.BMW + X.car.company.BUICK + 
                X.car.company.DODGE + X.car.company.HONDA + X.car.company.JAGUAR + 
                X.car.company.MAZDA + X.car.company.MITSUBISHI + X.car.company.NISSAN + 
                X.car.company.PLYMOUTH + X.car.company.RENAULT + 
                X.car.company.TOYOTA + X.car.company.VOLKSWAGEN + powertowt + 
                symboling1, data = train)

summary(model_4)
vif(model_4)

# Removing X.car.company.JAGUAR due to high p value

model_5 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
                enginesize + boreratio + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                enginetyperotor + cylindernumberfour + cylindernumberthree + 
                fuelsystem2bbl + fuelsystemmpfi + X.car.company.BMW + X.car.company.BUICK + 
                X.car.company.DODGE + X.car.company.HONDA + 
                X.car.company.MAZDA + X.car.company.MITSUBISHI + X.car.company.NISSAN + 
                X.car.company.PLYMOUTH + X.car.company.RENAULT + 
                X.car.company.TOYOTA + X.car.company.VOLKSWAGEN + powertowt + 
                symboling1, data = train)

summary(model_5)
vif(model_5)

# Removing curbweight due to high VIF value
model_6 <- lm(price ~ aspiration + enginelocation + carwidth +  
                enginesize + boreratio + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                enginetyperotor + cylindernumberfour + cylindernumberthree + 
                fuelsystem2bbl + fuelsystemmpfi + X.car.company.BMW + X.car.company.BUICK + 
                X.car.company.DODGE + X.car.company.HONDA + 
                X.car.company.MAZDA + X.car.company.MITSUBISHI + X.car.company.NISSAN + 
                X.car.company.PLYMOUTH + X.car.company.RENAULT + 
                X.car.company.TOYOTA + X.car.company.VOLKSWAGEN + powertowt + 
                symboling1, data = train)

summary(model_6)
vif(model_6)

# Remove Symboling1 due to high p value

model_7 <- lm(price ~ aspiration + enginelocation + carwidth +  
                enginesize + boreratio + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                enginetyperotor + cylindernumberfour + cylindernumberthree + 
                fuelsystem2bbl + fuelsystemmpfi + X.car.company.BMW + X.car.company.BUICK + 
                X.car.company.DODGE + X.car.company.HONDA + 
                X.car.company.MAZDA + X.car.company.MITSUBISHI + X.car.company.NISSAN + 
                X.car.company.PLYMOUTH + X.car.company.RENAULT + 
                X.car.company.TOYOTA + X.car.company.VOLKSWAGEN + powertowt, data = train)

summary(model_7)
vif(model_7)


# Removing carbodywagon due to high VIF
model_8 <- lm(price ~ aspiration + enginelocation + carwidth +  
                enginesize + boreratio + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + drivewheelrwd + 
                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                enginetyperotor + cylindernumberfour + cylindernumberthree + 
                fuelsystem2bbl + fuelsystemmpfi + X.car.company.BMW + X.car.company.BUICK + 
                X.car.company.DODGE + X.car.company.HONDA + 
                X.car.company.MAZDA + X.car.company.MITSUBISHI + X.car.company.NISSAN + 
                X.car.company.PLYMOUTH + X.car.company.RENAULT + 
                X.car.company.TOYOTA + X.car.company.VOLKSWAGEN + powertowt, data = train)

summary(model_8)
vif(model_8)

# Removing carbodyhardtop due to high p value
model_9 <- lm(price ~ aspiration + enginelocation + carwidth +  
                enginesize + boreratio + peakrpm +  
                carbodyhatchback + carbodysedan + drivewheelrwd + 
                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                enginetyperotor + cylindernumberfour + cylindernumberthree + 
                fuelsystem2bbl + fuelsystemmpfi + X.car.company.BMW + X.car.company.BUICK + 
                X.car.company.DODGE + X.car.company.HONDA + 
                X.car.company.MAZDA + X.car.company.MITSUBISHI + X.car.company.NISSAN + 
                X.car.company.PLYMOUTH + X.car.company.RENAULT + 
                X.car.company.TOYOTA + X.car.company.VOLKSWAGEN + powertowt, data = train)

summary(model_9)
vif(model_9)

# Removing carbodysedan due to high p value

model_10 <- lm(price ~ aspiration + enginelocation + carwidth +  
                enginesize + boreratio + peakrpm +  
                carbodyhatchback + drivewheelrwd + 
                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                enginetyperotor + cylindernumberfour + cylindernumberthree + 
                fuelsystem2bbl + fuelsystemmpfi + X.car.company.BMW + X.car.company.BUICK + 
                X.car.company.DODGE + X.car.company.HONDA + 
                X.car.company.MAZDA + X.car.company.MITSUBISHI + X.car.company.NISSAN + 
                X.car.company.PLYMOUTH + X.car.company.RENAULT + 
                X.car.company.TOYOTA + X.car.company.VOLKSWAGEN + powertowt, data = train)

summary(model_10)
vif(model_10)

# Removing carbodyhatchback due to high p value
model_11 <- lm(price ~ aspiration + enginelocation + carwidth +  
                 enginesize + boreratio + peakrpm +  
                 drivewheelrwd + 
                 enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                 enginetyperotor + cylindernumberfour + cylindernumberthree + 
                 fuelsystem2bbl + fuelsystemmpfi + X.car.company.BMW + X.car.company.BUICK + 
                 X.car.company.DODGE + X.car.company.HONDA + 
                 X.car.company.MAZDA + X.car.company.MITSUBISHI + X.car.company.NISSAN + 
                 X.car.company.PLYMOUTH + X.car.company.RENAULT + 
                 X.car.company.TOYOTA + X.car.company.VOLKSWAGEN + powertowt, data = train)

summary(model_11)
vif(model_11)

# Removing X.car.company.HONDA from the model due to high VIF
model_12 <- lm(price ~ aspiration + enginelocation + carwidth +  
                 enginesize + boreratio + peakrpm +  
                 drivewheelrwd + 
                 enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                 enginetyperotor + cylindernumberfour + cylindernumberthree + 
                 fuelsystem2bbl + fuelsystemmpfi + X.car.company.BMW + X.car.company.BUICK + 
                 X.car.company.DODGE +  
                 X.car.company.MAZDA + X.car.company.MITSUBISHI + X.car.company.NISSAN + 
                 X.car.company.PLYMOUTH + X.car.company.RENAULT + 
                 X.car.company.TOYOTA + X.car.company.VOLKSWAGEN + powertowt, data = train)

summary(model_12)
vif(model_12)

#Removing boreratio due to high VIF
model_13 <- lm(price ~ aspiration + enginelocation + carwidth +  
                 enginesize + peakrpm +  
                 drivewheelrwd + 
                 enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                 enginetyperotor + cylindernumberfour + cylindernumberthree + 
                 fuelsystem2bbl + fuelsystemmpfi + X.car.company.BMW + X.car.company.BUICK + 
                 X.car.company.DODGE +  
                 X.car.company.MAZDA + X.car.company.MITSUBISHI + X.car.company.NISSAN + 
                 X.car.company.PLYMOUTH + X.car.company.RENAULT + 
                 X.car.company.TOYOTA + X.car.company.VOLKSWAGEN + powertowt, data = train)

summary(model_13)
vif(model_13)

# Removing cylindernumberfour due to high p value
model_14 <- lm(price ~ aspiration + enginelocation + carwidth +  
                 enginesize + peakrpm +  
                 drivewheelrwd + 
                 enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                 enginetyperotor + cylindernumberthree + 
                 fuelsystem2bbl + fuelsystemmpfi + X.car.company.BMW + X.car.company.BUICK + 
                 X.car.company.DODGE +  
                 X.car.company.MAZDA + X.car.company.MITSUBISHI + X.car.company.NISSAN + 
                 X.car.company.PLYMOUTH + X.car.company.RENAULT + 
                 X.car.company.TOYOTA + X.car.company.VOLKSWAGEN + powertowt, data = train)

summary(model_14)
vif(model_14)

# Removing cylindernumberthree due to high p value
model_15 <- lm(price ~ aspiration + enginelocation + carwidth +  
                 enginesize + peakrpm +  
                 drivewheelrwd + 
                 enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                 enginetyperotor +  
                 fuelsystem2bbl + fuelsystemmpfi + X.car.company.BMW + X.car.company.BUICK + 
                 X.car.company.DODGE +  
                 X.car.company.MAZDA + X.car.company.MITSUBISHI + X.car.company.NISSAN + 
                 X.car.company.PLYMOUTH + X.car.company.RENAULT + 
                 X.car.company.TOYOTA + X.car.company.VOLKSWAGEN + powertowt, data = train)

summary(model_15)
vif(model_15)

#Removing peakrpm due to high VIF
model_16 <- lm(price ~ aspiration + enginelocation + carwidth +  
                 enginesize + drivewheelrwd + 
                 enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                 enginetyperotor +  
                 fuelsystem2bbl + fuelsystemmpfi + X.car.company.BMW + X.car.company.BUICK + 
                 X.car.company.DODGE +  
                 X.car.company.MAZDA + X.car.company.MITSUBISHI + X.car.company.NISSAN + 
                 X.car.company.PLYMOUTH + X.car.company.RENAULT + 
                 X.car.company.TOYOTA + X.car.company.VOLKSWAGEN + powertowt, data = train)

summary(model_16)
vif(model_16)

# Removing powertowt due to high VIF
model_17 <- lm(price ~ aspiration + enginelocation + carwidth +  
                 enginesize + drivewheelrwd + 
                 enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
                 enginetyperotor +  
                 fuelsystem2bbl + fuelsystemmpfi + X.car.company.BMW + X.car.company.BUICK + 
                 X.car.company.DODGE +  
                 X.car.company.MAZDA + X.car.company.MITSUBISHI + X.car.company.NISSAN + 
                 X.car.company.PLYMOUTH + X.car.company.RENAULT + 
                 X.car.company.TOYOTA + X.car.company.VOLKSWAGEN, data = train)

summary(model_17)
vif(model_17)

# Removing enginetypedohcv as it has higher p value compared to others
model_18 <- lm(price ~ aspiration + enginelocation + carwidth +  
                 enginesize + drivewheelrwd + 
                 enginetypel + enginetypeohc + enginetypeohcf + 
                 enginetyperotor +  
                 fuelsystem2bbl + fuelsystemmpfi + X.car.company.BMW + X.car.company.BUICK + 
                 X.car.company.DODGE +  
                 X.car.company.MAZDA + X.car.company.MITSUBISHI + X.car.company.NISSAN + 
                 X.car.company.PLYMOUTH + X.car.company.RENAULT + 
                 X.car.company.TOYOTA + X.car.company.VOLKSWAGEN, data = train)

summary(model_18)
vif(model_18)

# Removing drivewheelrwd due to relatively high p value
model_19 <- lm(price ~ aspiration + enginelocation + carwidth +  
                 enginesize + enginetypel + enginetypeohc + enginetypeohcf + 
                 enginetyperotor +  
                 fuelsystem2bbl + fuelsystemmpfi + X.car.company.BMW + X.car.company.BUICK + 
                 X.car.company.DODGE +  
                 X.car.company.MAZDA + X.car.company.MITSUBISHI + X.car.company.NISSAN + 
                 X.car.company.PLYMOUTH + X.car.company.RENAULT + 
                 X.car.company.TOYOTA + X.car.company.VOLKSWAGEN, data = train)

summary(model_19)
vif(model_19)

# Removing X.car.company.VOLKSWAGEN due to relatively high p value
model_20 <- lm(price ~ aspiration + enginelocation + carwidth +  
                 enginesize + enginetypel + enginetypeohc + enginetypeohcf + 
                 enginetyperotor +  
                 fuelsystem2bbl + fuelsystemmpfi + X.car.company.BMW + X.car.company.BUICK + 
                 X.car.company.DODGE +  
                 X.car.company.MAZDA + X.car.company.MITSUBISHI + X.car.company.NISSAN + 
                 X.car.company.PLYMOUTH + X.car.company.RENAULT + 
                 X.car.company.TOYOTA, data = train)

summary(model_20)
vif(model_20)

# Removing fuelsystemmpfi due to relatively high p value
model_21 <- lm(price ~ aspiration + enginelocation + carwidth +  
                 enginesize + enginetypel + enginetypeohc + enginetypeohcf + 
                 enginetyperotor +  
                 fuelsystem2bbl + X.car.company.BMW + X.car.company.BUICK + 
                 X.car.company.DODGE +  
                 X.car.company.MAZDA + X.car.company.MITSUBISHI + X.car.company.NISSAN + 
                 X.car.company.PLYMOUTH + X.car.company.RENAULT + 
                 X.car.company.TOYOTA, data = train)

summary(model_21)
vif(model_21)

# Removing X.car.company.RENAULT due to relatively high p value
model_22 <- lm(price ~ aspiration + enginelocation + carwidth +  
                 enginesize + enginetypel + enginetypeohc + enginetypeohcf + 
                 enginetyperotor +  
                 fuelsystem2bbl + X.car.company.BMW + X.car.company.BUICK + 
                 X.car.company.DODGE +  
                 X.car.company.MAZDA + X.car.company.MITSUBISHI + X.car.company.NISSAN + 
                 X.car.company.PLYMOUTH + X.car.company.TOYOTA, data = train)

summary(model_22)
vif(model_22)

# Removing enginetyperotor due to relatively high p value and high VIF

model_23 <- lm(price ~ aspiration + enginelocation + carwidth +  
                 enginesize + enginetypel + enginetypeohc + enginetypeohcf + 
                 fuelsystem2bbl + X.car.company.BMW + X.car.company.BUICK + 
                 X.car.company.DODGE +  
                 X.car.company.MAZDA + X.car.company.MITSUBISHI + X.car.company.NISSAN + 
                 X.car.company.PLYMOUTH + X.car.company.TOYOTA, data = train)

summary(model_23)
vif(model_23)

# Removing fuelsystem2bbl due to relatively high p value and high VIF

model_24 <- lm(price ~ aspiration + enginelocation + carwidth +  
                 enginesize + enginetypel + enginetypeohc + enginetypeohcf + 
                 X.car.company.BMW + X.car.company.BUICK + 
                 X.car.company.DODGE +  
                 X.car.company.MAZDA + X.car.company.MITSUBISHI + X.car.company.NISSAN + 
                 X.car.company.PLYMOUTH + X.car.company.TOYOTA, data = train)

summary(model_24)
vif(model_24)

# Removing X.car.company.DODGE due to relatively high p value
model_25 <- lm(price ~ aspiration + enginelocation + carwidth +  
                 enginesize + enginetypel + enginetypeohc + enginetypeohcf + 
                 X.car.company.BMW + X.car.company.BUICK + 
                 X.car.company.MAZDA + X.car.company.MITSUBISHI + X.car.company.NISSAN + 
                 X.car.company.PLYMOUTH + X.car.company.TOYOTA, data = train)

summary(model_25)
vif(model_25)

# Removing X.car.company.PLYMOUTH due to relatively high p value
model_26 <- lm(price ~ aspiration + enginelocation + carwidth +  
                 enginesize + enginetypel + enginetypeohc + enginetypeohcf + 
                 X.car.company.BMW + X.car.company.BUICK + 
                 X.car.company.MAZDA + X.car.company.MITSUBISHI + X.car.company.NISSAN + 
                 X.car.company.TOYOTA, data = train)

summary(model_26)
vif(model_26)

# Removing X.car.company.MAZDA  due to relatively high p value

model_27 <- lm(price ~ aspiration + enginelocation + carwidth +  
                 enginesize + enginetypel + enginetypeohc + enginetypeohcf + 
                 X.car.company.BMW + X.car.company.BUICK + 
                 X.car.company.MITSUBISHI + X.car.company.NISSAN + 
                 X.car.company.TOYOTA, data = train)

summary(model_27)
vif(model_27)

# Removing X.car.company.NISSAN due to relatively high p value

model_28 <- lm(price ~ aspiration + enginelocation + carwidth +  
                 enginesize + enginetypel + enginetypeohc + enginetypeohcf + 
                 X.car.company.BMW + X.car.company.BUICK + 
                 X.car.company.MITSUBISHI + X.car.company.TOYOTA, data = train)

summary(model_28)
vif(model_28)

# Removing X.car.company.MITSUBISHI due to relatively high p value

model_29 <- lm(price ~ aspiration + enginelocation + carwidth +  
                 enginesize + enginetypel + enginetypeohc + enginetypeohcf + 
                 X.car.company.BMW + X.car.company.BUICK + 
                 X.car.company.TOYOTA, data = train)

summary(model_29)
vif(model_29)

# Removing X.car.company.TOYOTA due to relatively high p value

model_30 <- lm(price ~ aspiration + enginelocation + carwidth +  
                 enginesize + enginetypel + enginetypeohc + enginetypeohcf + 
                 X.car.company.BMW + X.car.company.BUICK, data = train)

summary(model_30)
vif(model_30)

# Removing aspiration due to relatively high p value
model_31 <- lm(price ~ enginelocation + carwidth +  
                 enginesize + enginetypel + enginetypeohc + enginetypeohcf + 
                 X.car.company.BMW + X.car.company.BUICK, data = train)


summary(model_31)
vif(model_31)

# Removing enginetypel due to relatively high p value
model_32 <- lm(price ~ enginelocation + carwidth +  
                 enginesize + enginetypeohc + enginetypeohcf + 
                 X.car.company.BMW + X.car.company.BUICK, data = train)

summary(model_32)
vif(model_32)

# Removing enginetypeohcf due to relatively high p value

model_33 <- lm(price ~ enginelocation + carwidth +  
                 enginesize + enginetypeohc +  
                 X.car.company.BMW + X.car.company.BUICK, data = train)

summary(model_33)
vif(model_33)

# We arrived at few independant variables with Adjusted R-squared value high
# There are attributes on the car's features and couple of attributes on the 
# other companies manufacturing automobiles in America.
# 
#car features: enginelocation, carwidth, enginesize, enginetypeohc
# 
# Market parameters: X.car.company.BMW, X.car.company.BUICK which implies the price also depends on
# competition 


# predicting the results in test dataset
Predict_1 <- predict(model_33,test[,-18])
test$test_price <- Predict_1
corre <- cor(test$price,test$test_price)
corre
# The price prediction is highly correlated (0.9175) to the price value in test data. 

rsquared <- corre ^ 2

rsquared



