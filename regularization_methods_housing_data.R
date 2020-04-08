#Name: Abishek Gisoji Ogadram
#Chewy Assignment
setwd("~/Documents/R/BA with R")

library(tidyverse) 
library(ggplot2)
library(TSA)
library(randomForest)
library(dplyr)
library(glmnet)
library(caret)
library(data.table)
library(DMwR)

rm(list=ls(all=TRUE))

######################################################################################################################################################################################
                                  #Importing our Data
######################################################################################################################################################################################
chewy <- read.csv("chewy.csv", stringsAsFactors = F)

######################################################################################################################################################################################
                                   #Data Exploratiom
######################################################################################################################################################################################
#Having a look at our variables
str(chewy)
summary(chewy)

#I now want to see how are depended variable looks like. 
# Having a Look at our Dependent variable SalePrice 
summary(chewy$SalePrice)
summary(chewy$YrSold) #Sold between 2006 and 2010
table(is.na(chewy$SalePrice)) # checking for NA's

#scatter plot
ggplot(chewy) +
  geom_point(aes(X...Order,SalePrice )) 

#histogram
ggplot(chewy, aes(SalePrice)) + geom_histogram(fill="red", binwidth = 10000) 

# It is right skewed which means less people buy high price houses which is generally true in real life.
# I want to now check which variables effect Salesunit so i constructed a correaltion Matrix.
# Not all variables are numerical first lets separte them.
#### Existung numerical variables and its relationship with SalesUnit.

#check for how many variables are numerical
table(sapply(chewy, is.numeric))

#taking only the numeric variables
numeric <- select_if(chewy, is.numeric)

#checking for correlation between our numerical Variables and SalesUnit.
cor_matrix <- cor(numeric, use="pairwise.complete.obs") # get correlations (returns matrix)

#For good visiuzlization lets use coorplot package
library('corrplot')
corrplot(cor_matrix, method = "circle") # there is too much information to take in

#lets look at only the relavent ones that have a correlation >0.5 or <-0.5. 
c <- cor_matrix
diag(c) <- 0
ok <- apply(abs(c) >= 0.5, 1, any)
c <- c[ok, ok]
corrplot(c, method = "circle") # now we see the most important variables

#Overall Quality,above grade Living Area,TotalBsmtSF,X1stflrSF,garagecars, garage area seem to be the highest
#correlated with Saleprice

#we can also see that alot of variables are correlated.
# for eg. 1) total basement sf and 1st floor sf
#         2) total rooms above ground and above ground living area
#         3) garage cars and garage area 
#         4)year built and garage year bult etc
#There may also be a issue of multicollinearity here as both variables 
#are highly correlated with SalePrice.

#Lets look at these variables more closely
#1)Overal Quality
summary(chewy$OverallQual) #seems like its an overall rating scale between 1-10.
hist(chewy$OverallQual)
plot(chewy$OverallQual,chewy$SalePrice)

#using ggplot
ggplot(chewy, aes(x=factor(OverallQual), y=SalePrice))+
  geom_boxplot(col='red') + labs(x='Overall Quality') 
#there is a very good positive realtion between rating and salesprie.
#there seem to be a few outliers especially in ratings 4,6 and 8.

#2)Above Grade living Area
summary(chewy$GrLivArea)
plot(chewy$GrLivArea, chewy$SalePrice) # we can see a clear linear trend here which makes sense, more area means higher price.
#using ggplot
ggplot(chewy, aes(x=GrLivArea, y=SalePrice))+
  geom_point(col='red') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) 
# we can see a few outiers here that have really large above ground area(>4500 sqft) but the prices are low(< $200,000)
GrlivArea_outliers <- filter(chewy, chewy$GrLivArea > 4000)
GrlivArea_outliers[,c('SalePrice', 'GrLivArea', 'OverallQual')]

#3)if have time check other variables

#####################################################################################################################################################################################################
                        #Lets impute the missing data, dealing with missing values
#####################################################################################################################################################################################################
summary(chewy)
names(which(sapply(chewy, anyNA))) #25 Cols have missing values
colSums(is.na(chewy))
#PoolQC, Fence, MiscFeature, Alley and FirePlaceQu have the highest missing values here. I will start with these

#PoolQC
#Pool QC has previously seen has 2917 missing values, lets look at the data once to understand the scale.
table(chewy$PoolQC) 
# There seems to be a scale of quality like Excellent, Fair, Good,etc Therefore, NA here must mean there is no pool to rate the quality.

#Lets check the pool area to see if the NA number matches here.
table(chewy$PoolArea) #2917 have 0sqft(Which means no pool area)
#imputing the data
chewy$PoolQC[is.na(chewy$PoolQC)]= "NotAval"

#There seems to only a few houses will pools.Lets see if they have a big impact on SalesPrice
#first lets convert them to a ordinal datatype
table(chewy$PoolQC)
chewy$PoolQC[chewy$PoolQC == "Ex"] <- 5
chewy$PoolQC[chewy$PoolQC == "Gd"] <- 4
chewy$PoolQC[chewy$PoolQC == "TA"] <- 3
chewy$PoolQC[chewy$PoolQC == "Fa"] <- 2
chewy$PoolQC[chewy$PoolQC == "NotAval"] <- 0
chewy$PoolQC <- as.numeric(chewy$PoolQC)
plot(chewy$PoolQC,chewy$SalePrice)
#PoolQC seems to increase the price

#poolArea
summary(chewy$PoolArea)
chewy$PoolArea <- as.numeric(chewy$PoolArea)

#MiscFeatures
table(chewy$MiscFeature)
chewy$MiscFeature[is.na(chewy$MiscFeature)]= "NotAval"
table(chewy$MiscFeature)
chewy$MiscFeature <- as.factor(chewy$MiscFeature)
plot(chewy$MiscFeature, chewy$SalePrice)

ggplot(chewy, aes(x=MiscFeature, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='red') 

#they all seem to have the same prices except the house that has TenC which is just one house.
#I will ignore this variable.

#Fence
table(chewy$Fence)
#the NA here must mean there is no fence. So i will change all NA's to Not Available.
chewy$Fence[is.na(chewy$Fence)]= "NotAval"
table(chewy$Fence)
#I have no clue what the data means here It might be something realted to fence quality. I will just make this data as a factor to be safe.
chewy$Fence <- as.factor(chewy$Fence)
plot(chewy$Fence,chewy$SalePrice)
#one would assume having a fence would increases price but here No fences seems to have more prices.
#But most of these seem to be outliers

#Alley
table(chewy$Alley) #Gravel and Pavement are to types of Alleys access. NA's must mean there are no Alley access for the house.
chewy$Alley[is.na(chewy$Alley)] = "NotAval"
chewy$Alley <- as.factor(chewy$Alley)
plot(chewy$Alley,chewy$SalePrice) #Grl Alley houses seem to have lesser price.

#Fireplace Quality
table(chewy$FireplaceQu) #seems to be a scale similar to our Pool quality variable
sum(is.na(chewy$FireplaceQu)) #there are 1422 NA's
table(chewy$Fireplaces) #there are 1422 fireplace houses with no fireplace
#Therefore we can change it to a zero rating for all 'NA's'
chewy$FireplaceQu[is.na(chewy$FireplaceQu)]= "NotAval"
table(chewy$FireplaceQu)
chewy$FireplaceQu[chewy$FireplaceQu == "Ex"] <- 5
chewy$FireplaceQu[chewy$FireplaceQu == "Gd"] <- 4
chewy$FireplaceQu[chewy$FireplaceQu == "TA"] <- 3
chewy$FireplaceQu[chewy$FireplaceQu == "Fa"] <- 2 
chewy$FireplaceQu[chewy$FireplaceQu == "Po"] <- 1
chewy$FireplaceQu[chewy$FireplaceQu == "NotAval"] <- 0
table(chewy$FireplaceQu)
class(chewy$FireplaceQu)
chewy$FireplaceQu <- as.numeric(chewy$FireplaceQu)
plot(chewy$FireplaceQu,chewy$SalePrice) # TA seems to mean something more than FA(Fair) as it has higher salesprice.

colSums(is.na(chewy))

#LotFrontage with 490 NA's
table(chewy$LotFrontage)
summary(chewy$LotFrontage)
plot(chewy$LotFrontage,chewy$SalePrice) # there doesnt seem to be any clear relationship
#there is alot of heteroskedacitcy in the data as I can see.
#I will come to this later
#impute with median value
chewy$LotFrontage[is.na(chewy$LotFrontage)] <- median(chewy$LotFrontage, na.rm=TRUE)
colSums(is.na(chewy))
###################################################################################################################################################################################
#knn imputation
#summary(chewy_practise$LotFrontage)
#install.packages("DMwR")
#library(DMwR)
#imp <- chewy_practise
#imp = filter(imp, imp$LotFrontage )
#colSums(is.na(imp))
#imp=knnImputation(imp,k=15)
#data[,c(1,34,35)]=imp[,c(1,2,3)]
#imp=knnImputation(imp,k=15)
#####################################################################################################################################################################################################
plot(chewy$LotArea,chewy$SalePrice) 

#Lot Area
summary(chewy$LotArea) #seems fine
table(chewy$LotShape)

#lot shape
chewy$LotShape <- as.factor(chewy$LotShape) 
plot(chewy$LotShape,chewy$SalePrice) #no clear scale so lets keep it a factor type

#lotConfig
table(chewy$LotConfig)
chewy$LotConfig <- as.factor(chewy$LotConfig)
plot(chewy$LotConfig,chewy$SalePrice) #no clear scale here too so lets keep it a factor type

colSums(is.na(chewy)) 
#GarageYr
sum(is.na(chewy$GarageYrBlt)) #159 missing (done)
sum(is.na(chewy$GarageType)) #157 missing  (done)
sum(is.na(chewy$GarageFinish)) #157 missing  (done)
sum(is.na(chewy$GarageCond)) #158 missing (done)
sum(is.na(chewy$GarageCars)) #1 missing (done)
sum(is.na(chewy$GarageArea)) #1 missing (done)
sum(is.na(chewy$GarageQual)) #158 missing (done)
table(chewy$GarageArea) #Frm here I could see there are 157 houses with 0sqft garage(no garage)
#so there is 1 extra NA in GarageCond and GarageQual I will ignore this for the time.

summary(chewy$GarageYrBlt) #Max value is 2207 which is abnormal this may be year 2007
chewy$GarageYrBlt=ifelse((chewy$GarageYrBlt)==2207,2007,chewy$GarageYrBlt) #replace 2207 with 2007
summary(chewy$GarageYrBlt)
chewy$GarageYrBlt[is.na(chewy$GarageYrBlt)] <- chewy$YearBuilt[is.na(chewy$GarageYrBlt)]
sum(is.na(chewy$GarageYrBlt))
#so there is 1 extra NA in GarageCond and GarageQual. I am going to ignore this for the time 

#garageQual
table(chewy$GarageQual)
chewy$GarageQual[is.na(chewy$GarageQual)] = "NotAval"
chewy$GarageQual[chewy$GarageQual == "Ex"] <- 5
chewy$GarageQual[chewy$GarageQual == "Gd"] <- 4
chewy$GarageQual[chewy$GarageQual == "Fa"] <- 2
chewy$GarageQual[chewy$GarageQual == "TA"] <- 3 
chewy$GarageQual[chewy$GarageQual == "Po"] <- 1
chewy$GarageQual[chewy$GarageQual == "NotAval"] <- 0
table(chewy$GarageQual)
class(chewy$GarageQual)
chewy$GarageQual <- as.numeric(chewy$GarageQual)
plot(chewy$GarageQual,chewy$SalePrice) #TA seems like its Average #change in back 

#garagetype
table(chewy$GarageType) #no particluar scale
chewy$GarageType[is.na(chewy$GarageType)] = "NotAval"
chewy$GarageType <- as.factor(chewy$GarageType)
plot(chewy$GarageType,chewy$SalePrice) #BUilt in garages have higher house prices.

#garagefinish
table(chewy$GarageFinish) #no particluar scale is scene safe to take as a factor
summary(chewy$GarageFinish)
sum(is.na(chewy$GarageFinish))
chewy$GarageFinish[is.na(chewy$GarageFinish)] = "NotAval"
chewy$GarageFinish <- as.factor(chewy$GarageFinish)
plot(chewy$GarageFinish,chewy$SalePrice) #Fin garages have higher house prices.

#garagecond
table(chewy$GarageCond) #there seems to be a scale
chewy$GarageCond[is.na(chewy$GarageCond)] = "NotAval"
chewy$GarageCond[chewy$GarageCond == "Ex"] <- 5
chewy$GarageCond[chewy$GarageCond == "Gd"] <- 4
chewy$GarageCond[chewy$GarageCond == "Fa"] <- 2
chewy$GarageCond[chewy$GarageCond == "TA"] <- 3 
chewy$GarageCond[chewy$GarageCond == "Po"] <- 1
chewy$GarageCond[chewy$GarageCond == "NotAval"] <- 0
table(chewy$GarageCond)
class(chewy$GarageCond)
chewy$GarageCond <- as.numeric(chewy$GarageCond)
plot(chewy$GarageCond,chewy$SalePrice)

#garageCars
table(chewy$GarageCars)
chewy$GarageCars[is.na(chewy$GarageCond)] = 0

#garagearea
summary(chewy$GarageArea)
chewy$GarageArea[is.na(chewy$GarageArea)] = 0

#Basement
colSums(is.na(chewy)) #There seems to be 79 houses with no basement. We can check the commonality of the missing values here
#and fix the uncommon ones but for the lack of time I will generalise.
#basement Quality
table(chewy$BsmtQual)
chewy$BsmtQual[is.na(chewy$BsmtQual)] = "NotAval"
chewy$BsmtQual[chewy$BsmtQual == "Ex"] <- 5
chewy$BsmtQual[chewy$BsmtQual == "Gd"] <- 4
chewy$BsmtQual[chewy$BsmtQual == "Fa"] <- 2
chewy$BsmtQual[chewy$BsmtQual == "TA"] <- 3 
chewy$BsmtQual[chewy$BsmtQual == "Po"] <- 1
chewy$BsmtQual[chewy$BsmtQual == "NotAval"] <- 0

table(chewy$BsmtQual)
class(chewy$BsmtQual)
chewy$BsmtQual <- as.numeric(chewy$BsmtQual)
plot(chewy$BsmtQual,chewy$SalePrice)


colSums(is.na(chewy)) 
#basement condition
table(chewy$BsmtCond)
chewy$BsmtCond[is.na(chewy$BsmtCond)] = "NotAval"
chewy$BsmtCond[chewy$BsmtCond == "Ex"] <- 5
chewy$BsmtCond[chewy$BsmtCond == "Gd"] <- 4
chewy$BsmtCond[chewy$BsmtCond == "Fa"] <- 2
chewy$BsmtCond[chewy$BsmtCond == "TA"] <- 3 
chewy$BsmtCond[chewy$BsmtCond == "Po"] <- 1
chewy$BsmtCond[chewy$BsmtCond == "NotAval"] <- 0

table(chewy$BsmtCond)
class(chewy$BsmtCond)
chewy$BsmtCond <- as.numeric(chewy$BsmtCond)
plot(chewy$BsmtCond,chewy$SalePrice)

#BsmtExposure
table(chewy$BsmtExposure)
chewy$BsmtExposure[is.na(chewy$BsmtExposure)] = "No"
chewy$BsmtExposure <- as.factor(chewy$BsmtExposure)
plot(chewy$BsmtExposure,chewy$SalePrice) 

#Quality of basement finished area
table(chewy$BsmtFinType1)
chewy$BsmtFinType1[is.na(chewy$BsmtFinType1)] = "NotAval"
chewy$BsmtFinType1 <- as.factor(chewy$BsmtFinType1)
plot(chewy$BsmtFinType1,chewy$SalePrice) #there is no clear quality difference so i will leave this as a factor

#Quality of second basement finished area
table(chewy$BsmtFinType2)
chewy$BsmtFinType2[is.na(chewy$BsmtFinType2)] = "NotAval"
chewy$BsmtFinType2 <- as.factor(chewy$BsmtFinType2)
plot(chewy$BsmtFinType2,chewy$SalePrice) #there is no clear quality difference so i will leave this as a factor

#Masonry veneer Area
table(chewy$MasVnrType)
summary(chewy$MasVnrArea) #there seems to be 23 houses without Masonry Venner lets make this area 0.
chewy$MasVnrArea[is.na(chewy$MasVnrArea)] = 0
chewy$MasVnrType <- as.factor(chewy$MasVnrType)

#basement finish Sf
summary(chewy$BsmtFinSF1)
chewy$BsmtFinSF1[is.na(chewy$BsmtFinSF1)] = 0

#basement finish Sf2
summary(chewy$BsmtFinSF2)
chewy$BsmtFinSF2[is.na(chewy$BsmtFinSF2)] = 0

#bsmtunfsf
summary(chewy$BsmtUnfSF)
chewy$BsmtUnfSF[is.na(chewy$BsmtUnfSF)] = 0

#totalbsmtsf
summary(chewy$TotalBsmtSF)
chewy$TotalBsmtSF[is.na(chewy$TotalBsmtSF)] = 0

#bsmthalfbath
table(chewy$BsmtHalfBath) 
chewy$BsmtHalfBath[is.na(chewy$BsmtHalfBath)] <-0

#bsmtfullbath
table(chewy$BsmtFullBath)
chewy$BsmtFullBath[is.na(chewy$BsmtFullBath)] <-0
#at this point I have dealt with all the NA's, this took alot of time. I still need to factorize my other charcter variables.
#I can go through each variable and find if i can use any as a ordinal variable but to save time i will take all as factors for time being and change as and when i see anything important.
colSums(is.na(chewy))

#There are some new NA's that arrised this may have happened while converting to factors 
#I will just rerun the existing code for those particular variables.

#basement condition
table(chewy$BsmtCond)
chewy$BsmtCond[is.na(chewy$BsmtCond)] = "NotAval"
chewy$BsmtCond[chewy$BsmtCond == "NotAval"] <- 0
chewy$BsmtCond <- as.numeric(chewy$BsmtCond)

#basement quality
chewy$BsmtQual[is.na(chewy$BsmtQual)] = "NotAval"
chewy$BsmtQual[chewy$BsmtQual == "NotAval"] <- 0
chewy$BsmtQual <- as.numeric(chewy$BsmtQual)

#Basement Fintype1
chewy$BsmtFinType1[is.na(chewy$BsmtFinType1)] = "NotAval"
chewy$BsmtFinType1 <- as.factor(chewy$BsmtFinType1)

#GarageQual    
chewy$GarageQual[is.na(chewy$GarageQual)] = "NotAval"
chewy$GarageQual[chewy$GarageQual == "NotAval"] <- 0
chewy$GarageQual <- as.numeric(chewy$GarageQual)

#GarageCond 
chewy$GarageCond[is.na(chewy$GarageCond)] = "NotAval"
chewy$GarageCond[chewy$GarageCond == "NotAval"] <- 0
chewy$GarageCond <- as.numeric(chewy$GarageCond)

colSums(is.na(chewy))
##############################################--End of NA's----#############################################################################
##############################################################################################################################################
                                #changing all character variables to Factors
#################################################################################################################################################
character_vars <- lapply(chewy, class) == "character"
chewy[, character_vars] <- lapply(chewy[, character_vars], as.factor)
str(chewy)
#at this point i am done with initally data cleaning(missing values and converting data types to factors)
#there may be mistakes which i will correct as and when i see them.

#################################################################################################################################################
#Looking at variables that look like ID to remove them
#################################################################################################################################################
table(chewy$X...Order)
table(chewy$PID)
chewy$X...Order <- NULL
chewy$PID <- NULL
############################################################################################################################
                                #Looking for important varaiables! 
############################################################################################################################

#Linear regression with stewise function to see important variables.(time consuming with alot of variables)
#model <- step(lm(SalePrice~., chewy), direction = "both")

#problems with stepwise function ? among the correlated variables we might get output of the less depend variable
#time consuming etc

#As my primary importance is prediction and not inference I will use other methods.
##############################################################################################################
                                  #Random forest to see important variables
##############################################################################################################
set.seed(100)
rm_model1 <- randomForest(SalePrice ~ ., data = chewy, importance = TRUE)#default trees uses = 500, splits=26
rm_model1
# To check important variables
importance(rm_model1)        
varImpPlot(rm_model1)        

#The top 10 variables contain 4 square feet variables. Thnking about the prices of a house in real life these output variables
#seem right. Some of these variables are correlated and may lead to multicollinearity.
# So lets make some new variables before we omit them.

#####################################################################################################################
                                    #Feature Engineering/Creating variables 
#####################################################################################################################
#Square feet variables in top 10
cor(chewy$GrLivArea, (chewy$X1stFlrSF + chewy$X2ndFlrSF ))
# as expected these are highly correlated
#I will create a new variable for total Area
chewy$totalSF= chewy$TotalBsmtSF+chewy$X1stFlrSF+chewy$X2ndFlrSF
cor(chewy$SalePrice, chewy$totalSF, use= "pairwise.complete.obs")

ggplot(data=chewy[!is.na(chewy$SalePrice),], aes(x=totalSF, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))
# As expected Its a linear increase. there are a few outliers here that I will ignore for the time being.
# I will use this now and remove GrLiv area, 1st floorsf, 2nd floor sf and basement sf. and keep the Total SF variable we just created.

#house Age variables 
#YearRemodAdd = YearBuilt if there has been no Remodeling
chewy$age=chewy$YrSold - chewy$YearBuilt   
chewy$adage=chewy$YearRemodAdd - chewy$YearBuilt 
chewy$adsold=chewy$YearRemodAdd - chewy$YrSold 

#Lets plot our new variable
ggplot(data=chewy[!is.na(chewy$SalePrice),], aes(x=age, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))
#As expected older house means lower prices.

#Neigboorhood
summary(chewy$Neighborhood)
class(chewy$Neighborhood)

#overallQuality
summary(chewy$OverallQual)
ggplot(data=chewy[!is.na(chewy$SalePrice),], aes(x=OverallQual, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))

#basement variables in top 30
#I have already included a SF varaible the basement total SF which is important I will now include a quality variable and leave the others.
summary(chewy$BsmtFinType1) #ignore
plot(chewy$BsmtFinType1, chewy$SalePrice)
summary(chewy$BsmtFinSF1) 
plot(chewy$BsmtFinSF1, chewy$SalePrice) #ignore
summary(chewy$BsmtQual)
plot(chewy$BsmtQual, chewy$SalePrice) #imp
summary(chewy$BsmtUnfSF)
plot(chewy$BsmtUnfSF, chewy$SalePrice) #ignore
summary(chewy$BsmtExposure)
plot(chewy$BsmtExposure, chewy$SalePrice) #ignore

#garage variables
#Several Garage variables have a high correlation with SalePrice, and are also in the top-20 list of the quick random forest.
cor(chewy$GarageCars, chewy$GarageArea) #highly correletaed as scence in our intial analysis
cor(chewy$GarageCars,chewy$SalePrice)
cor(chewy$GarageArea,chewy$SalePrice) #I will keep only Garage Cars drop garage Area
cor(chewy$GarageCond,chewy$GarageQual)
cor(chewy$GarageCond,chewy$SalePrice)
cor(chewy$GarageQual,chewy$SalePrice)#I will keep only garageQualand drop garage Cond
plot(chewy$GarageType,chewy$SalePrice) #Builtin seem to have high saleprice so ill keep this

#Total bathrooms 
chewy$totalbath <- chewy$FullBath + (chewy$HalfBath*0.5) + chewy$BsmtFullBath + (chewy$BsmtHalfBath*0.5) 
# i will include half bathroom as half as it is defenitley less expensive then a full sized bathroom
cor(chewy$SalePrice,chewy$totalbath)

#Total Porch Area
chewy$TotalPorchSF <- chewy$OpenPorchSF + chewy$EnclosedPorch + chewy$X3SsnPorch + chewy$ScreenPorch + chewy$WoodDeckSF
cor(chewy$SalePrice,chewy$TotalPorchSF)


##############################################################################################################################################################
                                          #Dropping highly correlated variables 
######################################################################################################################################################################################
numeric <- select_if(chewy, is.numeric)
cor_matrix <- cor(numeric, use="pairwise.complete.obs") # get correlations (returns matrix)
corrplot(cor_matrix, method = "circle") 
#lets look at only the relavent ones
c <- cor_matrix
diag(c) <- 0
ok <- apply(abs(c) >= 0.5, 1, any)
c <- c[ok, ok]
corrplot(c, method = "circle")
drop <- c('YearRemodAdd', 'YearBuilt', 'GarageYrBlt', 'GarageArea', 'GarageCond', 'TotalBsmtSF', 'TotalRmsAbvGrd', 'BsmtFinSF1' , 'RoofMatl')
chewy_feature_new <- chewy
chewy <- chewy_feature_new
chewy <- chewy[,!(names(chewy) %in% drop)]

######################################################################################################################################################################################################
                                                        #Skewness
######################################################################################################################################################################################################
#Taking the log of SalePrice to deal with Skewness
#problems of skewness? why should i take log ? to normalize the data, our models are based on the assumption that our data is normal. it also help with dealing with hetroskedasity in our data.
#check for skewness
hist(chewy$SalePrice)
skewness(chewy$SalePrice) #this is too high it should ideally be between -1 and 1
chewy$SalePrice <- log(chewy$SalePrice) #default is the natural logarithm, "+1" is not necessary as there are no 0's
skewness(chewy$SalePrice) #this is better now 
hist(chewy$SalePrice)

###########################################################################################################################################################################################################
                                               #Split data into train and test
###########################################################################################################################################################################################################
## 75% of the sample size
smp_size <- floor(0.75 * nrow(chewy))
## set the seed to make your partition reproducible
set.seed(100)
train_ind <- sample(seq_len(nrow(chewy)), size = smp_size)
train <- chewy[train_ind, ]
test <- chewy[-train_ind, ]

y_test <- test$SalePrice
y_test
################################################################################################################################################################################################################
                                                #Models
###############################################################################################################################################################################################################
                                          #Linear Regression 
###############################################################################################################################################################################################################

model_lm <- lm(SalePrice~.,train)
summary(model_lm)

#stepwise <- step(lm(SalePrice~., chewy), direction = "both")
# Model with important variables taken from stepwise output using lowest AIC value.
model_lm2 <- lm(SalePrice~MSSubClass + MSZoning + LotArea + Street + LotShape + 
                  LandContour + Neighborhood + Condition1 + Condition2 + BldgType + 
                  OverallQual + OverallCond + MasVnrType + ExterCond + 
                  Foundation + BsmtQual + BsmtExposure + BsmtUnfSF + 
                  Heating + HeatingQC + CentralAir + X1stFlrSF + X2ndFlrSF + 
                  LowQualFinSF + BsmtFullBath + FullBath + HalfBath + KitchenQual + 
                  TotRmsAbvGrd + Functional + Fireplaces + FireplaceQu + GarageType + 
                  GarageCars + GarageQual + PavedDrive + WoodDeckSF + EnclosedPorch + 
                  X3SsnPorch + ScreenPorch + PoolQC + YrSold + 
                  SaleCondition + totalSF + age + adage,train)

summary(model_lm2)
plot(model_lm2$fit,model_lm$residuals)
abline(h=0,col="blue") #we can see a few outliers here.
sqrt(mean(model_lm2$residuals^2)) #rmse 

prediction_linear <- predict(model_lm2,test,type = "response")
regr.eval(y_test,prediction_linear) #0.11907  #this is not bad at all considering a simple linear regression.
################################################################################################################################################################################################################
                                                #Random Forest
#####################################################################################################################################################################################################################
#Using random forest, it is one of the best tree methods to use given the high number of categorical variables we have
set.seed(100)
model1 <- randomForest(SalePrice ~ ., data = chewy, importance = TRUE)#default trees uses = 500, splits=25
model1
# To check important variables
importance(model1)        
varImpPlot(model1)        

#model2
model2 <- randomForest(SalePrice ~ ., data = train, ntree=100, mtry = 9, importance = TRUE)
model2

#prediction on train
pred_train_rf <- predict(model1, train, type = "class")
regr.eval(y,pred_train_rf) #rmse 0.05498

# Predicting on test set
pred_test_rf <- predict(model1, test, type = "class")
regr.eval(y_test,pred_test_rf) #rmse 0.04676

#################################################################################################################
                                      #Using regularization methods
################################################################################################################
myTrainControl <- trainControl(method="cv",number=10,repeats = 4)
set.seed(100)
#preping our data for ridge
y <- train$SalePrice
y
X= data.matrix(train[,setdiff(names(train),"SalePrice")])
#############################################################################################################################################################################################
                                                #Ridge model
#############################################################################################################################################################################################
model_ridge <- train(X,y,trControl = myTrainControl,
                     method="glmnet", metric = "RMSE", tuneGrid=expand.grid(.alpha = 0, 
                                                                            .lambda = seq(0, 1, by = 0.05)))

model_ridge
model_ridge$bestTune #best tune is alpha 0 and lambda 0.1
min(model_ridge$results$RMSE) # 0.1474661
varImp(model_ridge) #lasso chose 77 variables out of our total 78(I expected this number to be lesser)

finaltest =  data.matrix(test[,setdiff(names(test),"SalePrice")])
pred_ridge = predict(model_ridge,finaltest,s=fit.glmnet$lambda.min)
regr.eval(y_test,pred_ridge) #rmse 0.133901

#############################################################################################################################################################################################
                                                #Lasso model
#############################################################################################################################################################################################

model_lasso <- train(X,y,trControl = myTrainControl,
                    method="glmnet",tuneGrid=expand.grid(.alpha = 1, 
                                                         .lambda = seq(0, 1, by = 0.01)))
model_lasso
model_lasso$bestTune #best tune is alpha 1 and lambda 0.01
min(model_lasso$results$RMSE)  #0.1482

pred_lasso = predict(model_lasso,finaltest,s=fit.glmnet$lambda.min)
regr.eval(y_test,pred_lasso)  #rmse 0.138457

#############################################################################################################################################################################################
                                                  #Elastic Net
#############################################################################################################################################################################################

fit.glmnet <- train(X,y,trControl = myTrainControl,
                    method="glmnet",tuneGrid=expand.grid(.alpha = seq(0,1,by=0.01), 
                                                         .lambda = seq(0, 1, by = 0.05)))

fit.glmnet
fit.glmnet$bestTune #best tune is alpha 0.09 and lambda 0.05
min(fit.glmnet$results$RMSE) #0.1465

pred_Test = predict(fit.glmnet,finaltest,s=fit.glmnet$lambda.min)
regr.eval(y_test,pred_Test) #rmse 0.13354


