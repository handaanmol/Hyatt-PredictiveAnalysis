marchTexasData <- read.csv("filteredData1.csv")
juneTexasData <- read.csv("filteredData2.csv")
septemberTexasData <- read.csv("filteredData3.csv")
decemberTexasData <- read.csv("filteredData4.csv")


combinedTexasData <- rbind(marchTexasData, juneTexasData, septemberTexasData, decemberTexasData)
write.csv(combinedTexasData,file= 'combinedTexasData.csv')
combinedTexasData

#Lets look at highest hotel entries along with net promoter score for the hotels

#install.packages("sqldf")
library(sqldf)
#install.packages("ggplot")
library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)

avgHotelNetPromoterScore <- sqldf("select AVG(Likelihood_Recommend_H), Count(X), Brand_PL from combinedTexasData GROUP BY Brand_PL ")
avgHotelNetPromoterScore

#To check NPS Score for Texas over 4 months
npsMarch <- sqldf("select Count(NPS_Type) from  marchTexasData Where NPS_TYPE='Promoter'and City_PL='San Antonio'")
npsMarchOverall <-sqldf("select Count(NPS_Type) from  marchTexasData Where City_PL='San Antonio'") 
npsMarch <- npsMarch/npsMarchOverall*100
npsMarch


npsJune <- sqldf("select Count(NPS_Type) from  juneTexasData Where NPS_TYPE='Promoter' and City_PL='San Antonio' ")
npsJuneOverall <-sqldf("select Count(NPS_Type) from  juneTexasData Where City_PL='San Antonio'") 
npsJune <- npsJune/npsJuneOverall*100
npsJune

npsSep <- sqldf("select Count(NPS_Type) from  septemberTexasData Where NPS_TYPE='Promoter' and City_PL='San Antonio' ")
npsDecOverall <-sqldf("select Count(NPS_Type) from  septemberTexasData Where City_PL='San Antonio'") 
npsSep <- npsSep/npsDecOverall*100
npsSep

npsDec <- sqldf("select Count(NPS_Type) from  decemberTexasData Where NPS_TYPE='Promoter' And City_PL='San Antonio' ")
npsDecOverall <-sqldf("select Count(NPS_Type) from  decemberTexasData Where City_PL='San Antonio'") 
npsDec <- npsDec/npsDecOverall*100

npsDec

#Now same calculation of AVG net promoter score for march month only
avgHotelNetPromoterScore3 <- sqldf("select AVG(Likelihood_Recommend_H), Count(X), Brand_PL from  marchTexasData GROUP BY Brand_PL ")
avgHotelNetPromoterScore3

#Now same calculation of AVG net promoter score for june month only
avgHotelNetPromoterScore4 <- sqldf("select AVG(Likelihood_Recommend_H), Count(X), Brand_PL from  juneTexasData GROUP BY Brand_PL ")
avgHotelNetPromoterScore4

#Now same calculation of AVG net promoter score for sep month only
avgHotelNetPromoterScore5 <- sqldf("select AVG(Likelihood_Recommend_H), Count(X), Brand_PL from  septemberTexasData GROUP BY Brand_PL ")
avgHotelNetPromoterScore5

#Now same calculation of AVG net promoter score for december month only
avgHotelNetPromoterScore2 <- sqldf("select AVG(Likelihood_Recommend_H), Count(X), Brand_PL from decemberTexasData GROUP BY Brand_PL ")
avgHotelNetPromoterScore2


#Comparison plot by tanay

#From the comparison we see that, Hyatt House and Hyatt Regency have weird behaviour throughout the 4 months with Hyatt regency having
#low promoter scores throughout data and especially in december with more customers coming, we will just pick Hyatt regency to do further analysis


#HyattRegency
hyattRegencyDataSet <- sqldf("select * from combinedTexasData WHERE Brand_PL ='Hyatt Regency' ")
hyattRegencyDataSet

#replacing null values with NA
hyattRegencyDataSet[hyattRegencyDataSet==''] <- NA

hyattRegencyDataSet$Clublounge_Used_H <- as.character(hyattRegencyDataSet$Clublounge_Used_H, stringsAsFactors=FALSE)
hyattRegencyDataSet$Spa_Used_H <- as.character(hyattRegencyDataSet$Spa_Used_H, stringsAsFactors=FALSE)
hyattRegencyDataSet$MEMBER_STATUS_R <- as.character(hyattRegencyDataSet$MEMBER_STATUS_R, stringsAsFactors=FALSE)

hyattRegencyDataSet[["Clublounge_Used_H"]][is.na(hyattRegencyDataSet[["Clublounge_Used_H"]])] <-"I don't know"
hyattRegencyDataSet[["Spa_Used_H"]][is.na(hyattRegencyDataSet[["Spa_Used_H"]])] <-"I don't know"
hyattRegencyDataSet[["MEMBER_STATUS_R"]][is.na(hyattRegencyDataSet[["MEMBER_STATUS_R"]])] <-"None"

#Replacing na Values in F.b_overall_h, Check_in_h, internet service and few other with mean values
for(col in 1:ncol(hyattRegencyDataSet)){
  hyattRegencyDataSet[is.na(hyattRegencyDataSet[,col]), col] <- round(mean(na.omit(hyattRegencyDataSet[,col])))
}


#Checking summary of fields to figure out which fields are not changing
#
summary(hyattRegencyDataSet$Mini.Bar_PL)
#
summary(hyattRegencyDataSet$All.Suites_PL)
summary(hyattRegencyDataSet$Bell.Staff_PL)
#
summary(hyattRegencyDataSet$Boutique_PL)
summary(hyattRegencyDataSet$Business.Center_PL)
#
summary(hyattRegencyDataSet$Casino_PL)
#
summary(hyattRegencyDataSet$Conference_PL)
summary(hyattRegencyDataSet$Convention_PL)
summary(hyattRegencyDataSet$Dry.Cleaning_PL)
#
summary(hyattRegencyDataSet$Elevators_PL)
#
summary(hyattRegencyDataSet$Fitness.Center_PL)
#
summary(hyattRegencyDataSet$Fitness.Trainer_PL)
summary(hyattRegencyDataSet$Golf_PL)
#
summary(hyattRegencyDataSet$Indoor.Corridors_PL)
summary(hyattRegencyDataSet$Laundry_PL)
summary(hyattRegencyDataSet$Limo.Service_PL)
#
summary(hyattRegencyDataSet$Pool.Indoor_PL)
#
summary(hyattRegencyDataSet$Pool.Outdoor_PL)
summary(hyattRegencyDataSet$Regency.Grand.Club_PL)
summary(hyattRegencyDataSet$Resort_PL)
#
summary(hyattRegencyDataSet$Restaurant_PL)
summary(hyattRegencyDataSet$Self.Parking_PL)
summary(hyattRegencyDataSet$Shuttle.Service_PL)
#
summary(hyattRegencyDataSet$Ski_PL)
summary(hyattRegencyDataSet$Spa_PL)
summary(hyattRegencyDataSet$Valet.Parking_PL)
summary(hyattRegencyDataSet$Spa_Used_H)
summary(hyattRegencyDataSet$Clublounge_Used_H)

#Dropping columns by names since these fields are not contributing here 
hyattRegencyDataSet = subset(hyattRegencyDataSet, select = -c(Spa.services.in.fitness.center_PL,Spa.online.booking_PL,Spa.F.B.offering_PL) )
hyattRegencyDataSetFiltered = subset(hyattRegencyDataSet, select = -c(Mini.Bar_PL,All.Suites_PL,Bell.Staff_PL, Boutique_PL,Casino_PL,Conference_PL,Elevators_PL,Fitness.Center_PL,
                                                              Fitness.Trainer_PL,Indoor.Corridors_PL,Pool.Indoor_PL,Pool.Outdoor_PL,Restaurant_PL,Ski_PL,ROOM_NUM_C,Club.Type_PL,MAJOR_MARKET_CODE_C,ROOM_TYPE_CODE_C,
                                                              PMS_ROOM_REV_USD_C,RESERVATION_STATUS_R,MAJOR_MARKET_CODE_R,PAST_VS_FUTURE_R,State_PL,Country_PL,Guest.NPS.Goal_PL,Brand_PL,Hotel.Inventory_PL,Floors_PL,Union_PL,Relationship_PL) )

#Cleaning Further by just keeping genders as male or females
hyattRegencyDataSetFiltered <- subset(hyattRegencyDataSetFiltered,Gender_H %in% c("Male","Female")) # only keep male and female gender
hyattRegencyDataSetFiltered$F.B_Overall_Experience_H <- round(hyattRegencyDataSetFiltered$F.B_Overall_Experience_H )
#Omitting few records where adult nos are not mentioned
hyattRegencyDataSetFiltered <-  hyattRegencyDataSetFiltered[!is.na(hyattRegencyDataSetFiltered$ADULT_NUM_C),]


hyattRegencyDataSetFiltered<- na.omit(hyattRegencyDataSetFiltered)
hyattRegencyDataSetFiltered <- hyattRegencyDataSetFiltered[hyattRegencyDataSetFiltered$X!=605,]
#write.csv(hyattRegencyDataSetFiltered, file='hyattRegencyDataSetFiltered.csv')

#Checkpoint after removing Na's and replacing relevant NA's with mean values


#Checking data is clean by going through the unique values


#Giving data types to each field properly
hyattRegencyDataSetFiltered$LENGTH_OF_STAY_C <- as.numeric(hyattRegencyDataSetFiltered$LENGTH_OF_STAY_C)
hyattRegencyDataSetFiltered$Gender_H <- as.factor(hyattRegencyDataSetFiltered$Gender_H)
hyattRegencyDataSetFiltered$Age_Range_H <- as.factor(hyattRegencyDataSetFiltered$Age_Range_H)
hyattRegencyDataSetFiltered$POV_CODE_C <- as.factor(hyattRegencyDataSetFiltered$POV_CODE_C)
hyattRegencyDataSetFiltered$ADULT_NUM_C <- as.numeric(hyattRegencyDataSetFiltered$ADULT_NUM_C) 
hyattRegencyDataSetFiltered$REVENUE_USD_R <- as.numeric(hyattRegencyDataSetFiltered$REVENUE_USD_R)
hyattRegencyDataSetFiltered$Spa_PL <- as.factor(hyattRegencyDataSetFiltered$Spa_PL)
hyattRegencyDataSetFiltered$Likelihood_Recommend_H <- as.numeric(hyattRegencyDataSetFiltered$Likelihood_Recommend_H)
hyattRegencyDataSetFiltered$Guest_Room_H <- as.numeric(hyattRegencyDataSetFiltered$Guest_Room_H)
hyattRegencyDataSetFiltered$Tranquility_H <- as.numeric(hyattRegencyDataSetFiltered$Tranquility_H)
hyattRegencyDataSetFiltered$Condition_Hotel_H <- as.numeric(hyattRegencyDataSetFiltered$Condition_Hotel_H)
hyattRegencyDataSetFiltered$Customer_SVC_H <- as.numeric(hyattRegencyDataSetFiltered$Customer_SVC_H)
hyattRegencyDataSetFiltered$Staff_Cared_H <- as.numeric(hyattRegencyDataSetFiltered$Staff_Cared_H)
hyattRegencyDataSetFiltered$Internet_Sat_H <- as.numeric(hyattRegencyDataSetFiltered$Internet_Sat_H)
hyattRegencyDataSetFiltered$Check_In_H <- as.numeric(hyattRegencyDataSetFiltered$Check_In_H)
hyattRegencyDataSetFiltered$Clublounge_Used_H <- as.factor(hyattRegencyDataSetFiltered$Clublounge_Used_H)
hyattRegencyDataSetFiltered$Spa_Used_H <- as.factor(hyattRegencyDataSetFiltered$Spa_Used_H)
hyattRegencyDataSetFiltered$MEMBER_STATUS_R <- as.factor(hyattRegencyDataSetFiltered$MEMBER_STATUS_R)

#Adding a new field to check whether people recommend
hyattRegencyDataSetFiltered$willRecommend <- as.character(as.numeric(hyattRegencyDataSetFiltered$Likelihood_Recommend_H > 7))
hyattRegencyDataSetFiltered <- within(hyattRegencyDataSetFiltered, willRecommend[Likelihood_Recommend_H >8 & Likelihood_Recommend_H <=10] <- 1)
hyattRegencyDataSetFiltered <- within(hyattRegencyDataSetFiltered, willRecommend[Likelihood_Recommend_H >=7 & Likelihood_Recommend_H <= 8] <-  0)
hyattRegencyDataSetFiltered <- within(hyattRegencyDataSetFiltered, willRecommend[Likelihood_Recommend_H >=0 & Likelihood_Recommend_H < 7] <- -1)
hyattRegencyDataSetFiltered$willRecommend <- as.factor(hyattRegencyDataSetFiltered$willRecommend)
write.csv(hyattRegencyDataSetFiltered,"hyattRegencyDataSetFiltered.csv")

#Calculating Avg revenue each customer giving us from Business and Leisure class
avgRevenue<- aggregate(hyattRegencyDataSetFiltered$REVENUE_USD_R, by= list(hyattRegencyDataSetFiltered$POV_CODE_C), mean)
avgRevenue

#Calculating Overall revenue from different groups of customers
netRevenue<- tapply(hyattRegencyDataSetFiltered$REVENUE_USD_R, hyattRegencyDataSetFiltered$POV_CODE_C, sum)
netRevenue

#Calculating Overall Customers from Leisure and Business
overallCustomers <- tapply(hyattRegencyDataSetFiltered$REVENUE_USD_R, hyattRegencyDataSetFiltered$POV_CODE_C, length)
overallCustomers

#Calculating Avg Length of stay for Business and Leisure Class
avgLengthStay<- tapply(hyattRegencyDataSetFiltered$LENGTH_OF_STAY_C, hyattRegencyDataSetFiltered$POV_CODE_C, mean)
avgLengthStay

#Calculating Avg likelihood to Recommend for business and Leisure Class
avgLikeLihood<- tapply(hyattRegencyDataSetFiltered$Likelihood_Recommend_H, hyattRegencyDataSetFiltered$POV_CODE_C, mean)
avgLikeLihood

medianLikeLihood<- tapply(hyattRegencyDataSetFiltered$Likelihood_Recommend_H, hyattRegencyDataSetFiltered$POV_CODE_C, median)
medianLikeLihood

comparisonRowNames <- c("Leisure", "Business")
#For Avg Revenue
pieSlices <- c(avgRevenue[2,2],avgRevenue[1,2]) 
pct <- round(pieSlices/sum(pieSlices)*100)
label <- paste(comparisonRowNames, pct) # add percents to labels 
lbls <- paste(label,"%",sep="") # ad % to labels 
pieChart1 <- pie(pieSlices,labels = lbls, col=rainbow(length(lbls)),main="Average Revenue for different Type of Guests in $")

#For total Revenue
pieSlices2 <- c(netRevenue[2],netRevenue[1])
pct2 <- round(pieSlices2/sum(pieSlices2)*100)
label2 <- paste(comparisonRowNames, pct2) # add percents to labels 
lbls <- paste(label2,"%",sep="")
pieChart2 <- pie(pieSlices2,labels = lbls, col=rainbow(length(lbls)),main="Net Revenue for different Type of Guests in $")

#For overall Customers
pieSlices3 <- c(overallCustomers[2],overallCustomers[1])
pct3 <- round(pieSlices3/sum(pieSlices3)*100)
label3 <- paste(comparisonRowNames, pct3) # add percents to labels 
lbls <- paste(label3,"%",sep="")
pieChart3 <- pie(pieSlices3,labels = lbls, col=rainbow(length(lbls)),main="Frequency of Guests")

#Conclusion - Avg $ spent by Business visitor is less than the Leisure Vistor but netrevenue from
#Business Vistor is 5 times the revenue from Leisure with proven fact both are staying for almost 2 days on an average
#Also, no of Business Vistors are 5 times the Leisure Visitors
#Main source of income for hyatt regency in Texas is Business Class


################Checking AVG LIKELIHOOD throughout months
avgHotelNetPromoterScoreBasedOnMonths <- sqldf("select AVG(Likelihood_Recommend_H) as avgLikelihood , strftime('%m',CHECK_IN_DATE_C) as month  from  hyattRegencyDataSetFiltered GROUP BY strftime('%m',CHECK_IN_DATE_C)")
avgHotelNetPromoterScoreBasedOnMonths$month <- as.factor(avgHotelNetPromoterScoreBasedOnMonths$month)

comparisonRowNames <- c("March", "June", "Sep", "Dec")


barplot(avgHotelNetPromoterScoreBasedOnMonths$avgLikelihood, xlab= "Months", ylab= "Avg Likelihood", main= "Comparison of Avg Likelihood for different Quarters", names.arg= comparisonRowNames, col = "blue", ylim = c(8,9),beside=TRUE, xpd=FALSE )
#### Avg Likelihood came down


#Splitting data into training and test datasets
#2/3 training dataset and 1/3 test dataset
datasetFormation <- function(df){
  randIndex <- sample(1:nrow(df))
  cutPoint <- round(2 * nrow(df) / 3)
  trainingData <- df[randIndex[1:cutPoint],]
  testData <- df[randIndex[(cutPoint + 1): nrow(df)],]
  return(list(trainingData, testData))
}


#Calling datasetFormation for traindataSet and testDataSet
#All the importatn attributes taken here except X, and NPS_Type and Likelihood_Recommend_H
#tempDataFrame <- datasetFormation(hyattRegencyDataSetFiltered[,c(2:13,15:36,38)])
tempDataFrame <- datasetFormation(hyattRegencyDataSetFiltered[,c(5,8,10:13,15:22,38)])
#all the age, sex, date of checkin, adults no, class,guest country, membership, gender, length of stay
#tempDataFrame2 <- datasetFormation(hyattRegencyDataSetFiltered[,c(2:8,10:11,38)])

trainingData <- as.data.frame(tempDataFrame[1])
testingData <- as.data.frame(tempDataFrame[2])
rm(tempDataFrame)

install.packages("kernlab")
library("kernlab")

install.packages("e1071")
library("e1071")

install.packages("gridExtra")
library("gridExtra")

aim <- "willRecommend"

# This function compares the accuracy of a KSVM, an SVM and a Naive Bayes
# model and returns the nameof the appropriate model and its accuracy

  ############ KSVM Model
  ksvmModel <- ksvm(willRecommend~ ., data = trainingData, kernel = "rbfdot",
                    kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)
  ksvmModel
  
  ksvmPrediction <- predict(ksvmModel, testingData, type = "response")
  ksvmPrediction
  
  plot(ksvmPrediction)
  
  newDf1 = data.frame(v1=testingData$willRecommend, v2=ksvmPrediction)
  newDf1
  
  ksvmError <- sum(testingData[,aim] != ksvmPrediction) * 100 /length(ksvmPrediction)
  ksvmError
  
  #Error is only 20.24902%
  
  ############ SVM Model
  svmModel <- svm(willRecommend ~ ., data = trainingData, kernel = "linear",
                  kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)
  svmModel
  svmPrediction <- predict(svmModel, testingData, type = "response")
  svmPrediction
  
  plot(svmPrediction)
  
  newDf2 = data.frame(v1=testingData$willRecommend, v2=svmPrediction)
  newDf2
  
  svmError <- sum(testingData[,aim] != svmPrediction) * 100 /length(svmPrediction)
  svmError
  
  # Error is 19.39712% by SVM 
  
  ############ Naive Bayes Model
  nbModel <- naiveBayes(trainingData[,aim] ~ ., data = trainingData, kernel = "linear",
                        kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)
  nbModel
  
  nbPrediction <- predict(nbModel, testingData, type = "class")
  nbPrediction
  
  plot(nbPrediction)
  newDf3 = data.frame(v1=testingData$willRecommend, v2=nbPrediction)
  newDf3
  
  nbError <- sum(testingData[,aim] != nbPrediction) * 100 /length(nbPrediction)
  nbError
  
  #Error is 4.390564% by Naive Bayes
  
  
  #Best algorithm to pick will be Naive Bayes Model here since it has just 4.453176% error  while other have
  #more percentage of error
  
  # We will use Naive Bayes to understand importance of all attributes in prediction by removing "willRecommend" in prediction
  predictors <- colnames(trainingData)[-35]
  # Let's get the weights of the different predictors
  valueOfAttributes <- nbModel$tables
  valueOfAttributes <- t(valueOfAttributes)
  write.csv(valueOfAttributes, file='NaiveBayesValues.csv')
  
  #Observed weights to be very scattered for values

  tempDataFrame <- datasetFormation(hyattRegencyDataSetFiltered[,c(5,10:22)])
  trainingData <- as.data.frame(tempDataFrame[1])
  testingData <- as.data.frame(tempDataFrame[2])
  rm(tempDataFrame)
  
  ############ Linear Model
  lmodel <- lm(as.numeric(Likelihood_Recommend_H) ~ ., data = trainingData)
  summary(lmodel)
  plot(lmodel)
  lPred <- predict(lmodel, testingData, type = "response")
  lPred
 
  rootMeanSquareErrorLM <- mean((testingData$Likelihood_Recommend_H - lPred) ^ 2)
  rootMeanSquareErrorLM
  
  #Adjusted R-squared value = 0.6139 <<<1 and hence this model is not useful 
  #Residual Standard Error = 1.192 on 3027 degrees of freedom
  
  #Hence Linear Model is not useful here and automatically dumped
  
  #####################################################
  #Checking Guest_country and NPS
  
  guestslikelihoodCountrywise<- sqldf("select AVG(Likelihood_Recommend_H) as avgLikelihood, Count(X), GUEST_COUNTRY_R from  hyattRegencyDataSetFiltered GROUP BY GUEST_COUNTRY_R ORDER BY avgLikelihood")
  guestslikelihoodCountrywise
  
  #avgLikelihood below 7 is just for 5 customers which is very small compared 4600 customers present in database and hence customer's country 
  #doensn't play any role in determing good NPS_SCORE
  
  
  #######################Age and Gender , Tanay is doing
  
  
  #Yes and No factors
  #Remove this one, all are yes
  avgBusinessCenterNPS<- tapply(hyattRegencyDataSetFiltered$Likelihood_Recommend_H, hyattRegencyDataSetFiltered$Business.Center_PL, mean)
  avgBusinessCenterNPS
  
  
  #Convention_PL
  conventionNPS<- tapply(hyattRegencyDataSetFiltered$Likelihood_Recommend_H, hyattRegencyDataSetFiltered$Convention_PL, mean)
  conventionNPS
  
  #Remove it , all are yes
  dryCleaningNPS<- tapply(hyattRegencyDataSetFiltered$Likelihood_Recommend_H, hyattRegencyDataSetFiltered$Dry.Cleaning_PL, mean)
  dryCleaningNPS
  
  
  #Golf
  avgGolfNPS<- tapply(hyattRegencyDataSetFiltered$Likelihood_Recommend_H, hyattRegencyDataSetFiltered$Golf_PL, mean)
  avgGolfNPS
  
  ######################
  #Write sql query to see how many business class played golf and how many didnt and how many overall played golf
  golfPlayedCount <- sqldf("select Count(Golf_PL), POV_CODE_C from  hyattRegencyDataSetFiltered GROUP BY POV_CODE_C")
  golfPlayedCount
  
  golfPlayedNPSscore <- sqldf("select Count(Golf_PL) FROM hyattRegencyDataSetFiltered WHERE Golf_PL='N'")
  golfPlayedNPSscore
  
  golfPlayedNPSscore2 <- sqldf("select Count(Golf_PL), CITY_PL FROM hyattRegencyDataSetFiltered GROUP BY CITY_PL HAVING Golf_PL='N'")
  golfPlayedNPSscore2
  
  
  #Laundry_PL
  laundryNPS<- tapply(hyattRegencyDataSetFiltered$Likelihood_Recommend_H, hyattRegencyDataSetFiltered$Laundry_PL, mean)
  laundryNPS
   
  #Limo.Service_PL  
  limoNPS<- tapply(hyattRegencyDataSetFiltered$Likelihood_Recommend_H, hyattRegencyDataSetFiltered$Limo.Service_PL, mean)
  limoNPS
  
  #Regency.Grand.Club_PL
  regencyGrandNPS<- tapply(hyattRegencyDataSetFiltered$Likelihood_Recommend_H, hyattRegencyDataSetFiltered$Regency.Grand.Club_PL, mean)
  regencyGrandNPS

  #Resort_PL
  resortNPS<- tapply(hyattRegencyDataSetFiltered$Likelihood_Recommend_H, hyattRegencyDataSetFiltered$Resort_PL, mean)
  resortNPS

  #Self.Parking_PL ------ Important
  selfParkingNPS<- tapply(hyattRegencyDataSetFiltered$Likelihood_Recommend_H, hyattRegencyDataSetFiltered$Self.Parking_PL, mean)
  selfParkingNPS
  
  #Y -NPS - 8.7444559
  #N- NPS- 7.841860
  
  #Shuttle.Service_PL
  shuttleNPS<- tapply(hyattRegencyDataSetFiltered$Likelihood_Recommend_H, hyattRegencyDataSetFiltered$Shuttle.Service_PL, mean)
  shuttleNPS

  #Spa_PL
  spaNPS<- tapply(hyattRegencyDataSetFiltered$Likelihood_Recommend_H, hyattRegencyDataSetFiltered$Spa_PL, mean)
  spaNPS
  
  #Valet.Parking_PL 
  valetNPS<- tapply(hyattRegencyDataSetFiltered$Likelihood_Recommend_H, hyattRegencyDataSetFiltered$Valet.Parking_PL, mean)
  valetNPS
  
  
  #Booking_Channel
  channelNPS<- tapply(hyattRegencyDataSetFiltered$Likelihood_Recommend_H, hyattRegencyDataSetFiltered$Booking_Channel, mean)
  channelNPS
  
  #City_PL
  cityNPS<- tapply(hyattRegencyDataSetFiltered$Likelihood_Recommend_H, hyattRegencyDataSetFiltered$City_PL, mean)
  cityNPS
  
  cityNPS<- tapply(hyattRegencyDataSetFiltered$Likelihood_Recommend_H, hyattRegencyDataSetFiltered$City_PL, median)
  cityNPS
  
  cityNPS<- tapply(hyattRegencyDataSetFiltered$X, hyattRegencyDataSetFiltered$City_PL, length)
  cityNPS
  
  #Dallas has least Likelihood to recommend, maybe there is some problem in hyatt regency at dallas
  
  
  #Clublounge_Used_H
  clubUsedNPS<- tapply(hyattRegencyDataSetFiltered$Likelihood_Recommend_H, hyattRegencyDataSetFiltered$Clublounge_Used_H, mean)
  clubUsedNPS

  #Spa_Used_H
  spaUsedNPS<- tapply(hyattRegencyDataSetFiltered$Likelihood_Recommend_H, hyattRegencyDataSetFiltered$Spa_Used_H, mean)
  spaUsedNPS

  #Age_Range_H
  ageRangeNPS<- tapply(hyattRegencyDataSetFiltered$Likelihood_Recommend_H, hyattRegencyDataSetFiltered$Age_Range_H, mean)
  ageRangeNPS
  
  ageRangeNPS<- tapply(hyattRegencyDataSetFiltered$Likelihood_Recommend_H, hyattRegencyDataSetFiltered$Age_Range_H, median)
  ageRangeNPS
  
  ageRangeNPS<- tapply(hyattRegencyDataSetFiltered$Likelihood_Recommend_H, hyattRegencyDataSetFiltered$Age_Range_H, length)
  ageRangeNPS
  
  #NPS is low for age 26-35 , 36-45 with overall  , given customers count is similar to other ranges, 66-75 has 368 records and highest score
  
  #Gender_H
  genderNPS<- tapply(hyattRegencyDataSetFiltered$Likelihood_Recommend_H, hyattRegencyDataSetFiltered$Gender_H, mean)
  genderNPS
  
  genderNPS<- tapply(hyattRegencyDataSetFiltered$Likelihood_Recommend_H, hyattRegencyDataSetFiltered$Gender_H, length)
  genderNPS
  
  genderNPS<- tapply(hyattRegencyDataSetFiltered$Likelihood_Recommend_H, hyattRegencyDataSetFiltered$Gender_H, median)
  genderNPS
  #NPS for male is less and customers count are almost equal
  
  #MEMBER_STATUS_R
  memberStatusNPS<- tapply(hyattRegencyDataSetFiltered$Likelihood_Recommend_H, hyattRegencyDataSetFiltered$MEMBER_STATUS_R, mean)
  memberStatusNPS
  
  #LENGTH_OF_STAY_C
  lengthStayNPS<- tapply(hyattRegencyDataSetFiltered$Likelihood_Recommend_H, hyattRegencyDataSetFiltered$LENGTH_OF_STAY_C, mean)
  lengthStayNPS
  
  lengthStayNPS<- tapply(hyattRegencyDataSetFiltered$Likelihood_Recommend_H, hyattRegencyDataSetFiltered$LENGTH_OF_STAY_C, length)
  lengthStayNPS
  
  #Calculating percentage of detractors, promoters and passives
  detractors<- subset(hyattRegencyDataSetFiltered,Likelihood_Recommend_H < 7)
  length(detractors$X)
  promoters<- subset(hyattRegencyDataSetFiltered,Likelihood_Recommend_H > 8)
  length(promoters$X)
  passives <- subset(hyattRegencyDataSetFiltered,( Likelihood_Recommend_H <= 8 & Likelihood_Recommend_H>=7))
  length(passives$X)
  
  length(hyattRegencyDataSetFiltered$X)
  labels =  c("Detractors", "Passive", "Promoters")
  slices <- c(length(detractors$X), length(passives$X),length(promoters$X))
  pct <- round(slices/sum(slices)*100,digits=2)
  labels <- paste(labels, pct) # add percents to labels 
  labels <- paste(labels,"%",sep="") # ad % to labels 
  pie(slices,labels=labels,col= rainbow(length(labels)))
  
  #Our aim is totally on these 19.06 + 11.11 % of people
  
  
  
  ######################### GGPLOT2 #######################
  #Some plots again using gglplot2
  
  
  #Length of stay vs likelihoodRecommend
  projectScatteredBehaviour = ggplot(data = hyattRegencyDataSetFiltered,aes(x=as.numeric(hyattRegencyDataSetFiltered$Likelihood_Recommend_H), y=as.numeric(hyattRegencyDataSetFiltered$LENGTH_OF_STAY_C))) + geom_point(data=hyattRegencyDataSetFiltered, aes(size=Tranquility_H,color= Condition_Hotel_H)) + labs(title = "Factors Affecting Likelihood to Recommend",y="Length of Stay",x="Likelihood To Recommend")
  projectScatteredBehaviour
  
  #this shows that likelihood_recommend_h is more when hotel condition is between 7.5-10. Tranquility is consistent for higher likelihood to recommend irrespective of length of stay
  
  #Guest Country vs Likelihood_recommend_H
  p <- ggplot(hyattRegencyDataSetFiltered, aes(GUEST_COUNTRY_R, Likelihood_Recommend_H))
  p + geom_boxplot(outlier.colour = "blue", outlier.shape = 1,fill = "white", colour = "green") +theme(axis.text.x = element_text(angle=90, hjust=1,vjust=0.3))
  
  
  #Plotting Factors like Tranquility, Guest_room_H, Condition_H, Customer_SVC_H,staff_Cared_H against Likelihood_Recommend_H
  plot1<- ggplot(data=hyattRegencyDataSetFiltered, aes(x=Tranquility_H,y=Likelihood_Recommend_H)) +
    geom_bar(stat="summary",fun.data="mean_se")  + 
    labs(y="Likelihood to Recommend",x="Tranquility")
  plot1
  
  #Its almost nearly increasing with tranquility, that means likelihood is directly linked with tranquility
  
  #Internet Satisfaction vs Likelihood
  plot2<- ggplot(data=hyattRegencyDataSetFiltered, aes(x=Internet_Sat_H,y=Likelihood_Recommend_H)) +
    geom_bar(stat="summary",fun.data="mean_se") + 
    labs(y="Likelihood to Recommend",x="Internet Satisfaction")
  plot2
  
  #It doesn't play a big role in determining the Overall Likelihood to Recommend
  
  #Guest_Room_H
  plot3<- ggplot(data=hyattRegencyDataSetFiltered, aes(x=Guest_Room_H,y=Likelihood_Recommend_H)) +
    geom_bar(stat="summary",fun.data="mean_se") + 
    labs(y="Likelihood to Recommend",x="Guest Room Satisfaction")
  plot3
  # Guest_Room_H Plays an important role, linear trend
  
  
  #Condition_Hotel_H
  plot4<- ggplot(data=hyattRegencyDataSetFiltered, aes(x=Condition_Hotel_H,y=Likelihood_Recommend_H)) +
    geom_bar(stat="summary",fun.data="mean_se") + 
    labs(y="Likelihood to Recommend",x="Condition_Hotel_H")
  plot4
  # Condition_Hotel_H Plays an important role, linear trend
  
  #Customer_SVC_H
  plot5<- ggplot(data=hyattRegencyDataSetFiltered, aes(x=Customer_SVC_H,y=Likelihood_Recommend_H)) +
    geom_bar(stat="summary",fun.data="mean_se") + 
    labs(y="Likelihood to Recommend",x="Customer_SVC_H")
  plot5
  # Customer_SVC_H Plays an important role, linear trend
  
  
  #Staff_Cared_H
  plot6<- ggplot(data=hyattRegencyDataSetFiltered, aes(x=Staff_Cared_H,y=Likelihood_Recommend_H)) +
    geom_bar(stat="summary",fun.data="mean_se") + 
    labs(y="Likelihood to Recommend",x="Staff_Cared_H")
  plot6
  # Staff_Cared_H Plays an important role, linear trend
  
  
  #Check_In_H
  plot7<- ggplot(data=hyattRegencyDataSetFiltered, aes(x=Check_In_H,y=Likelihood_Recommend_H)) +
    geom_bar(stat="summary",fun.data="mean_se") + 
    labs(y="Likelihood to Recommend",x="Check_In_H")
  plot7
  # Check_In_H is not showing a proper trend and might not be involved directly in increasing likelihood_recommend_h
  
  
  #F.B_Overall_Experience_H
  plot8<- ggplot(data=hyattRegencyDataSetFiltered, aes(x=F.B_Overall_Experience_H,y=Likelihood_Recommend_H)) +
    geom_bar(stat="summary",fun.data="mean_se") + 
    labs(y="Likelihood to Recommend",x="F.B_Overall_Experience_H")
  plot8
  # F.B_Overall_Experience_H Plays#Step 4- Look at all the data via a Heatmap
  
  
  install.packages("reshape2")
  library(reshape2)
  
  
  #Reusing modified Dataset named dfAirQualityNew formed above by melt function, putting y=variable
  # and x= Date, fill will show value which is value with each variable
  #Variable = O
  
  hyattHeatMapData <- hyattRegencyDataSetFiltered[,c(2,15,17,18,21,22)]
  hyattHeatMapData<- melt(hyattHeatMapData, measure.vars = c("Guest_Room_H", "Condition_Hotel_H","Customer_SVC_H","Check_In_H",'F.B_Overall_Experience_H'))
  
  hyattHeatMapData$CHECK_IN_DATE_C <- as.Date(hyattHeatMapData$CHECK_IN_DATE_C)
  str( hyattHeatMapData$CHECK_IN_DATE_C )
  heatMap <- ggplot(hyattHeatMapData, aes(x=CHECK_IN_DATE_C, y=variable, fill=value)) + geom_tile()
  
  heatMap <- heatMap +xlab("Dates in 2014") + ylab("Deciding Factors for customers retention") + ggtitle("Factors affecting NPS_Score over period of months")
  
  heatMap
  
  
  #Check In service has improved over period of time and Customer_SVC has gone down and remain bad
  #F.B service has improved over time Condition of Hotel has remain bad over period of time
  #Also, the guest_room_service has varied over period of time but has not been top notch
  
  hyattHeatMapData <- hyattRegencyDataSetFiltered[,c(2,27,32)]
  hyattHeatMapData<- melt(hyattHeatMapData, measure.vars = c("Golf_PL",'Self.Parking_PL'))
  
  hyattHeatMapData$CHECK_IN_DATE_C <- as.Date(hyattHeatMapData$CHECK_IN_DATE_C)
  str( hyattHeatMapData$CHECK_IN_DATE_C )
  heatMap <- ggplot(hyattHeatMapData, aes(x=CHECK_IN_DATE_C, y=variable, fill=value)) + geom_tile()
  
  heatMap <- heatMap +xlab("Dates in 2014") + ylab("Deciding Factors for customers retention") + ggtitle("Factors affecting NPS_Score over period of months")
  
  heatMap
  
  #Golf Facilities and Self Parking facilities are staggering by the end of the year
  
  
  #Linear Model for all the above observed factors
  
  ####################Linear Modeling############################# on the above factors
  #Likelihood on F.B_Overall_Experience_H + Condition_Hotel_H
  Likelihood_lm <- lm(formula = Likelihood_Recommend_H ~ F.B_Overall_Experience_H + Condition_Hotel_H, data= hyattRegencyDataSetFiltered)
  plot(Likelihood_lm)
  summary(Likelihood_lm)
  #R^2 value= 0.4837
  
  ##############F.B_Overall_Experience_H
  Likelihood_lm <- lm(formula = Likelihood_Recommend_H ~ F.B_Overall_Experience_H, data= hyattRegencyDataSetFiltered)
  plot(Likelihood_lm)
  summary(Likelihood_lm)
  #R^2 value is very low = 0.08294
  
  ############Condition_Hotel_H
  Likelihood_lm <- lm(formula = Likelihood_Recommend_H ~ Condition_Hotel_H, data= hyattRegencyDataSetFiltered)
  plot(Likelihood_lm)
  summary(Likelihood_lm)
  #R^2 value is 0.4586  <--- good
  
  ############Staff_Cared_H
  Likelihood_lm <- lm(formula = Likelihood_Recommend_H ~ Staff_Cared_H, data= hyattRegencyDataSetFiltered)
  plot(Likelihood_lm)
  summary(Likelihood_lm)
  #R^2 value is 0.2025
  
  ############Customer_SVC_H
  Likelihood_lm <- lm(formula = Likelihood_Recommend_H ~ Customer_SVC_H, data= hyattRegencyDataSetFiltered)
  plot(Likelihood_lm)
  summary(Likelihood_lm)
  #R^2 value is 0.437  <--- good
  
  
  ############Guest_Room_H
  Likelihood_lm <- lm(formula = Likelihood_Recommend_H ~ Guest_Room_H, data= hyattRegencyDataSetFiltered)
  plot(Likelihood_lm)
  summary(Likelihood_lm)
  #R^2 value is 0.430  <--- good
  
  ############Tranquility_H
  Likelihood_lm <- lm(formula = Likelihood_Recommend_H ~ Tranquility_H, data= hyattRegencyDataSetFiltered)
  plot(Likelihood_lm)
  summary(Likelihood_lm)
  #R^2 value is 0.149  <--- less
  
  ############Customer_SVC_H + Condition_Hotel_H
  Likelihood_lm <- lm(formula = Likelihood_Recommend_H ~ Customer_SVC_H + Condition_Hotel_H, data= hyattRegencyDataSetFiltered)
  plot(Likelihood_lm)
  summary(Likelihood_lm)
  #R^2 value is 0.5748  <--- good
  
  ############Customer_SVC_H + Guest_Room_H
  Likelihood_lm <- lm(formula = Likelihood_Recommend_H ~ Customer_SVC_H + Guest_Room_H, data= hyattRegencyDataSetFiltered)
  plot(Likelihood_lm)
  summary(Likelihood_lm)
  #R^2 value is 0.5744  <--- good
  
  ############Customer_SVC_H + Guest_Room_H
  Likelihood_lm <- lm(formula = Likelihood_Recommend_H ~ Customer_SVC_H + Tranquility_H +Condition_Hotel_H, data= hyattRegencyDataSetFiltered)
  plot(Likelihood_lm)
  summary(Likelihood_lm)
  #R^2 value is 0.5865  <--- good
  
  #Overall Linear Modeling based on important factors identified
  Likelihood_lm <- lm(formula = Likelihood_Recommend_H ~ F.B_Overall_Experience_H + Condition_Hotel_H+Staff_Cared_H+Customer_SVC_H+Guest_Room_H+Tranquility_H, data= hyattRegencyDataSetFiltered)
  plot(Likelihood_lm)
  summary(Likelihood_lm)
  #R^2 value = 0.624 ---> combination of these attributes is strong
  
  
  #Important factors are Customer_SVC_H, Condition_Hotel_H, Guestroom_H
  
  
  #Creating a subset again
  hyattRegencyDataSetFiltered2 = subset(hyattRegencyDataSetFiltered, select = -c(Business.Center_PL,Convention_PL,Dry.Cleaning_PL,Laundry_PL,
                                                                                 Limo.Service_PL,Regency.Grand.Club_PL,Resort_PL,Shuttle.Service_PL,
                                                                                 Spa_PL,Valet.Parking_PL,Booking_Channel,NPS_Type,Clublounge_Used_H,Spa_Used_H,REVENUE_USD_R,MEMBER_STATUS_R,
                                                                                 GUEST_COUNTRY_R,PMS_FOOD_BEVERAGE_REV_USD_C,ADULT_NUM_C,LENGTH_OF_STAY_C,CHECK_IN_DATE_C, City_PL,POV_CODE_C, Gender_H, Check_In_H, Internet_Sat_H) )
  
  View(hyattRegencyDataSetFiltered2)

 
  #Use Apriori on this
  install.packages('arules')
  library(arules)
  install.packages('arulesViz')
  library(arulesViz)
  
  str(hyattRegencyDataSetFiltered2)
  
  #taking detractors to understand the data better
  #hyattDetractorData<- hyattRegencyDataSetFiltered2[which(hyattRegencyDataSetFiltered2$Likelihood_Recommend_H < 8,arr.ind = TRUE),]
  hyattDataFinal <-subset(hyattRegencyDataSetFiltered2, select = -c(X) )
  head(hyattDataFinal)
  
  ################Change Range of values   >=8 and <=10 = Good  :  <8 and >=7 = Average : < 7 Bad 
  hyattDataFinal <- within(hyattDataFinal, Guest_Room_H[Guest_Room_H >=0 & Guest_Room_H < 7] <- -1)
  hyattDataFinal <- within(hyattDataFinal, Guest_Room_H[Guest_Room_H >8 & Guest_Room_H <=10] <- 1)
  hyattDataFinal <- within(hyattDataFinal, Guest_Room_H[Guest_Room_H >=7 & Guest_Room_H <= 8] <-  0)
 
  
  hyattDataFinal <- within(hyattDataFinal, Tranquility_H[Tranquility_H >=0 & Tranquility_H < 7] <- -1)
  hyattDataFinal <- within(hyattDataFinal, Tranquility_H[Tranquility_H >8 & Tranquility_H <=10] <- 1)
  hyattDataFinal <- within(hyattDataFinal, Tranquility_H[Tranquility_H >=7 & Tranquility_H <=8] <-  0)
  
  hyattDataFinal <- within(hyattDataFinal, Condition_Hotel_H[Condition_Hotel_H >=0 & Condition_Hotel_H < 7] <- -1)
  hyattDataFinal <- within(hyattDataFinal, Condition_Hotel_H[Condition_Hotel_H >8 & Condition_Hotel_H <=10] <- 1)
  hyattDataFinal <- within(hyattDataFinal, Condition_Hotel_H[Condition_Hotel_H >=7 & Condition_Hotel_H <=8] <-  0)
  
  hyattDataFinal <- within(hyattDataFinal, Customer_SVC_H[Customer_SVC_H >=0 & Customer_SVC_H < 7] <- -1)
  hyattDataFinal <- within(hyattDataFinal, Customer_SVC_H[Customer_SVC_H >8 & Customer_SVC_H <=10] <- 1)
  hyattDataFinal <- within(hyattDataFinal, Customer_SVC_H[Customer_SVC_H >=7 & Customer_SVC_H <=8] <-  0)
  
  hyattDataFinal <- within(hyattDataFinal, Staff_Cared_H[Staff_Cared_H >=0 & Staff_Cared_H < 7] <- -1)
  hyattDataFinal <- within(hyattDataFinal, Staff_Cared_H[Staff_Cared_H >8 & Staff_Cared_H <=10] <- 1)
  hyattDataFinal <- within(hyattDataFinal, Staff_Cared_H[Staff_Cared_H >=7 & Staff_Cared_H <=8] <-  0)
  
  hyattDataFinal <- within(hyattDataFinal, F.B_Overall_Experience_H[F.B_Overall_Experience_H >=0 & F.B_Overall_Experience_H < 7] <- -1)
  hyattDataFinal <- within(hyattDataFinal, F.B_Overall_Experience_H[F.B_Overall_Experience_H >8 & F.B_Overall_Experience_H <=10] <- 1)
  hyattDataFinal <- within(hyattDataFinal, F.B_Overall_Experience_H[F.B_Overall_Experience_H >=7 & F.B_Overall_Experience_H <=8] <-  0)
  
  
  
  #Since the columns selected were either characters or integers, it was necessary to convert them as factors in order to perform apriori
  hyattDataFinal$Age_Range_H <- as.factor(hyattDataFinal$Age_Range_H)
  hyattDataFinal$Guest_Room_H <- as.factor(hyattDataFinal$Guest_Room_H)
  hyattDataFinal$Tranquility_H <- as.factor(hyattDataFinal$Tranquility_H)
  hyattDataFinal$Condition_Hotel_H <- as.factor(hyattDataFinal$Condition_Hotel_H)
  hyattDataFinal$Customer_SVC_H <- as.factor(hyattDataFinal$Customer_SVC_H)
  hyattDataFinal$Staff_Cared_H <- as.factor(hyattDataFinal$Staff_Cared_H)
  hyattDataFinal$F.B_Overall_Experience_H <- as.factor(hyattDataFinal$F.B_Overall_Experience_H)
  hyattDataFinal$Self.Parking_PL <- as.factor(hyattDataFinal$Self.Parking_PL)
  hyattDataFinal$Golf_PL <- as.factor(hyattDataFinal$Golf_PL)
  
  #Removing Likelihood_recommend_h because we have will recommend field based on thats
  hyattDataFinal <-subset(hyattDataFinal, select = -c(Likelihood_Recommend_H) )
  
  
  #Performing Data association rules using apriori 
  #Having the confidence as 0.8 and support as 0.01 gives the rules which are likely to predict one another
  rulesSet <- apriori(hyattDataFinal,parameter=list(support=0.01, confidence=0.8),
                     appearance=list(default='lhs', rhs=('willRecommend=-1')))
  goodRules <- rulesSet[quality(rulesSet)$lift>8]
  
  goodRules <- sort(goodRules,by='lift',decreasing=T)
  goodRules
  inspect(head(goodRules, 10)) 
  #Plotting the rule1
  plot(rulesSet)  
  ################# Top 10 rules for detractors
  ## for Detractors
  #{Tranquility_H=-1,Customer_SVC_H=-1} 8.658487    51
  #{Guest_Room_H=-1,Condition_Hotel_H=-1,Customer_SVC_H=-1} 8.417517    87
  #{Guest_Room_H=-1,Customer_SVC_H=-1,Golf_PL=Y} 8.387999    55
  #{Guest_Room_H=-1, Customer_SVC_H=-1,Golf_PL=Y, Self.Parking_PL=Y} 8.387999   55
  #{Guest_Room_H=-1, Condition_Hotel_H=-1, Customer_SVC_H=-1, F.B_Overall_Experience_H=1} 8.346004    64
 
  
  #Having the confidence as 0.8 and support as 0.01 gives the rules which are likely to predict one another
  #For Passives
   rulesSet <- apriori(hyattDataFinal,parameter=list(support=0.01, confidence=0.8),
                      appearance=list(default='lhs', rhs=('willRecommend=0')))
  goodRules <- rulesSet[quality(rulesSet)$lift>4]
  
  goodRules <- sort(goodRules,by='lift',decreasing=T)
  goodRules
  inspect(head(goodRules, 10)) 
  #Plotting the rule1
  plot(rulesSet)  
  
  ####Passives Rules
  #{Guest_Room_H=0,Tranquility_H=0,Condition_Hotel_H=0,Customer_SVC_H=0,Staff_Cared_H=0, Self.Parking_PL=Y} 4.421862    59
  #{Guest_Room_H=0,Tranquility_H=0, Condition_Hotel_H=0,Customer_SVC_H=0, Staff_Cared_H=0}  4.371898    60
  #{Guest_Room_H=0,Condition_Hotel_H=0,Customer_SVC_H=0,Staff_Cared_H=0,Self.Parking_PL=Y} 4.362694    79
  #{Condition_Hotel_H=0,Customer_SVC_H=0,Staff_Cared_H=0,Golf_PL=N,Self.Parking_PL=Y}  4.357078 49
  #{Guest_Room_H=0,Tranquility_H=0,Condition_Hotel_H=0,Staff_Cared_H=0,Self.Parking_PL=Y}  4.350571    68
  
  #For Promoters
  rulesSet <- apriori(hyattDataFinal,parameter=list(support=0.01, confidence=0.8),
                      appearance=list(default='lhs', rhs=('willRecommend=1')))
  goodRules <- rulesSet[quality(rulesSet)$lift>1.4]
  
  goodRules <- sort(goodRules,by='lift',decreasing=T)
  goodRules
  inspect(head(goodRules, 10)) 
  #Plotting the rule1
  plot(rulesSet)  
  
  # Promoters
  #{Age_Range_H=46-55,Guest_Room_H=1,Tranquility_H=1,Customer_SVC_H=1,Staff_Cared_H=1, F.B_Overall_Experience_H=0} => 1.402309    47
  #{Age_Range_H=46-55,Guest_Room_H=1,Tranquility_H=1,Customer_SVC_H=1,Staff_Cared_H=1, F.B_Overall_Experience_H=0,Self.Parking_PL=Y} 1.402309    47
  #{Age_Range_H=46-55, Tranquility_H=1, Condition_Hotel_H=1,Customer_SVC_H=1,Staff_Cared_H=1, F.B_Overall_Experience_H=0} => 1.401674    46
  #{Age_Range_H=46-55, Guest_Room_H=1, Tranquility_H=1, Condition_Hotel_H=1, Customer_SVC_H=1,Staff_Cared_H=1,F.B_Overall_Experience_H=0} => 1.401674    46
  #{Age_Range_H=46-55,Tranquility_H=1,Condition_Hotel_H=1,Customer_SVC_H=1,Staff_Cared_H=1, F.B_Overall_Experience_H=0, Self.Parking_PL=Y} 1.401674    46
  
  
  