marchData <- read.csv("sampleMarchData.csv")
juneData <- read.csv("sampleJuneData.csv")
septemberData <- read.csv("sampleSeptemberData.csv")
decemberData <- read.csv("sampleDecemberData.csv")
fieldNames <- read.csv("majorAttributesUpdated.csv")
marchData <- marchData[,which(colnames(marchData) %in% colnames(fieldNames))]
juneData <- juneData[,which(colnames(juneData) %in% colnames(fieldNames))]
septemberData <- septemberData[,which(colnames(septemberData) %in% colnames(fieldNames))]
decemberData <- decemberData[,which(colnames(decemberData) %in% colnames(fieldNames))]
View(marchData)
View(juneData)
View(septemberData)
View(decemberData)

write.csv(marchData,file= 'filteredDataMarch.csv')
write.csv(juneData,file='filteredDataJune.csv')
write.csv(septemberData,file='filteredDataSep.csv')
write.csv(decemberData,file='filteredDataDec.csv')


#Comparison to shortlist Countries

#Find Highest visited Country
install.packages("ggplot2")
library(ggplot2)

install.packages("reshape2")
library(reshape2)

#For March, the top countries are:
marchdataTable <- table(marchData$Country_PL, marchData$State_PL, marchData$City_PL)
marchTableVector <- as.data.frame(marchdataTable)


headMarchData <- head(marchTableVector[order(-marchTableVector$Freq),])
usMapData<- map_data("state")
dummyDF <- data.frame(state.name, stringsAsFactors=FALSE)
dummyDF$state <- tolower(dummyDF$state.name)

map.simple <- ggplot(headMarchData, aes(map_id = tolower(Var2), fill=Freq))  
map.simple <- map.simple +geom_map(map = usMapData, color="white") 
map.simple <- map.simple +expand_limits(x = usMapData$long, y = usMapData$lat)
map.simple <- map.simple +coord_map() + ggtitle("basic map of USA")
map.simple


mapSimple <- ggplot(headMarchData, aes(map_id = Var2, fill=Freq))
mapSimple <- mapSimple + geom_map(map = usMapData, color="white")

#forming states using lat and long based on mean state income
mapSimple <- mapSimple+ expand_limits(x = usMapData$long, y = usMapData$lat)
mapSimple<- mapSimple +coord_map() + ggtitle("Map of USA for March")
mapSimple

#United States
#States- Flordia, New York,Texas, California, DC, California
#City- Orlando, New York, San Antonio, San Diego

#For June, the top countries are:
juneDataTable <- table(juneData$Country_PL, juneData$State_PL, juneData$City_PL)
juneTableVector <- as.data.frame(juneDataTable)
head(juneTableVector[order(-juneTableVector$Freq),])
#United States
#States- New York, Florida, Texas, California, California
#City- New York, Orlando, San Antonio,San Diego



#For Sep, the top countries are:

sepDataTable <- table(septemberData$Country_PL,septemberData$State_PL, septemberData$City_PL)
sepTableVector <- as.data.frame(sepDataTable)
head(sepTableVector[order(-sepTableVector$Freq),])
#United States
#States- New York, Florida, Texas, Illinois
#City- New York, Orlando, San Antonio, Chicago
#For Dec, the top countries are:

decDataTable <- table(decemberData$Country_PL,decemberData$State_PL, decemberData$City_PL)
decTableVector <- as.data.frame(decDataTable)
head(decTableVector[order(-decTableVector$Freq),])
#United States
#State- Illinois, Flordia, New York, California, Texas
#City- Chicago, Orlando, New York


#Since US is always leading, our main Aim is to consider majorly the records from United States
#Now for States, the states with highest booking considering all quarters are majorly Flordia and New York
# City involved in major amount of bookings are - New York, Orlando
#So, we will consider only orlando and New York bookings as they are the major source of revenue

#for March Data, cutting down the city to Ny and Orlando
marchDataUpdated<- marchData[which(marchData$City_PL =='New York' | marchData$City_PL =='Orlando'),]
juneDataUpdated<- juneData[which(juneData$City_PL =='New York' | juneData$City_PL =='Orlando'),]
sepDataUpdated<- septemberData[which(septemberData$City_PL =='New York' | septemberData$City_PL =='Orlando'),]
decDataUpdated<- decemberData[which(decemberData$City_PL =='New York' | decemberData$City_PL =='Orlando'),]

write.csv(marchDataUpdated,file= 'CityDataMarch.csv')
write.csv(juneDataUpdated,file='CityDataJune.csv')
write.csv(sepDataUpdated,file='CityDataSep.csv')
write.csv(decDataUpdated,file='CityDataDec.csv')

#Merging Updated data into 1 File
combinedDataSet <- rbind(marchDataUpdated, juneDataUpdated, sepDataUpdated, decDataUpdated)
write.csv(combinedDataSet,file= 'combinedDataSet.csv')
View(marchData$Country_PL)
#Comparison to shortlist cities





