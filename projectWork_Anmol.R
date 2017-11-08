marchData <- read.csv("sampleMarchData.csv")
juneData <- read.csv("sampleJuneData.csv")
septemberData <- read.csv("sampleSeptemberData.csv")
decemberData <- read.csv("sampleDecemberData.csv")
fieldNames <- read.csv("majorAttributesUpdated.csv")
marchData <- marchData[,which(colnames(marchData) %in% colnames(fieldNames))]
juneData <- juneData[,which(colnames(juneData) %in% colnames(fieldNames))]
septemberData <- septemberData[,which(colnames(juneData) %in% colnames(fieldNames))]
decemberData <- decemberData[,which(colnames(decemberData) %in% colnames(fieldNames))]
View(marchData)
View(juneData)
View(septemberData)
View(decemberData)

write.csv(marchData,file= 'filteredDataMarch.csv')
write.csv(juneData,file='filteredDataJune.csv')
write.csv(septemberData,file='filteredDataSep.csv')
write.csv(decemberData,file='filteredDataDec.csv')
