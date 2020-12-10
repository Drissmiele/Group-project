#import data file
library(readr)
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep =";")
View(Data1)
str(Data1)
summary(Data1)

#check variables name
names(Data1)

Data2$plant_ID <- paste(Data2$Treatment, Data2$Plant, sep ="_") # created a new variable called plant_ID that is highly specific (plant number)
unique(Data2$plant_ID)
Data2 <- Data2[Data2$BUFF_DIST =="100",] #selecting in Data 2 all the rows with buff = 100

