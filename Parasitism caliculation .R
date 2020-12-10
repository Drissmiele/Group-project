###############################
#import data file
library(readr)
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep =";")

# created a new variable called plant_ID that is highly specific (plant number)
Data1$plant_ID <- paste(Data1$Treatment, Data1$Plant, sep ="_")

#selecting in Data 2 all the rows with buff = 100
BUFF <- Data1[Data1$BUFF_DIST =="100",] 
View(BUFF)


# Parasitism rates
# (the ratio of parasitized to all aphids) 

#calculate the aphids parasitism rate (n = 1271)
parasitism_rate　<- BUFF$aphid_parasitized/(BUFF$aphid_live)
BUFF$parasitism_rate <- parasitism_rate

#make new data frame which include aphids population growth rate
BUFF_APG_pr <- data.frame(BUFF, APG_pr)
View(BUFF_APG_pr)


