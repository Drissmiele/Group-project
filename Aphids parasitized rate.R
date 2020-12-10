###############################
#import data file
library(readr)
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep =";")

# created a new variable called plant_ID that is highly specific (plant number)
Data1$plant_ID <- paste(Data1$Treatment, Data1$Plant, sep ="_")

#selecting in Data 2 all the rows with buff = 100
BUFF <- Data1[Data1$BUFF_DIST =="100",] 
View(BUFF)

#calculate the aphids population growth (n = 1271)
logNaphids_pr <- log(BUFF$aphid_parasitized+1) - log(BUFF$aphidsinoculated_init+1) 
APG_pr <- logNaphids_pr/10

#make new data frame which include aphids population growth rate
BUFF_APG_pr <- data.frame(BUFF, APG_pr)
View(BUFF_APG_pr)


