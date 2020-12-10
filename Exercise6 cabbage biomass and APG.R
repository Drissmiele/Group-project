#Exercise6
#Cabbage and aphids population growth rate

###############################
#import data file
library(readr)
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep =";")

# created a new variable called plant_ID that is highly specific (plant number)
Data1$plant_ID <- paste(Data1$Treatment, Data1$Plant, sep ="_")

#selecting in Data 2 all the rows with buff = 100
BUFF <- Data1[Data1$BUFF_DIST =="100",] 

#calculate the aphids population growth (n = 1271)
logNaphids_all <- log(BUFF$aphid_live+1) - log(BUFF$aphidsinoculated_init+1) 
APG_all <- logNaphids_all/10

#make new data frame which include aphids population growth rate
BUFF_APG_all <- data.frame(BUFF, APG_all)
View(BUFF_APG_all)

#variables that we need
Cabbage <- BUFF_APG_all$Biomass_fin
APG_all <- BUFF_APG_all$APG_all

A6 <- aov(Cabbage~APG_all, data = BUFF_APG_all)
summary(A6)




