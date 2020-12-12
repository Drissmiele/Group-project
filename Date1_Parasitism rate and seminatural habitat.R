#import data file
library(readr)
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep =";")

#####################################
#If BUff_DUST is needed
#select BUFF_DUST = 100
#selecting in Data 2 all the rows with buff = 100
#BUFF <- Data1[Data1$BUFF_DIST =="100",] 
########################################

###dataframe for sampling date1 
DATE1 <- Data1[Data1$Date == "1", ]

#calculate the aphids parasitism rate (n = 1271)
parasitism_rate　<- Data1$aphid_parasitized/(Data1$aphid_live+BUFF$aphid_parasitized)
BUFF$parasitism_rate <- parasitism_rate