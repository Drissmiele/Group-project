library(readr)
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep =";")

# created a new variable called plant_ID that is highly specific (plant number)
Data1$plant_ID <- paste(Data1$Treatment, Data1$Plant, sep ="_")

#selecting in Data 2 all the rows with buff = 100
BUFF <- Data1[Data1$BUFF_DIST =="100",] 
View(BUFF)


#calculate the aphids parasitism rate (n = 1271)
parasitism_rate　<- BUFF$aphid_parasitized/(BUFF$aphid_live)
BUFF$parasitism_rate <- parasitism_rate

# creating the syrphid fraction variable 

syrphid_fraction <- (BUFF$syrphidl_p/(BUFF$aphid_live + BUFF$syrphidl_p))
BUFF$syrphid_fraction <- syrphid_fraction


