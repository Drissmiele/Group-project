library(readr)
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep = ";")

# created a new variable called plant_ID that is highly specific (plant number)
Data1$plant_ID <- paste(Data1$Treatment, Data1$Plant, sep = "_")

#selecting in Data 2 all the rows with buff = 100
BUFF <- Data1[Data1$BUFF_DIST == "100",] 
View(BUFF)


#calculate the aphids parasitism rate (n = 1271)
parasitism_rate　<- BUFF$aphid_parasitized/(BUFF$aphid_live + BUFF$aphid_parasitized)
BUFF$parasitism_rate <- parasitism_rate

# calculating parasitism rate for the whole data set D1

parasitism_rate　<- Data1$aphid_parasitized/(Data1$aphid_live + Data1$aphid_parasitized)
Data1$parasitism_rate <- parasitism_rate

# calculating the syrphid fraction = # syrphids/(total aphids + syrphids)
syrphid_fraction <- (Data1$syrphidl_p / ((Data1$aphid_live) + (Data1$syrphidl_p)))
Data1$syrphid_fraction <- syrphid_fraction
                     