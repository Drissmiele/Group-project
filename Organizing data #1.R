library(readr)
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep =";")
View(Data1)


#remove variables that we dont need: plot_ID, Treatment, Field_Mgmt and crop type
Variables <- names(Data1)[c(-1, -11, -12, -13, -14, -15, -17)]
Data2 <- Data1[Variables]

#install packages
install.packages(c("dplyr", "tidyr"), dependencies = TRUE)
library(dplyr)
library(tidyr)

# select only treatment H on plot 37 (herbivores only) 
i <- select(filter(Data2, Data2$Treatment_ID == '37_H'), aphid_live, Biomass_fin)
# Alternative way of doing it 
Data2[Data2$Treatment_ID == "37_H", c("Treatment_ID", "aphid_live", "Biomass_fin")]
# select treatment H (herbivores), live aphids and final biomass
Data2[Data2$Treatment =="H", c("Treatment", "aphid_live", "Biomass_fin")]


Data2$plant_ID <- paste(Data2$Treatment, Data2$Plant, sep ="_") # created a new variable called plant_ID that is highly specific (plant number)
unique(Data2$plant_ID) 
Data2 <- Data2[Data2$BUFF_DIST =="100",] # selecting  all the rows with buff = 100 in Data2


# separating Data2 into three data frames for each date
dframe_date1 <- data.frame(slice(Data2, 1:431))
dframe_date2 <- data.frame(slice(Data2, 432:863))
dframe_date3 <- data.frame(slice(Data2, 863:1271))
# => there is missing data

# checking for missing data
which(!dframe_date2$plant_ID %in% dframe_date3$plant_ID)
which(!dframe_date1$plant_ID %in% dframe_date2$plant_ID)
