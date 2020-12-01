library(readr)
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep =";")
View(Data1)


#remove variables that we dont need: plot_ID, Treatment, Field_Mgmt and crop type
Variables <- names(Data1)[c(-1, -11, -13)]
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
Data2 <- Data2[Data2$BUFF_DIST =="100",] #selecting in Data 2 all the rows with buff = 100
