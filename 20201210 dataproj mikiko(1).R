#import data file
library(readr)
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep =";")
View(Data1)
str(Data1)
summary(Data1)

#check variables name
names(Data1)

#remove variables that we dont need: plot_ID, Treatment, Field_Mgmt and crop type
Variables <- names(Data1)[c(-1,-2, -11, -13, -14)]
Variables
Data2 <- Data1[Variables]
Data2 <- data.frame(Data2)

Data2$plant_ID <- paste(Data2$Treatment, Data2$Plant, sep ="_") # created a new variable called plant_ID that is highly specific (plant number)
unique(Data2$plant_ID)
Data2 <- Data2[Data2$BUFF_DIST =="100",] #selecting in Data 2 all the rows with buff = 100
View(Data2)


###dataframe for sampling date1 
DATE1 <- Data2[Data2$Date == "1", ]
summary(DATE1)
View(DATE1)

#calculate the aphids population growth at date1
logNaphids_D1 <- log(DATE1$aphid_live+1) - log(DATE1$aphidsinoculated_init+1) 
APG_D1 <- logNaphids_D1/10


####dataframe for sampling date2
DATE2 <- Data2[Data2$Date == "2", ]
summary(DATE2)
View(DATE2)

#calculate the aphids population growth at date2
logNaphids_D2 <- log(mean(DATE2$aphid_live)+1) - log(mean(DATE2$aphidsinoculated_init)+1) 
APG_D2 <- logNaphids_D2/10


###dataframe for sampling date3
DATE3 <- Data2[Data2$Date == "3", ]
summary(DATE3)
View(DATE3)

#calculate the aphids population growth at date3
logNaphids_D3 <- log(mean(DATE3$aphid_live)+1) - log(mean(DATE3$aphidsinoculated_init)+1) 
APG_D3 <- logNaphids_D3/10

#Package that we use
#Aphid population growth was modelled using a linear mixed model
#install.packages("nlme")
library(nlme)
#install.packages("lme4")
library(lme4)

#dataframe for seminatural habitat and aphids population growth at date1 
SA_D1 <- data.frame(DATE1$Pt.seminatura, APG_D1)
View(SA_D1)

#barplot for % Seminatural habitat and APhid population growth
library(ggplot2)
ggplot(SA_D1, 
       aes(x = SA_D1$DATE1.Pt.seminatura, y = SA_D1$APG_D1)) +
                  geom_smooth() 
     

#lmer for plot (not yet, trying)
lmer(SA_D1$DATE1.Pt.seminatura~ SA_D1$APG_D1, data = SA_D1)

