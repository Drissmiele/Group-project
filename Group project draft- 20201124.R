# Data <- read.table("Data_for_project.csv", dec = ",", sep = ";")
# mean(Data_for_project$aphidsinoculated_init)

###20201124 miki script##################################
#import data file
library(readr)
Data1 <- read_csv("C:/Users/mk4rb/Desktop/Rpractice/Data for project.csv")
View(Data1)
str(Data1)
summary(Data1)

#check variables name
names(Data1)

#remove variables that we dont need: plot_ID, Treatment, Field_Mgmt and crop type
Variables <- names(Data1)[c(-1, -2, -11, -13)]
Variables
Data2 <- Data1[Variables]
Data3 <- data.frame(Data2)

Treatment_ID <- Field_Mgmt<- croptype <- character()
Plant<- aphidsinoculated_init <- aphid_live <- aphid_parasitized <- numeric()
syrphidl_p <- Biomass_fin <- cropmaturity_init <- X <- Y <- numeric()
BUFF_DIST <-  Pt.seminatural <- numeric()

###Summarizing multiple columns with dplyr 
library(dplyr)
Data3 <- Data3 %>% 
group_by(Treatment_ID, Plant) %>% 
summarise(across(everything(), list(mean)))
View(Data3)

#remove variable "Plant"???
Data3[-2]
              

#Package that we use
#Aphid population growth was modelled using a linear mixed model
install.packages("nlme")
#Parasitism rates (the ratio of parasitized to all aphids) 
#and syrphid fractions (the ratio of syrphids to total aphids + syrphids) 
#were modelled using a binomial response with logit link
install.packages("lme4")
#using sequential AICc testing with the function “dredge” 
install.packages("MuMIn")

library(nlme)
library(lme4)
library(MuMIn)