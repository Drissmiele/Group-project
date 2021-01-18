####DataOG

# creation of DataOG with all variables

Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep = ";")
Data1$APG <- NA 
A <- Data1$Date == "1"
Data1[A, "APG"] <- ((log(Data1$aphid_live[A] + 1) - log(Data1$aphidsinoculated_init[A] + 1))/10)
A <- Data1$Date == "2"
Data1[A, "APG"] <- ((log(Data1$aphid_live[A] + 1) - log(Data1$aphidsinoculated_init[A] + 1))/20)
A <- Data1$Date == "3"
Data1[A, "APG"] <- ((log(Data1$aphid_live[A] + 1) - log(Data1$aphidsinoculated_init[A] + 1))/30)
DataOG <- Data1

parasitism_rate <- (Data1$aphid_parasitized/Data1$aphid_live + Data1$aphid_parasitized)
DataOG$parasitism_rate <- parasitism_rate

syrphid_fraction <- (Data1$syrphidl_p / ((Data1$aphid_live) + (Data1$syrphidl_p)))
DataOG$syrphid_fraction <- syrphid_fraction

# conversion of character vectors into numeric vectors !

B <- lapply(DataOG[c(2,3,11,13)], as.factor)
B <- lapply(B, as.numeric)
DataOG[c(2,3,11,13)] <- B


#convert NaN to 0
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

DataOG[is.nan(DataOG)] <- 0


#convert inf to 1
is.infinite.data.frame <- function(y)
  do.call(cbind, lapply(y, is.infinite))

DataOG[is.infinite(DataOG)] <- 1


#linear model arranged from complex to simple
lmPR_M6 <- lm(DataOG$parasitism_rate ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I((DataOG$Field_Mgmt)*(DataOG$Date)) + I((DataOG$Pt.seminatural)*(DataOG$Date)) + I((DataOG$Treatment)*(DataOG$Date) + I((DataOG$Pt.seminatural)*(DataOG$Treatment))))
lmPR_M2 <- lm(DataOG$parasitism_rate ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I((DataOG$Field_Mgmt)*(DataOG$Date)) + I((DataOG$Pt.seminatural)*(DataOG$Date)) + I((DataOG$Field_Mgmt)*(DataOG$Treatment)))
lmPR_M3<- lm(DataOG$parasitism_rate ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I((DataOG$Field_Mgmt)*(DataOG$Date)) + I((DataOG$Pt.seminatural)*(DataOG$Treatment)))
lmPR_M7<- lm(DataOG$parasitism_rate ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I((DataOG$Field_Mgmt)*(DataOG$Date)) + I((DataOG$Pt.seminatural)*(DataOG$Date)))
lmPR_M5 <- lm(DataOG$parasitism_rate ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I((DataOG$Pt.seminatural)*(DataOG$Date)) + I((DataOG$Pt.seminatural)*(DataOG$Treatment)))
lmPR_M4 <- lm(DataOG$parasitism_rate ~ DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I((DataOG$Pt.seminatural)*(DataOG$Date)) + I((DataOG$Pt.seminatural)*(DataOG$Treatment)))

anova(lmPR_M4, lmPR_M5)
#M5:  < 2.2e-16 ***
anova(lmPR_M5, lmPR_M7)
anova(lmPR_M7, lmPR_M3)
anova(lmPR_M3, lmPR_M2)
anova(lmPR_M2, lmPR_M6)

#For all these anovas, no p value was given. The models are not nested. Only the AIC index can be used.

#AIC
AIC(lmPR_M6, lmPR_M2, lmPR_M3,lmPR_M7, lmPR_M5,lmPR_M4)

# df      AIC
# lmPR_M6  9 3627.890 #rank 2
# lmPR_M2  9 3683.616 #rank 3
# lmPR_M3  8 3623.658 #rank 1
# lmPR_M7  8 3683.080 #rank 4
# lmPR_M5  8 3692.778 #rank 5
# lmPR_M4  7 3814.166 #rank 6

#lmPR_M3: best model
