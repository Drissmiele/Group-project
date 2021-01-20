#DataOG
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

parasitism_rate <- (Data1$aphid_parasitized/(Data1$aphid_live + Data1$aphid_parasitized))
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


#linear model arranged from complex to simple models
lmSF_M11 <- lm(DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural+ DataOG$Date+ DataOG$Treatment+ I(DataOG$Field_Mgmt*DataOG$Date)+ I(DataOG$Pt.seminatural*DataOG$Date) + I(DataOG$Pt.seminatural*DataOG$Treatment))
lmSF_M10 <- lm(DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural+ DataOG$Date + DataOG$Treatment + I(DataOG$Field_Mgmt*DataOG$Date) + I(DataOG$Pt.seminatural*DataOG$Treatment))
lmSF_M14 <- lm(DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date +DataOG$Treatment + I(DataOG$Pt.seminatural*DataOG$Date) + I(DataOG$Pt.seminatural*DataOG$Treatment))
lmSF_M8 <- lm(DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date +DataOG$Treatment + I(DataOG$Pt.seminatural*DataOG$Treatment))
lmSF_M15  <- lm(DataOG$syrphid_fraction ~  DataOG$Pt.seminatural + DataOG$Date +DataOG$Treatment +  I(DataOG$Pt.seminatural*DataOG$Date) + I(DataOG$Pt.seminatural*DataOG$Treatment))
lmSF_M17 <- lm(DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural+ DataOG$Date + DataOG$Treatment + I(DataOG$Field_Mgmt*DataOG$Date))
lmSF_M9 <- lm(DataOG$syrphid_fraction ~ DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I(DataOG$Pt.seminatural*DataOG$Treatment))
lmSF_M12 <- lm(DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$Date + DataOG$Treatment+ I(DataOG$Field_Mgmt*DataOG$Date))
lmSF_M13 <- lm(DataOG$syrphid_fraction ~ DataOG$Field_Mgmt +DataOG$Date + DataOG$Treatment)
lmSF_M16 <- lm(DataOG$syrphid_fraction ~ DataOG$Date +DataOG$Treatment)

# anova
anova(lmSF_M16, lmSF_M13)
#lmSFM13: 2.2e-16 ***

anova(lmSF_M13, lmSF_M12)
#lmSFM12: 7.997e-13 ***

anova(lmSF_M12, lmSF_M9)
#no p value 

anova(lmSF_M9,lmSF_M17)
# M17 < 2.2e-16 ***

anova(lmSF_M17, lmSF_M15)
#no p value

anova(lmSF_M15, lmSF_M8)
#no p value

anova(lmSF_M8, lmSF_M14)
# M14 0.002846 **

anova(lmSF_M14, lmSF_M10)
#no p value

anova(lmSF_M10, lmSF_M11)
# M11  0.164
# no significant difference

anova(lmSF_M14, lmSF_M11)
#M11 2.44e-11 ***

#M11 is the best model accroding to anova. We still decide to calculate the AIC index.

#AIC
AIC(lmSF_M11, lmSF_M10, lmSF_M14, lmSF_M8, lmSF_M15, lmSF_M17, lmSF_M9, lmSF_M12,lmSF_M13,lmSF_M16)
# df      AIC
# lmSF_M11  9 1409.151 # rank 2
# lmSF_M10  8 1409.089 # rank 1
# lmSF_M14  8 1451.763 # rank 5
# lmSF_M8   7 1458.672 # rank 6
# lmSF_M15  7 1618.325 # rank 8
# lmSF_M17  7 1428.652 # rank 3
# lmSF_M9   6 1624.700 # rank 9
# lmSF_M12  6 1432.100 # rank 4
# lmSF_M13  5 1481.405 # rank 7
# lmSF_M16  4 1670.877 # rank 10


#M10 is the best model




