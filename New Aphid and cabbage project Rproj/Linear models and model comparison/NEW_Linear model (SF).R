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
lmSF_M1 <- lm(DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural+ DataOG$Date+ DataOG$Treatment+ I(DataOG$Field_Mgmt*DataOG$Date)+ I(DataOG$Pt.seminatural*DataOG$Date) + I(DataOG$Pt.seminatural*DataOG$Treatment))
lmSF_M2 <- lm(DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural+ DataOG$Date + DataOG$Treatment + I(DataOG$Field_Mgmt*DataOG$Date) + I(DataOG$Pt.seminatural*DataOG$Treatment))
lmSF_M3 <- lm(DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date +DataOG$Treatment + I(DataOG$Pt.seminatural*DataOG$Date) + I(DataOG$Pt.seminatural*DataOG$Treatment))
lmSF_M4 <- lm(DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date +DataOG$Treatment + I(DataOG$Pt.seminatural*DataOG$Treatment))
lmSF_M5  <- lm(DataOG$syrphid_fraction ~  DataOG$Pt.seminatural + DataOG$Date +DataOG$Treatment +  I(DataOG$Pt.seminatural*DataOG$Date) + I(DataOG$Pt.seminatural*DataOG$Treatment))
lmSF_M6 <- lm(DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural+ DataOG$Date + DataOG$Treatment + I(DataOG$Field_Mgmt*DataOG$Date))
lmSF_M7 <- lm(DataOG$syrphid_fraction ~ DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I(DataOG$Pt.seminatural*DataOG$Treatment))
lmSF_M8 <- lm(DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$Date + DataOG$Treatment+ I(DataOG$Field_Mgmt*DataOG$Date))
lmSF_M9 <- lm(DataOG$syrphid_fraction ~ DataOG$Field_Mgmt +DataOG$Date + DataOG$Treatment)
lmSF_M10 <- lm(DataOG$syrphid_fraction ~ DataOG$Date +DataOG$Treatment)
lmSF_M11 <- lm(DataOG$syrphid_fraction ~ DataOG$Pt.seminatural +DataOG$Treatment)
lmSF_M12 <- lm(DataOG$syrphid_fraction ~ DataOG$Field_Mgmt)
lmSF_M13 <- lm(DataOG$syrphid_fraction ~ DataOG$Pt.seminatural)
lmSF_M14 <- lm(DataOG$syrphid_fraction ~ DataOG$Treatment)
lmSF_M15 <- lm(DataOG$syrphid_fraction ~ DataOG$croptype)

#AIC model comparison
AIC(lmSF_M1, lmSF_M2, lmSF_M3, lmSF_M4, lmSF_M5, lmSF_M6, lmSF_M7, lmSF_M8,lmSF_M9,lmSF_M10,lmSF_M11,lmSF_M12,lmSF_M13,lmSF_M14,lmSF_M15)

# df      AIC
# lmSF_M1   9 1409.151
# lmSF_M2   8 1409.089 #lowest AIC value = best model
# lmSF_M3   8 1451.763
# lmSF_M4   7 1458.672
# lmSF_M5   7 1618.325
# lmSF_M6   7 1428.652
# lmSF_M7   6 1624.700
# lmSF_M8   6 1432.100
# lmSF_M9   5 1481.405
# lmSF_M10  4 1670.877
# lmSF_M11  4 1661.267
# lmSF_M12  3 1654.150
# lmSF_M13  3 1815.628
# lmSF_M14  3 1688.798
# lmSF_M15  3 1755.386


# anova model comparison

anova(lmSF_M15, lmSF_M14)
#no p value

anova(lmSF_M15, lmSF_M13)
#no p value

anova(lmSF_M15, lmSF_M12)
#no p value 

anova(lmSF_M15,lmSF_M11)
#< 2.2e-16 ***

anova(lmSF_M11, lmSF_M10)
#no p value

anova(lmSF_M11, lmSF_M9)
#< 2.2e-16 ***

anova(lmSF_M9, lmSF_M8)
#7.997e-13 ***

anova(lmSF_M8, lmSF_M7)
#no p value

anova(lmSF_M8, lmSF_M6)
# 0.01962 *

anova(lmSF_M6, lmSF_M5)
#no p value

anova(lmSF_M6, lmSF_M4)
#no p value

anova(lmSF_M6, lmSF_M3)
#no p value

anova(lmSF_M6, lmSF_M2)
# 3.447e-06 ***

anova(lmSF_M2, lmSF_M1)
# 0.164

# M1 and M2 do not significantly differ from one another, M2 would hence be preferred over M1 because it is more simple.





