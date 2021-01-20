#linear model 1/17
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

#linear model
lmAPG_M1 <- lm(DataOG$APG ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I(DataOG$Field_Mgmt* DataOG$Date) + I(DataOG$Pt.seminatural*DataOG$Date) + I(DataOG$Treatment* DataOG$Date) + I(DataOG$Pt.seminatural*DataOG$Treatment) + I(DataOG$Pt.seminatural*DataOG$Treatment*DataOG$Date))
lmAPG_M2 <- lm(DataOG$APG ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I(DataOG$Field_Mgmt* DataOG$Date) + I(DataOG$Pt.seminatural*DataOG$Date) + I(DataOG$Treatment* DataOG$Date) + I(DataOG$Pt.seminatural*DataOG$Treatment))
lmAPG_M3 <-  lm(DataOG$APG ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I(DataOG$Field_Mgmt* DataOG$Date) + I(DataOG$Pt.seminatural*DataOG$Date) + I(DataOG$Treatment* DataOG$Date))
lmAPG_M4 <-  lm(DataOG$APG ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I(DataOG$Field_Mgmt* DataOG$Date) + I(DataOG$Pt.seminatural*DataOG$Date))
lmAPG_M5 <- lm(DataOG$APG ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I(DataOG$Field_Mgmt* DataOG$Date) + I(DataOG$Pt.seminatural*DataOG$Treatment))
lmAPG_M6 <-  lm(DataOG$APG ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I(DataOG$Field_Mgmt* DataOG$Date))
lmAPG_M7 <-  lm(DataOG$APG ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment)
lmAPG_M8 <-  lm(DataOG$APG ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date)
lmAPG_M9 <-  lm(DataOG$APG ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural)
lmAPG_M10 <-  lm(DataOG$APG ~ DataOG$Treatment + DataOG$Field_Mgmt)
lmAPG_M11 <-  lm(DataOG$APG ~ DataOG$Treatment + DataOG$Pt.seminatural)
lmAPG_M12 <-  lm(DataOG$APG ~ DataOG$Field_Mgmt)
lmAPG_M13 <-  lm(DataOG$APG ~ DataOG$Treatment)
lmAPG_M14 <-  lm(DataOG$APG ~ DataOG$parasitism_rate)
lmAPG_M15 <-  lm(DataOG$APG ~ DataOG$Pt.seminatural)



#AIC
AIC(lmAPG_M1, lmAPG_M2, lmAPG_M3, lmAPG_M4, lmAPG_M5, lmAPG_M6, lmAPG_M7, lmAPG_M8,lmAPG_M9, lmAPG_M10,lmAPG_M11,lmAPG_M12,lmAPG_M13,lmAPG_M14,lmAPG_M15)

# df       AIC
# lmAPG_M1  11 -19871.81  #lowest AIC value = best model
# lmAPG_M2  10 -19870.33
# lmAPG_M3   9 -19824.13
# lmAPG_M4   8 -19670.80
# lmAPG_M5   8 -19616.24
# lmAPG_M6   7 -19573.01
# lmAPG_M7   6 -19007.95
# lmAPG_M8   5 -18456.11
# lmAPG_M9   4 -18196.38
# lmAPG_M10  4 -18407.72
# lmAPG_M11  4 -18370.04
# lmAPG_M12  3 -17880.85
# lmAPG_M13  3 -17851.17
# lmAPG_M14  3 -17459.71
# lmAPG_M15  3 -17844.41



#lmAPG_M1: best model

anova(lmAPG_M15, lmAPG_M14)
#no p value

anova(lmAPG_M15, lmAPG_M13)
#no p value

anova(lmAPG_M15, lmAPG_M12)
#no p value

anova(lmAPG_M15, lmAPG_M11)
#  < 2.2e-16 ***

anova(lmAPG_M11, lmAPG_M10)
#  < 2.2e-16 ***

anova(lmAPG_M10, lmAPG_M9)
#no p value

anova(lmAPG_M10, lmAPG_M8)
#1.275e-12 ***

anova(lmAPG_M8, lmAPG_M7)
#< 2.2e-16 ***

anova(lmAPG_M7, lmAPG_M6)
# < 2.2e-16 ***

anova(lmAPG_M6, lmAPG_M5)
# 1.784e-11 ***

anova(lmAPG_M5, lmAPG_M4)
#no p value

anova(lmAPG_M5, lmAPG_M3)
# < 2.2e-16 ***

anova(lmAPG_M3, lmAPG_M2)
# 3.934e-12 ***

anova(lmAPG_M2, lmAPG_M1)
# 0.06214 


# because no significant difference was found between M2 and M1, M2 is the best model according to the anova() because it is "simpler" than M1.