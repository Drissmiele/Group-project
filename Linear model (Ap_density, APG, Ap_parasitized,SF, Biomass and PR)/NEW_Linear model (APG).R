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
lmAPG_M5 <-  lm(DataOG$APG ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I(DataOG$Field_Mgmt* DataOG$Date))
lmAPG_M6 <-  lm(DataOG$APG ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment)
lmAPG_M7 <-  lm(DataOG$APG ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date)
lmAPG_M8 <-  lm(DataOG$APG ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural)
lmAPG_M9 <-  lm(DataOG$APG ~ DataOG$Field_Mgmt)

#AIC
AIC(lmAPG_M1, lmAPG_M2, lmAPG_M3, lmAPG_M4, lmAPG_M5, lmAPG_M6, lmAPG_M7, lmAPG_M8,lmAPG_M9)

# df       AIC
# lmAPG_M1 11 -19871.81  #lowest
# lmAPG_M2 10 -19870.33
# lmAPG_M3  9 -19824.13
# lmAPG_M4  8 -19670.80
# lmAPG_M5  7 -19573.01
# lmAPG_M6  6 -19007.95
# lmAPG_M7  5 -18456.11
# lmAPG_M8  4 -18196.38
# lmAPG_M9  3 -17880.85


#lmAPG_M1: best model

anova(lmAPG_M9, lmAPG_M8)
#  < 2.2e-16 ***

anova(lmAPG_M8, lmAPG_M7)
#  < 2.2e-16 ***

anova(lmAPG_M7, lmAPG_M6)
# < 2.2e-16 ***

anova(lmAPG_M6, lmAPG_M5)
#  < 2.2e-16 ***


anova(lmAPG_M5, lmAPG_M4)
#  < 2.2e-16 ***

anova(lmAPG_M4, lmAPG_M3)
# < 2.2e-16 ***

anova(lmAPG_M3, lmAPG_M2)
#  3.934e-12 ***


anova(lmAPG_M2, lmAPG_M1)
#  0.06214 .

#lmAPG_M2, lmAPG_M1: better models
