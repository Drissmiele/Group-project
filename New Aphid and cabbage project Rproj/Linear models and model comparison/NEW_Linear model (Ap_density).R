#model comparison for aphids density

#creation of DataOG with all variables
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

#linear models from complex to simple

lmAD1 <- lm(DataOG$aphid_live ~ DataOG$Pt.seminatural + DataOG$croptype + DataOG$Date + DataOG$Plot_ID + DataOG$Treatment + DataOG$syrphid_fraction + DataOG$cropmaturity_init + DataOG$Field_Mgmt)
lmAD2 <- lm(DataOG$aphid_live ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I(DataOG$Pt.seminatural* DataOG$Date) + I(DataOG$Field_Mgmt*DataOG$Treatment))
lmAD3 <- lm(DataOG$aphid_live ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I(DataOG$Field_Mgmt*DataOG$Treatment))
lmAD4 <- lm(DataOG$aphid_live ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I(DataOG$Pt.seminatural* DataOG$Date))
lmAD5 <- lm(DataOG$aphid_live ~ DataOG$Treatment + DataOG$Pt.seminatural + DataOG$Date + DataOG$Field_Mgmt)
lmAD6 <- lm(DataOG$aphid_live ~ DataOG$aphid_parasitized + DataOG$syrphid_fraction + DataOG$Treatment + DataOG$croptype)
lmAD7 <- lm(DataOG$aphid_live ~ DataOG$aphid_parasitized + DataOG$syrphid_fraction + DataOG$croptype)
lmAD8 <- lm(DataOG$aphid_live ~ DataOG$aphid_parasitized + DataOG$cropmaturity_init)
lmAD9 <- lm(DataOG$aphid_live ~ DataOG$Pt.seminatural + DataOG$Treatment)
lmAD10 <- lm(DataOG$aphid_live ~ DataOG$Pt.seminatural + DataOG$Date)
lmAD11 <- lm(DataOG$aphid_live ~ DataOG$Field_Mgmt + DataOG$parasitism_rate)
lmAD12 <- lm(DataOG$aphid_live ~ DataOG$Treatment + DataOG$syrphid_fraction)
lmAD13 <- lm(DataOG$aphid_live ~ DataOG$Treatment)
lmAD14 <- lm(DataOG$aphid_live ~ DataOG$aphid_parasitized)
lmAD15 <- lm(DataOG$aphid_live ~ DataOG$Date)
lmAD16 <- lm(DataOG$aphid_live ~ DataOG$syrphid_fraction)
lmAD17 <- lm(DataOG$aphid_live ~DataOG$Field_Mgmt)
lmAD18 <- lm(DataOG$aphid_live ~ DataOG$cropmaturity_init)
lmAD19 <- lm(DataOG$aphid_live ~ DataOG$croptype)
lmAD20 <- lm(DataOG$aphid_live ~ DataOG$Pt.seminatural)

#AIC model comparison
AIC(lmAD1, lmAD2, lmAD3, lmAD4, lmAD5, lmAD6, lmAD7, lmAD8, lmAD9, lmAD10, lmAD11, lmAD12, lmAD13, lmAD14, lmAD15, lmAD16, lmAD17, lmAD18, lmAD19, lmAD20)


# df      AIC
# lmAD1  10 150273.9
# lmAD2   8 149319.6 #lowest AIC value = best model
# lmAD3   7 149364.4
# lmAD4   7 150390.6
# lmAD5   6 150431.6
# lmAD6   6 151371.1
# lmAD7   5 152125.9
# lmAD8   4 152180.8
# lmAD9   4 151370.4
# lmAD10  4 151858.0
# lmAD11  4 151494.2
# lmAD12  4 151529.4
# lmAD13  3 151564.6
# lmAD14  3 152255.4
# lmAD15  3 152050.8
# lmAD16  3 152261.6
# lmAD17  3 151662.9
# lmAD18  3 152252.3
# lmAD19  3 152301.8
# lmAD20  3 152156.6


# anova model comparison

anova(lmAD20, lmAD19)
#no p value

anova(lmAD20, lmAD18)
#no p value

anova(lmAD20, lmAD17)
#no p value

anova(lmAD20, lmAD16)
#no p value

anova(lmAD20, lmAD15)
#no p value

anova(lmAD20, lmAD14)
#no p value

anova(lmAD20, lmAD13)
#no p value

anova(lmAD20, lmAD12)
#< 2.2e-16 ***

anova(lmAD12, lmAD11)
#no p value

anova(lmAD12, lmAD10)
#no p value

anova(lmAD12, lmAD9)
#no p value

anova(lmAD12, lmAD8)
#no p value

anova(lmAD12, lmAD7)
#no p value

anova(lmAD12, lmAD6)
# < 2.2e-16 ***

anova(lmAD6, lmAD5)
#no p value

anova(lmAD6, lmAD4)
# < 2.2e-16 ***

anova(lmAD4, lmAD3)
#no p value

anova(lmAD4, lmAD2)
# < 2.2e-16 ***
  
anova(lmAD2, lmAD1)
#no p value

# M2 appears to be the best model.

