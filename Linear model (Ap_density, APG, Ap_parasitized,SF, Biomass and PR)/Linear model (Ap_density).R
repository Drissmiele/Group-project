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

#linear model
#dataOG
lmAD1 <- lm(DataOG$aphid_live ~ DataOG$syrphid_fraction)
lmAD2 <- lm(DataOG$aphid_live ~DataOG$Field_Mgmt)
lmAD3 <- lm(DataOG$aphid_live ~ DataOG$cropmaturity_init)
lmAD4 <- lm(DataOG$aphid_live ~ DataOG$croptype)
lmAD5 <- lm(DataOG$aphid_live ~ DataOG$Pt.seminatural)
lmAD6 <- lm(DataOG$aphid_live ~ DataOG$Plot_ID)
lmAD7 <- lm(DataOG$aphid_live ~ DataOG$Date)
lmAD8 <- lm(DataOG$aphid_live ~ DataOG$Field_Mgmt + DataOG$parasitism_rate)
lmAD9 <- lm(DataOG$aphid_live ~ DataOG$Treatment)
lmAD10 <- lm(DataOG$aphid_live ~ DataOG$aphid_parasitized)
lmAD11 <- lm(DataOG$aphid_live ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I(DataOG$Pt.seminatural* DataOG$Date) + I(DataOG$Field_Mgmt*DataOG$Treatment))
lmAD12 <- lm(DataOG$aphid_live ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I(DataOG$Field_Mgmt*DataOG$Treatment))
lmAD13 <- lm(DataOG$aphid_live ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I(DataOG$Pt.seminatural* DataOG$Date))
lmAD14 <- lm(DataOG$aphid_live ~ DataOG$Treatment + DataOG$Pt.seminatural + DataOG$Date + DataOG$Field_Mgmt)
lmAD15 <- lm(DataOG$aphid_live ~ DataOG$aphid_parasitized + DataOG$syrphid_fraction + DataOG$Treatment + DataOG$croptype)
lmAD16 <- lm(DataOG$aphid_live ~ DataOG$aphid_parasitized + DataOG$syrphid_fraction + DataOG$croptype)
lmAD17 <- lm(DataOG$aphid_live ~ DataOG$aphid_parasitized + DataOG$cropmaturity_init)
lmAD18 <- lm(DataOG$aphid_live ~ DataOG$Pt.seminatural + DataOG$croptype + DataOG$Date + DataOG$Plot_ID + DataOG$Treatment + DataOG$syrphid_fraction + DataOG$cropmaturity_init + DataOG$Field_Mgmt)
lmAD19 <- lm(DataOG$aphid_live ~ DataOG$Pt.seminatural + DataOG$Treatment)
lmAD20 <- lm(DataOG$aphid_live ~ DataOG$Pt.seminatural + DataOG$Date)

AIC(lmAD1, lmAD2, lmAD3, lmAD4, lmAD5, lmAD6, lmAD7, lmAD8, lmAD9, lmAD10, lmAD11, lmAD12, lmAD13, lmAD14, lmAD15, lmAD16, lmAD17, lmAD18, lmAD19, lmAD20)

#lowest AIC
#lmAD11 149319.6

#only 2 variables comparison
AIC(lmAD1, lmAD2, lmAD3, lmAD4, lmAD5, lmAD6, lmAD7)
# df      AIC
# lmAD1  3 152261.6
# lmAD2  3 151662.9
# lmAD3  3 152252.3
# lmAD4  3 152301.8
# lmAD5  3 152156.6
# lmAD6  3 152336.1
# lmAD7  3 152050.8

#lowest
#lmAD2: field management
#secondly lowest
#lmAD7: date


#############################not sure
#ANOVA
anovaAD3 <- aov(DataOG$aphid_live ~ DataOG$cropmaturity_init)
anovaAD4 <- aov(DataOG$aphid_live ~ DataOG$croptype)
anovaAD5 <- aov(DataOG$aphid_live ~ DataOG$Pt.seminatural)
anovaAD6 <- aov(DataOG$aphid_live ~ DataOG$Plot_ID)
anovaAD7 <- aov(DataOG$aphid_live ~ DataOG$Date)
summary(anovaAD1)
summary(anovaAD2)
summary(anovaAD3)
summary(anovaAD4)
summary(anovaAD5)
summary(anovaAD6)
summary(anovaAD7)

TukeyHSD(anovaAD1)
TukeyHSD(anovaAD2)
TukeyHSD(anovaAD3)
TukeyHSD(anovaAD4)
TukeyHSD(anovaAD5)
TukeyHSD(anovaAD6)
TukeyHSD(anovaAD7)

