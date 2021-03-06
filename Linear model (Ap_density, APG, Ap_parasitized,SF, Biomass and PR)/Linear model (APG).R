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

lmAPG1 <- lm(DataOG$APG ~ DataOG$Treatment)
lmAPG2 <- lm(DataOG$APG ~ DataOG$aphid_parasitized)
lmAPG3 <- lm(DataOG$APG ~ DataOG$parasitism_rate)
lmAPG4 <- lm(DataOG$APG ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I((DataOG$Pt.seminatural)*(DataOG$Date)) + I((DataOG$Pt.seminatural)*(DataOG$Treatment)))
lmAPG5 <- lm(DataOG$APG ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I(DataOG$Field_Mgmt* DataOG$Date) + I(DataOG$Pt.seminatural*DataOG$Date) + I(DataOG$Treatment* DataOG$Date) + I(DataOG$Pt.seminatural*DataOG$Treatment) + I(DataOG$Pt.seminatural*DataOG$Treatment*DataOG$Date))
lmAPG6 <- lm(DataOG$APG ~ DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I((DataOG$Pt.seminatural)*(DataOG$Date)) + I((DataOG$Pt.seminatural)*(DataOG$Treatment)))
lmAPG7 <- lm(DataOG$APG ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I((DataOG$Field_Mgmt)*(DataOG$Date)) + I((DataOG$Pt.seminatural)*(DataOG$Date)) + I((DataOG$Treatment)*(DataOG$Date) + I((DataOG$Pt.seminatural)*(DataOG$Treatment))))
lmAPG8 <- lm(DataOG$APG ~ DataOG$Treatment + DataOG$Field_Mgmt)
lmAPG9 <- lm(DataOG$APG ~ DataOG$aphid_parasitized + DataOG$Treatment)
lmAPG10 <- lm(DataOG$APG ~ DataOG$aphid_parasitized + DataOG$syrphid_fraction)
lmAPG11 <- lm(DataOG$APG ~ DataOG$aphid_parasitized + DataOG$Field_Mgmt)
lmAPG12 <- lm(DataOG$APG ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I((DataOG$Field_Mgmt)*(DataOG$Date)) + I((DataOG$Pt.seminatural)*(DataOG$Treatment)))
lmAPG13 <- lm(DataOG$APG ~ DataOG$parasitism_rate + DataOG$syrphid_fraction)
lmAPG14 <- lm(DataOG$APG ~ DataOG$parasitism_rate + DataOG$Field_Mgmt)
lmAPG15 <- lm(DataOG$APG ~ DataOG$Pt.seminatural + DataOG$Treatment)
lmAPG16 <- lm(DataOG$APG ~ DataOG$Pt.seminatural + DataOG$parasitism_rate)
lmAPG17 <- lm(DataOG$APG ~ DataOG$Pt.seminatural)
lmAPG18 <- lm(DataOG$APG ~ DataOG$Field_Mgmt + DataOG$Treatment)
lmAPG19 <- lm(DataOG$APG ~ DataOG$Field_Mgmt + DataOG$parasitism_rate)
lmAPG20 <- lm(DataOG$APG ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I((DataOG$Field_Mgmt)*(DataOG$Date)) + I((DataOG$Pt.seminatural)*(DataOG$Date)))

AIC(lmAPG1, lmAPG2, lmAPG3, lmAPG4, lmAPG5, lmAPG6, lmAPG7, lmAPG8, lmAPG9, lmAPG10, lmAPG11, lmAPG12, lmAPG13, lmAPG14, lmAPG15, lmAPG16, lmAPG17, lmAPG18, lmAPG19, lmAPG20)
#lowest AIC:
#lmAPG5  11 -19871.81

#only 2 variables comparison
AIC(lmAPG1, lmAPG2, lmAPG3, lmAPG4, lmAPG17)
# df       AIC
# lmAPG1   3 -17851.17 #treatment  #lowest
# lmAPG2   3 -17698.67
# lmAPG3   3 -17459.71
# lmAPG4   3 -17687.73 #syrphid fraction
# lmAPG17  3 -17844.41 #pt seminatural #secondly lowest

#APG is affected by treatment and pt seminatural


