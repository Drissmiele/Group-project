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


lmSF1 <- lm(DataOG$syrphid_fraction ~ DataOG$Field_Mgmt)
lmSF2 <- lm(DataOG$syrphid_fraction ~DataOG$cropmaturity_init)
lmSF3 <- lm(DataOG$syrphid_fraction ~ DataOG$croptype)
lmSF4 <- lm(DataOG$syrphid_fraction ~ DataOG$Pt.seminatural)
lmSF5 <- lm(DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$croptype + DataOG$cropmaturity_init +DataOG$Pt.seminatural)
lmSF6 <- lm(DataOG$syrphid_fraction ~ DataOG$cropmaturity_init + DataOG$Field_Mgmt + DataOG$croptype + DataOG$Pt.seminatural)
lmSF7 <- lm(DataOG$syrphid_fraction ~ DataOG$croptype + DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$cropmaturity_init)
lmSF8 <- lm(DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$cropmaturity_init + DataOG$croptype)
lmSF9 <- lm(DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date +DataOG$Treatment + I(DataOG$Pt.seminatural*DataOG$Treatment))
lmSF10 <- lm(DataOG$syrphid_fraction ~ DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I(DataOG$Pt.seminatural*DataOG$Treatment))
lmSF11 <- lm(DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural+ DataOG$Date + DataOG$Treatment + I(DataOG$Field_Mgmt*DataOG$Date) + I(DataOG$Pt.seminatural*DataOG$Treatment))
lmSF12 <- lm(DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural+ DataOG$Date+ DataOG$Treatment+ I(DataOG$Field_Mgmt*DataOG$Date)+ I(DataOG$Pt.seminatural*DataOG$Date) + I(DataOG$Pt.seminatural*DataOG$Treatment))
lmSF13 <- lm(DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$Date + DataOG$Treatment+ I(DataOG$Field_Mgmt*DataOG$Date))
lmSF14 <- lm(DataOG$syrphid_fraction ~ DataOG$Field_Mgmt +DataOG$Date + DataOG$Treatment)
lmSF15 <- lm(DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date +DataOG$Treatment + I(DataOG$Pt.seminatural*DataOG$Date) + I(DataOG$Pt.seminatural*DataOG$Treatment))
lmSF16 <- lm(DataOG$syrphid_fraction ~  DataOG$Pt.seminatural + DataOG$Date +DataOG$Treatment +  I(DataOG$Pt.seminatural*DataOG$Date) + I(DataOG$Pt.seminatural*DataOG$Treatment))
lmSF17 <- lm(DataOG$syrphid_fraction ~ DataOG$Date +DataOG$Treatment)
lmSF18 <- lm(DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural+ DataOG$Date + DataOG$Treatment + I(DataOG$Field_Mgmt*DataOG$Date))
lmSF19 <- lm(DataOG$syrphid_fraction ~ DataOG$Pt.seminatural + DataOG$croptype)
lmSF20 <- lm(DataOG$syrphid_fraction ~ DataOG$Pt.seminatural + DataOG$Treatment)
lmSF21 <- lm(DataOG$syrphid_fraction ~ DataOG$Treatment)
lmSF22 <- lm(DataOG$syrphid_fraction ~ DataOG$APG)
lmSF23 <- lm(DataOG$syrphid_fraction ~ DataOG$parasitism_rate)
lmSF24 <- lm(DataOG$syrphid_fraction ~ DataOG$aphid_live)
lmSF25 <- lm(DataOG$syrphid_fraction ~ DataOG$aphid_parasitized)
lmSF26 <- lm(DataOG$syrphid_fraction ~ DataOG$syrphidl_p)
lmSF27 <- lm(DataOG$syrphid_fraction ~DataOG$Biomass_fin)


AIC(lmSF1, lmSF2, lmSF3, lmSF4, lmSF5, lmSF6, lmSF7, lmSF8, lmSF9, lmSF10, lmSF11, lmSF12, lmSF13, lmSF14, lmSF15, lmSF16, lmSF17, lmSF18, lmSF19, lmSF20)

#lowest AIC:
#lmSF11  8 1409.089

#2 variables
AIC(lmSF1, lmSF2, lmSF3, lmSF4, lmSF21,lmSF22, lmSF23, lmSF24, lmSF25, lmSF26, lmSF27)

# df       AIC
# lmSF1   3 1654.1500
# lmSF2   3 1814.9511
# lmSF3   3 1755.3858
# lmSF4   3 1815.6282 #pt seminatural
# lmSF21  3 1688.7981 #treatment
# lmSF22  3 1501.2663
# lmSF23  3 1392.4256 #parasitism rate
# lmSF24  3 1765.2290
# lmSF25  3 1794.0518
# lmSF26  3 -370.0624 #syrphid larvae
# lmSF27  3 1661.4472 #biomass



