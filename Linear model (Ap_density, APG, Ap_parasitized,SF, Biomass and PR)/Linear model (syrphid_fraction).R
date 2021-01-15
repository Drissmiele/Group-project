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


AIC(lmSF1, lmSF2, lmSF3, lmSF4, lmSF5, lmSF6, lmSF7, lmSF8, lmSF9, lmSF10, lmSF11, lmSF12, lmSF13, lmSF14, lmSF15, lmSF16, lmSF17, lmSF18, lmSF19, lmSF20)

#lowest AIC:
#lmSF11  8 1409.089