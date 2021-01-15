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


lmAP1 <- lm(DataOG$aphid_parasitized ~ DataOG$aphid_live)
lmAP2 <- lm(DataOG$aphid_parasitized ~DataOG$croptype)
lmAP3 <- lm(DataOG$aphid_parasitized ~ DataOG$Pt.seminatural)
lmAP4 <- lm(DataOG$aphid_parasitized ~ DataOG$Field_Mgmt)
lmAP5 <- lm(DataOG$aphid_parasitized ~ DataOG$APG)
lmAP6 <- lm(DataOG$aphid_parasitized ~ DataOG$aphid_live + DataOG$croptype + DataOG$Pt.seminatural + DataOG$Field_Mgmt + DataOG$APG)
lmAP7 <- lm(DataOG$aphid_parasitized ~ DataOG$APG + DataOG$croptype + DataOG$Pt.seminatural + DataOG$aphid_live + DataOG$Field_Mgmt)
lmAP8 <- lm(DataOG$aphid_parasitized ~ DataOG$Field_Mgmt + DataOG$croptype + DataOG$Pt.seminatural + DataOG$aphid_live + DataOG$APG)
lmAP9 <- lm(DataOG$aphid_parasitized ~ DataOG$Pt.seminatural + DataOG$croptype + DataOG$Field_Mgmt + DataOG$aphid_live + DataOG$APG)
lmAP10 <- lm(DataOG$aphid_parasitized ~ DataOG$aphid_live + DataOG$croptype)
lmAP11 <- lm(DataOG$aphid_parasitized ~ DataOG$aphid_live + DataOG$Pt.seminatural)
lmAP12 <- lm(DataOG$aphid_parasitized ~ DataOG$croptype + DataOG$Pt.seminatural)
lmAP13 <- lm(DataOG$aphid_parasitized ~ DataOG$croptype + DataOG$Field_Mgmt)
lmAP14 <- lm(DataOG$aphid_parasitized ~ DataOG$Pt.seminatural + DataOG$Field_Mgmt)
lmAP15 <- lm(DataOG$aphid_parasitized ~ DataOG$Pt.seminatural + DataOG$APG)
lmAP16 <- lm(DataOG$aphid_parasitized ~ DataOG$Field_Mgmt + DataOG$APG)
lmAP17 <- lm(DataOG$aphid_parasitized ~ DataOG$Field_Mgmt + DataOG$croptype)
lmAP18 <- lm(DataOG$aphid_parasitized ~ DataOG$APG + DataOG$croptype)
lmAP19 <- lm(DataOG$aphid_parasitized ~ DataOG$APG + DataOG$Pt.seminatural)
lmAP20 <- lm(DataOG$aphid_parasitized ~ DataOG$APG + DataOG$Field_Mgmt)


AIC(lmAP1, lmAP2, lmAP3, lmAP4, lmAP5, lmAP6, lmAP7, lmAP8, lmAP9, lmAPG10, lmAPG11, lmAPG12, lmAPG13, lmAPG14, lmAPG15, lmAPG16, lmAPG17, lmAPG18, lmAPG19, lmAPG20)

#lowest AIC:
lmAPG12   -18945.04