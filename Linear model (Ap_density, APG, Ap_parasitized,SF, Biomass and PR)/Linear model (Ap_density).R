#model comparison for aphids density

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

#linear model
#dataOG
lmAD1 <- lm(DataOG$aphid_live ~ DataOG$syrphid_fraction)
lmAD2 <- lm(DataOG$aphid_live ~DataOG$Field_Mgmt)
lmAD3 <- lm(DataOG$aphid_live ~ DataOG$cropmaturity_init)
lmAD4 <- lm(DataOG$aphid_live ~ DataOG$croptype)
lmAD5 <- lm(DataOG$aphid_live ~ DataOG$Pt.seminatural)
lmAD6 <- lm(DataOG$aphid_live ~ DataOG$Plot_ID)
lmAD7 <- lm(DataOG$aphid_live ~ DataOG$Date)
lmAD8 <- lm(DataOG$aphid_live ~ DataOG$Field_Mgmt)
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

AIC(lmAD1, lmAD2, lmAD3, lmAD4, lmAD5, lmAD6, lmAD7, lmAD9, lmAD10, lmAD11, lmAD12, lmAD13, lmAD14, lmAD15, lmAD16, lmAD17, lmAD18, lmAD19, lmAD20)

#lowest
#lmAD18 72913.84



#check difference between DataOG and Data1
#use data1 
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep = ";")
parasitism_rate <- (Data1$aphid_parasitized/Data1$aphid_live + Data1$aphid_parasitized)
syrphid_fraction <- (Data1$syrphidl_p / ((Data1$aphid_live) + (Data1$syrphidl_p)))
Data1_syrphid_fraction <- data.frame(Data1, syrphid_fraction )

lmAD1 <- lm(DataOG$aphid_live ~ DataOG$syrphid_fraction)
lmAD1_data1 <- lm(Data1_syrphid_fraction$aphid_live ~ Data1_syrphid_fraction$syrphid_fraction)
AIC(lmAD1, lmAD1_data1)

#same AIC value
