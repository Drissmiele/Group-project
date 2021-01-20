#model comparison for Biomass lm

####DataOG

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

#linear model Biomass
#dataOG

lmBM1 <- lm(DataOG$Biomass_fin ~ DataOG$aphid_live)
lmBM2 <- lm(DataOG$Biomass_fin ~ DataOG$Pt.seminatural)
lmBM3 <- lm(DataOG$Biomass_fin ~ DataOG$Field_Mgmt)
lmBM4 <- lm(DataOG$Biomass_fin ~ DataOG$aphid_live + DataOG$Pt.seminatural)
lmBM5 <- lm(DataOG$Biomass_fin ~ DataOG$aphid_live + DataOG$Field_Mgmt)
lmBM6 <- lm(DataOG$Biomass_fin ~ DataOG$Pt.seminatural + DataOG$Field_Mgmt)
lmBM7 <- lm(DataOG$Biomass_fin ~ DataOG$Pt.seminatural + DataOG$aphid_live)
lmBM8 <- lm(DataOG$Biomass_fin ~ DataOG$Field_Mgmt + DataOG$aphid_live)
lmBM9 <- lm(DataOG$Biomass_fin ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural)
lmBM10 <- lm(DataOG$Biomass_fin ~ DataOG$aphid_live + DataOG$Field_Mgmt + DataOG$Pt.seminatural)
lmBM11 <- lm(DataOG$Biomass_fin ~ DataOG$Pt.seminatural + DataOG$Field_Mgmt + DataOG$aphid_live)
lmBM12 <- lm(DataOG$Biomass_fin ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$aphid_live)
lmBM13 <- lm(DataOG$Biomass_fin ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Treatment + I((DataOG$Field_Mgmt)*(DataOG$aphid_live)) + I((DataOG$Pt.seminatural)*(DataOG$aphid_parasitized)) + I((DataOG$Field_Mgmt)*(DataOG$Treatment)))
lmBM14 <- lm(DataOG$Biomass_fin ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Treatment + I((DataOG$Field_Mgmt)*(DataOG$aphid_live)) + I((DataOG$Pt.seminatural)*(DataOG$Treatment)))
lmBM15 <- lm(DataOG$Biomass_fin ~ DataOG$Pt.seminatural + DataOG$Treatment + I((DataOG$Pt.seminatural)*(DataOG$aphid_live)) + I((DataOG$Field_Mgmt)*(DataOG$parasitism_rate)))


# anova for model comparaison from simple to complex models

anova(lmBM1, lmBM2)
#no p value

anova(lmBM2, lmBM3)
#no p value

anova(lmBM3, lmBM4)
#no p value

anova(lmBM4, lmBM5)
#no p value

anova(lmBM5, lmBM6)
#no p  value

anova(lmBM6, lmBM7)
#no p value

anova(lmBM7, lmBM8)
#no p value

anova(lmBM8, lmBM9)
#no p value 

anova(lmBM9, lmBM10)
#M10 < 2.2e-16 ***

anova(lmBM10, lmBM11)
#no p value

anova(lmBM11, lmBM12)
#no p value

anova(lmBM12, lmBM15)
#no p value

anova(lmBM15, lmBM14)
#M14 < 2.2e-16 ***

anova(lmBM14, lmBM13)
#M13 < 2.2e-16 ***

#M13 is the best model

#　Given the absence of p values with the anova() function, we relied on the AIC index.
AIC(lmBM1, lmBM2, lmBM3, lmBM4, lmBM5, lmBM6, lmBM7, lmBM8, lmBM9, lmBM10, lmBM11, lmBM12, lmBM13, lmBM14, lmBM15)

#lowest
#lmBM13 158068.6


    