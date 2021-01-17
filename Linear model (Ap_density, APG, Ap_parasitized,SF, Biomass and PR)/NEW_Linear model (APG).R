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
# Model 1: DataOG$APG ~ DataOG$Field_Mgmt
# Model 2: DataOG$APG ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural
# Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
# 1  12708 182.17                                  
# 2  12707 177.68  1    4.4948 321.45 < 2.2e-16 ***

anova(lmAPG_M8, lmAPG_M7)
# Model 1: DataOG$APG ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural
# Model 2: DataOG$APG ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date
# Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
# 1  12707 177.68                                  
# 2  12706 174.06  1    3.6214 264.36 < 2.2e-16 ***

anova(lmAPG_M7, lmAPG_M6)
# Model 1: DataOG$APG ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date
# Model 2: DataOG$APG ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + 
#   DataOG$Treatment
# Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
# 1  12706 174.06                                  
# 2  12705 166.64  1    7.4217 565.86 < 2.2e-16 ***

anova(lmAPG_M6, lmAPG_M5)
# Model 1: DataOG$APG ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + 
#   DataOG$Treatment
# Model 2: DataOG$APG ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + 
#   DataOG$Treatment + I(DataOG$Field_Mgmt * DataOG$Date)
# Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
# 1  12705 166.64                                  
# 2  12704 159.37  1    7.2711 579.63 < 2.2e-16 ***


anova(lmAPG_M5, lmAPG_M4)
# Model 1: DataOG$APG ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + 
#   DataOG$Treatment + I(DataOG$Field_Mgmt * DataOG$Date)
# Model 2: DataOG$APG ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + 
#   DataOG$Treatment + I(DataOG$Field_Mgmt * DataOG$Date) + I(DataOG$Pt.seminatural * 
#                                                               DataOG$Date)
# Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
# 1  12704 159.37                                  
# 2  12703 158.12  1    1.2463 100.13 < 2.2e-16 ***

anova(lmAPG_M4, lmAPG_M3)
#Model 1: DataOG$APG ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + 
# DataOG$Treatment + I(DataOG$Field_Mgmt * DataOG$Date) + I(DataOG$Pt.seminatural * 
#                                                             DataOG$Date)
# Model 2: DataOG$APG ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + 
#   DataOG$Treatment + I(DataOG$Field_Mgmt * DataOG$Date) + I(DataOG$Pt.seminatural * 
#                                                               DataOG$Date) + I(DataOG$Treatment * DataOG$Date)
# Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
# 1  12703 158.12                                  
# 2  12702 156.20  1    1.9206 156.19 < 2.2e-16 ***

anova(lmAPG_M3, lmAPG_M2)
# Model 1: DataOG$APG ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + 
#   DataOG$Treatment + I(DataOG$Field_Mgmt * DataOG$Date) + I(DataOG$Pt.seminatural * 
#                                                               DataOG$Date) + I(DataOG$Treatment * DataOG$Date)
# Model 2: DataOG$APG ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + 
#   DataOG$Treatment + I(DataOG$Field_Mgmt * DataOG$Date) + I(DataOG$Pt.seminatural * 
#                                                               DataOG$Date) + I(DataOG$Treatment * DataOG$Date) + I(DataOG$Pt.seminatural * 
#                                                                                                                      DataOG$Treatment)
# Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
# 1  12702 156.20                                  
# 2  12701 155.61  1   0.59114 48.251 3.934e-12 ***


anova(lmAPG_M2, lmAPG_M1)
# Model 1: DataOG$APG ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + 
#   DataOG$Treatment + I(DataOG$Field_Mgmt * DataOG$Date) + I(DataOG$Pt.seminatural * 
#                                                               DataOG$Date) + I(DataOG$Treatment * DataOG$Date) + I(DataOG$Pt.seminatural * 
#                                                                                                                      DataOG$Treatment)
# Model 2: DataOG$APG ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + 
#   DataOG$Treatment + I(DataOG$Field_Mgmt * DataOG$Date) + I(DataOG$Pt.seminatural * 
#                                                           DataOG$Date) + I(DataOG$Treatment * DataOG$Date) + I(DataOG$Pt.seminatural * 
#                                                                                                                      DataOG$Treatment) + I(DataOG$Pt.seminatural * DataOG$Treatment * 
#                                                                                                                                              DataOG$Date)
# Res.Df    RSS Df Sum of Sq    F  Pr(>F)  
# 1  12701 155.61                            
# 2  12700 155.56  1  0.042627 3.48 0.06214 .



#lmAPG_M2, lmAPG_M1: better models

