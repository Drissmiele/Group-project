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

#linear model
lmSF_M8 <- lm(DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date +DataOG$Treatment + I(DataOG$Pt.seminatural*DataOG$Treatment))
lmSF_M9 <- lm(DataOG$syrphid_fraction ~ DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I(DataOG$Pt.seminatural*DataOG$Treatment))
lmSF_M10 <- lm(DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural+ DataOG$Date + DataOG$Treatment + I(DataOG$Field_Mgmt*DataOG$Date) + I(DataOG$Pt.seminatural*DataOG$Treatment))
lmSF_M11 <- lm(DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural+ DataOG$Date+ DataOG$Treatment+ I(DataOG$Field_Mgmt*DataOG$Date)+ I(DataOG$Pt.seminatural*DataOG$Date) + I(DataOG$Pt.seminatural*DataOG$Treatment))
lmSF_M12 <- lm(DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$Date + DataOG$Treatment+ I(DataOG$Field_Mgmt*DataOG$Date))
lmSF_M13 <- lm(DataOG$syrphid_fraction ~ DataOG$Field_Mgmt +DataOG$Date + DataOG$Treatment)
lmSF_M14 <- lm(DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date +DataOG$Treatment + I(DataOG$Pt.seminatural*DataOG$Date) + I(DataOG$Pt.seminatural*DataOG$Treatment))
lmSF_M15  <- lm(DataOG$syrphid_fraction ~  DataOG$Pt.seminatural + DataOG$Date +DataOG$Treatment +  I(DataOG$Pt.seminatural*DataOG$Date) + I(DataOG$Pt.seminatural*DataOG$Treatment))
lmSF_M16 <- lm(DataOG$syrphid_fraction ~ DataOG$Date +DataOG$Treatment)
lmSF_M17 <- lm(DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural+ DataOG$Date + DataOG$Treatment + I(DataOG$Field_Mgmt*DataOG$Date))

#AIC
SF_AIC <- AIC(lmSF_M8, lmSF_M9, lmSF_M10, lmSF_M11, lmSF_M12,lmSF_M13, lmSF_M14, lmSF_M15,lmSF_M16,lmSF_M17)

# df      AIC
# lmSF_M8   7 1458.672
# lmSF_M9   6 1624.700
# lmSF_M10  8 1409.089  #lowest
# lmSF_M11  9 1409.151
# lmSF_M12  6 1432.100
# lmSF_M13  5 1481.405
# lmSF_M14  8 1451.763
# lmSF_M15  7 1618.325
# lmSF_M16  4 1670.877
# lmSF_M17  7 1428.652

#lmSF_M10: best model



#ANOVA
anova(lmSF_M16, lmSF_M13)
# Model 1: DataOG$syrphid_fraction ~ DataOG$Date + DataOG$Treatment
# Model 2: DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$Date + DataOG$Treatment
# Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
# 1  12707 848.19                                  
# 2  12706 835.50  1    12.682 192.86 < 2.2e-16 ***

anova(lmSF_M13, lmSF_M12)
# Model 1: DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$Date + DataOG$Treatment
# Model 2: DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$Date + DataOG$Treatment + 
#   I(DataOG$Field_Mgmt * DataOG$Date)
# Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
# 1  12706 835.50                                  
# 2  12705 832.14  1    3.3658 51.389 7.997e-13 ***


anova(lmSF_M12, lmSF_M17)
# Model 1: DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$Date + DataOG$Treatment + 
#   I(DataOG$Field_Mgmt * DataOG$Date)
# Model 2: DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + 
#   DataOG$Date + DataOG$Treatment + I(DataOG$Field_Mgmt * DataOG$Date)
# Res.Df    RSS Df Sum of Sq      F  Pr(>F)  
# 1  12705 832.14                              
# 2  12704 831.78  1    0.3566 5.4465 0.01962 *


anova(lmSF_M17, lmSF_M9)
# Model 1: DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + 
#   DataOG$Date + DataOG$Treatment + I(DataOG$Field_Mgmt * DataOG$Date)
# Model 2: DataOG$syrphid_fraction ~ DataOG$Pt.seminatural + DataOG$Date + 
#   DataOG$Treatment + I(DataOG$Pt.seminatural * DataOG$Treatment)
# Res.Df    RSS Df Sum of Sq     F    Pr(>F)    
# 1  12704 831.78                                 
# 2  12705 844.84 -1   -13.062 199.5 < 2.2e-16 ***


anova(lmSF_M9, lmSF_M8)
# Model 1: DataOG$syrphid_fraction ~ DataOG$Pt.seminatural + DataOG$Date + 
#   DataOG$Treatment + I(DataOG$Pt.seminatural * DataOG$Treatment)
# Model 2: DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + 
#   DataOG$Date + DataOG$Treatment + I(DataOG$Pt.seminatural * 
#                                        DataOG$Treatment)
# Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
# 1  12705 844.84                                  
# 2  12704 833.75  1    11.095 169.06 < 2.2e-16 ***


 anova(lmSF_M8, lmSF_M15)
# Model 1: DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + 
#   DataOG$Date + DataOG$Treatment + I(DataOG$Pt.seminatural * 
#                                        DataOG$Treatment)
# Model 2: DataOG$syrphid_fraction ~ DataOG$Pt.seminatural + DataOG$Date + 
#   DataOG$Treatment + I(DataOG$Pt.seminatural * DataOG$Date) + 
#   I(DataOG$Pt.seminatural * DataOG$Treatment)
# Res.Df    RSS Df Sum of Sq F Pr(>F)
# 1  12704 833.75                      
# 2  12704 844.29  0   -10.539         

 anova(lmSF_M14, lmSF_M10)
 # Model 1: DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + 
 #   DataOG$Date + DataOG$Treatment + I(DataOG$Pt.seminatural * 
 #                                        DataOG$Date) + I(DataOG$Pt.seminatural * DataOG$Treatment)
 # Model 2: DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + 
 #   DataOG$Date + DataOG$Treatment + I(DataOG$Field_Mgmt * DataOG$Date) + 
 #   I(DataOG$Pt.seminatural * DataOG$Treatment)
 # Res.Df    RSS Df Sum of Sq F Pr(>F)
 # 1  12703 833.16                      
 # 2  12703 830.37  0    2.7926         

anova(lmSF_M8, lmSF_M15, lmSF_M14, lmSF_M10)
# Model 1: DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + 
#   DataOG$Date + DataOG$Treatment + I(DataOG$Pt.seminatural * 
#                                        DataOG$Treatment)
# Model 2: DataOG$syrphid_fraction ~ DataOG$Pt.seminatural + DataOG$Date + 
#   DataOG$Treatment + I(DataOG$Pt.seminatural * DataOG$Date) + 
#   I(DataOG$Pt.seminatural * DataOG$Treatment)
# Model 3: DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + 
#   DataOG$Date + DataOG$Treatment + I(DataOG$Pt.seminatural * 
#                                        DataOG$Date) + I(DataOG$Pt.seminatural * DataOG$Treatment)
# Model 4: DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + 
#   DataOG$Date + DataOG$Treatment + I(DataOG$Field_Mgmt * DataOG$Date) + 
#   I(DataOG$Pt.seminatural * DataOG$Treatment)
# Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
# 1  12704 833.75                                  
# 2  12704 844.29  0  -10.5389                     
# 3  12703 833.16  1   11.1232 169.59 < 2.2e-16 ***
#  4  12703 830.37  0    2.7926       

anova(lmSF_M14, lmSF_M11)
# Model 1: DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + 
#   DataOG$Date + DataOG$Treatment + I(DataOG$Pt.seminatural * 
#                                        DataOG$Date) + I(DataOG$Pt.seminatural * DataOG$Treatment)
# Model 2: DataOG$syrphid_fraction ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + 
#   DataOG$Date + DataOG$Treatment + I(DataOG$Field_Mgmt * DataOG$Date) + 
#   I(DataOG$Pt.seminatural * DataOG$Date) + I(DataOG$Pt.seminatural * 
#                                                DataOG$Treatment)
# Res.Df    RSS Df Sum of Sq      F   Pr(>F)    
# 1  12703 833.16                                 
# 2  12702 830.24  1    2.9192 44.662 2.44e-11 ***


#lmSF_M11: best model