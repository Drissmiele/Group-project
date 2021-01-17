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


#linear model
lmPR_M2 <- lm(DataOG$parasitism_rate ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I((DataOG$Field_Mgmt)*(DataOG$Date)) + I((DataOG$Pt.seminatural)*(DataOG$Date)) + I((DataOG$Field_Mgmt)*(DataOG$Treatment)))
lmPR_M3<- lm(DataOG$parasitism_rate ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I((DataOG$Field_Mgmt)*(DataOG$Date)) + I((DataOG$Pt.seminatural)*(DataOG$Treatment)))
lmPR_M4 <- lm(DataOG$parasitism_rate ~ DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I((DataOG$Pt.seminatural)*(DataOG$Date)) + I((DataOG$Pt.seminatural)*(DataOG$Treatment)))
lmPR_M5 <- lm(DataOG$parasitism_rate ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I((DataOG$Pt.seminatural)*(DataOG$Date)) + I((DataOG$Pt.seminatural)*(DataOG$Treatment)))
lmPR_M6 <- lm(DataOG$parasitism_rate ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I((DataOG$Field_Mgmt)*(DataOG$Date)) + I((DataOG$Pt.seminatural)*(DataOG$Date)) + I((DataOG$Treatment)*(DataOG$Date) + I((DataOG$Pt.seminatural)*(DataOG$Treatment))))
lmPR_M7<- lm(DataOG$parasitism_rate ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I((DataOG$Field_Mgmt)*(DataOG$Date)) + I((DataOG$Pt.seminatural)*(DataOG$Date)))

#AIC
AIC(lmPR_M2, lmPR_M3, lmPR_M4,lmPR_M5, lmPR_M6,lmPR_M7)

# df      AIC
# lmPR_M2  9 72303.25
# lmPR_M3  8 72352.78
# lmPR_M4  7 72305.84
# lmPR_M5  8 72278.21 #lowest
# lmPR_M6  9 72281.11
# lmPR_M7  8 72304.96

#lmPR_M5: best model

#ANOVA
anova(lmPR_M4, lmPR_M3)
# Model 1: DataOG$parasitism_rate ~ DataOG$Pt.seminatural + DataOG$Date + 
#   DataOG$Treatment + I((DataOG$Pt.seminatural) * (DataOG$Date)) + 
#   I((DataOG$Pt.seminatural) * (DataOG$Treatment))
# Model 2: DataOG$parasitism_rate ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + 
#   DataOG$Date + DataOG$Treatment + I((DataOG$Field_Mgmt) * 
#                                        (DataOG$Date)) + I((DataOG$Pt.seminatural) * (DataOG$Treatment))
# Res.Df    RSS Df Sum of Sq F Pr(>F)
# 1  12704 219709                      
# 2  12703 220487  1   -778.34         

anova(lmPR_M4, lmPR_M5)
#M5 5.271e-08 ***


anova(lmPR_M3, lmPR_M5)
# Model 1: DataOG$parasitism_rate ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + 
#   DataOG$Date + DataOG$Treatment + I((DataOG$Field_Mgmt) * 
#                                        (DataOG$Date)) + I((DataOG$Pt.seminatural) * (DataOG$Treatment))
# Model 2: DataOG$parasitism_rate ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + 
#   DataOG$Date + DataOG$Treatment + I((DataOG$Pt.seminatural) * 
#                                        (DataOG$Date)) + I((DataOG$Pt.seminatural) * (DataOG$Treatment))
# Res.Df    RSS Df Sum of Sq F Pr(>F)
# 1  12703 220487                      
# 2  12703 219197  0      1290         


anova(lmPR_M3, lmPR_M4, lmPR_M5)
#M5: 5.763e-08 ***

anova(lmPR_M5, lmPR_M7)
# Model 1: DataOG$parasitism_rate ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + 
#   DataOG$Date + DataOG$Treatment + I((DataOG$Pt.seminatural) * 
#                                        (DataOG$Date)) + I((DataOG$Pt.seminatural) * (DataOG$Treatment))
# Model 2: DataOG$parasitism_rate ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + 
#   DataOG$Date + DataOG$Treatment + I((DataOG$Field_Mgmt) * 
#                                        (DataOG$Date)) + I((DataOG$Pt.seminatural) * (DataOG$Date))
# Res.Df    RSS Df Sum of Sq F Pr(>F)
# 1  12703 219197                      
# 2  12703 219659  0   -461.93        

anova(lmPR_M5, lmPR_M7, lmPR_M6)
#M6: 3.728e-07 ***

anova(lmPR_M6, lmPR_M2)
# Model 1: DataOG$parasitism_rate ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + 
#   DataOG$Date + DataOG$Treatment + I((DataOG$Field_Mgmt) * 
#                                        (DataOG$Date)) + I((DataOG$Pt.seminatural) * (DataOG$Date)) + 
#   I((DataOG$Treatment) * (DataOG$Date) + I((DataOG$Pt.seminatural) * 
#                                              (DataOG$Treatment)))
# Model 2: DataOG$parasitism_rate ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + 
#   DataOG$Date + DataOG$Treatment + I((DataOG$Field_Mgmt) * 
#                                        (DataOG$Date)) + I((DataOG$Pt.seminatural) * (DataOG$Date)) + 
#   I((DataOG$Field_Mgmt) * (DataOG$Treatment))
# Res.Df    RSS Df Sum of Sq F Pr(>F)
# 1  12702 219213                      
# 2  12702 219595  0   -382.04   


#lmPR_M6, lmPR_M2: better models
