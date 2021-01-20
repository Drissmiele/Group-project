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


#Linear models from complex to simple

lmAP1 <- lm(DataOG$aphid_parasitized ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I((DataOG$Field_Mgmt)*(DataOG$Date)) + I((DataOG$Pt.seminatural)*(DataOG$Date)) + I((DataOG$Treatment)*(DataOG$Date) + I((DataOG$Pt.seminatural)*(DataOG$Treatment))))
lmAP2 <- lm(DataOG$aphid_parasitized ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I((DataOG$Field_Mgmt)*(DataOG$Date)) + I((DataOG$Pt.seminatural)*(DataOG$Date)) + I((DataOG$Treatment)*(DataOG$Date)))
lmAP3 <- lm(DataOG$aphid_parasitized ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I((DataOG$Field_Mgmt)*(DataOG$Date)) + I((DataOG$Pt.seminatural)*(DataOG$Date)))
lmAP4 <- lm(DataOG$aphid_parasitized ~ DataOG$Field_Mgmt + DataOG$croptype + DataOG$Pt.seminatural + DataOG$aphid_live + DataOG$APG)
lmAP5 <- lm(DataOG$aphid_parasitized ~ DataOG$aphid_live + DataOG$croptype)
lmAP6 <- lm(DataOG$aphid_parasitized ~ DataOG$aphid_live + DataOG$Pt.seminatural)
lmAP7 <- lm(DataOG$aphid_parasitized ~ DataOG$croptype + DataOG$Pt.seminatural)
lmAP8 <- lm(DataOG$aphid_parasitized ~ DataOG$croptype + DataOG$Field_Mgmt)
lmAP9 <- lm(DataOG$aphid_parasitized ~ DataOG$Pt.seminatural + DataOG$Field_Mgmt)
lmAP10 <- lm(DataOG$aphid_parasitized ~ DataOG$Pt.seminatural + DataOG$APG)
lmAP11 <- lm(DataOG$aphid_parasitized ~ DataOG$Field_Mgmt + DataOG$APG)
lmAP12 <- lm(DataOG$aphid_parasitized ~ DataOG$Pt.seminatural + DataOG$Treatment)
lmAP13 <- lm(DataOG$aphid_parasitized ~ DataOG$APG + DataOG$croptype)
lmAP14 <- lm(DataOG$aphid_parasitized ~ DataOG$Treatment)
lmAP15 <- lm(DataOG$aphid_parasitized ~ DataOG$aphid_live)
lmAP16 <- lm(DataOG$aphid_parasitized ~DataOG$croptype)
lmAP17 <- lm(DataOG$aphid_parasitized ~ DataOG$Pt.seminatural)
lmAP18 <- lm(DataOG$aphid_parasitized ~ DataOG$Field_Mgmt)
lmAP19 <- lm(DataOG$aphid_parasitized ~ DataOG$APG)
lmAP20 <- lm(DataOG$aphid_parasitized ~ DataOG$cropmaturity_init)


#AIC model comparison
AIC(lmAP1, lmAP2, lmAP3, lmAP4, lmAP5, lmAP6, lmAP7, lmAP8, lmAP9, lmAP10, lmAP11, lmAP12, lmAP13, lmAP14, lmAP15, lmAP16, lmAP17, lmAP18, lmAP19, lmAP20)


# df      AIC
# lmAP1   9 78258.94
# lmAP2   9 78272.80
# lmAP3   8 78285.50
# lmAP4   7 78178.60  #lowest AIC value = best model
# lmAP5   4 78591.33
# lmAP6   4 78433.43
# lmAP7   4 78448.56
# lmAP8   4 78609.79
# lmAP9   4 78461.46
# lmAP10  4 78226.79
# lmAP11  4 78347.11
# lmAP12  4 78471.63
# lmAP13  4 78334.34
# lmAP14  3 78707.01
# lmAP15  3 78639.51
# lmAP16  3 78668.07
# lmAP17  3 78488.26
# lmAP18  3 78649.44
# lmAP19  3 78370.83
# lmAP20  3 78645.36


#anova model comparison

anova(lmAP20, lmAP19)
#no p value

anova(lmAP20, lmAP18)
#no p value

anova(lmAP20, lmAP17)
#no p value

anova(lmAP20, lmAP16)
#no p value

anova(lmAP20, lmAP15)
#no p value

anova(lmAP20, lmAP14)
#no p value

anova(lmAP20, lmAP13)
# < 2.2e-16 ***

anova(lmAP13, lmAP12)
#no p value

anova(lmAP13, lmAP11)
#no p value

anova(lmAP13, lmAP10)
#no p value

anova(lmAP13, lmAP9)
#no p value

anova(lmAP13, lmAP8)
#no p value

anova(lmAP13, lmAP7)
#no p value

anova(lmAP13, lmAP6)
#no p value

anova(lmAP13, lmAP5)
#no p value

anova(lmAP13, lmAP4)
# < 2.2e-16 ***

anova(lmAP4, lmAP3)
#no p value

anova(lmAP4, lmAP2)
#no p value

anova(lmAP4, lmAP1)
#no p value

# M4 appears to be the best model.



