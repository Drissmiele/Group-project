#model comparison for Parasitism rate lm

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

#linear model for parasitism rate ordered from complex to simple

lmPR1 <- lm(DataOG$parasitism_rate ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I((DataOG$Field_Mgmt)*(DataOG$Date)) + I((DataOG$Pt.seminatural)*(DataOG$Date)) + I((DataOG$Treatment)*(DataOG$Date) + I((DataOG$Pt.seminatural)*(DataOG$Treatment))))
lmPR2 <- lm(DataOG$parasitism_rate ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I((DataOG$Field_Mgmt)*(DataOG$Date)) + I((DataOG$Pt.seminatural)*(DataOG$Date)) + I((DataOG$Pt.seminatural)*(DataOG$Treatment)))
lmPR3 <- lm(DataOG$parasitism_rate ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I((DataOG$Field_Mgmt)*(DataOG$Date)) + I((DataOG$Pt.seminatural)*(DataOG$Treatment)))
lmPR4 <- lm(DataOG$parasitism_rate ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I((DataOG$Field_Mgmt)*(DataOG$Date)) + I((DataOG$Pt.seminatural)*(DataOG$Date)))
lmPR5 <- lm(DataOG$parasitism_rate ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I((DataOG$Pt.seminatural)*(DataOG$Date)) + I((DataOG$Pt.seminatural)*(DataOG$Treatment)))
lmPR6 <- lm(DataOG$parasitism_rate ~ DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I((DataOG$Pt.seminatural)*(DataOG$Date)) + I((DataOG$Pt.seminatural)*(DataOG$Treatment)))
lmPR7 <- lm(DataOG$parasitism_rate ~ DataOG$Field_Mgmt + DataOG$Treatment + DataOG$cropmaturity_init + DataOG$Pt.seminatural)
lmPR8 <- lm(DataOG$parasitism_rate ~ DataOG$Pt.seminatural + DataOG$Treatment + DataOG$cropmaturity_init + DataOG$Field_Mgmt)
lmPR9 <- lm(DataOG$parasitism_rate ~ DataOG$cropmaturity_init + DataOG$Treatment + DataOG$Field_Mgmt + DataOG$Pt.seminatural)
lmPR10 <- lm(DataOG$parasitism_rate ~ DataOG$Treatment + DataOG$cropmaturity_init + DataOG$Field_Mgmt + DataOG$Pt.seminatural)
lmPR11 <- lm(DataOG$parasitism_rate ~ DataOG$Field_Mgmt)
lmPR12 <- lm(DataOG$parasitism_rate ~ DataOG$Pt.seminatural)
lmPR13 <- lm(DataOG$parasitism_rate ~ DataOG$cropmaturity_init)
lmPR14 <- lm(DataOG$parasitism_rate ~ DataOG$Treatment)
lmPR15 <- lm(DataOG$parasitism_rate ~ DataOG$croptype)

#AIC model comparison
AIC(lmPR1, lmPR2, lmPR3, lmPR4, lmPR5, lmPR6, lmPR7, lmPR8, lmPR9, lmPR10, lmPR11, lmPR12, lmPR13, lmPR14, lmPR15)

# df      AIC
# lmPR1   9 3627.890
# lmPR2   9 3625.303
# lmPR3   8 3623.658 #lowest
# lmPR4   8 3683.080
# lmPR5   8 3692.778
# lmPR6   7 3814.166
# lmPR7   6 3723.120
# lmPR8   6 3723.120
# lmPR9   6 3723.120
# lmPR10  6 3723.120
# lmPR11  3 4129.413
# lmPR12  3 4014.469
# lmPR13  3 4264.808
# lmPR14  3 4219.207
# lmPR15  3 4336.550


# Anova model comparison


anova(lmPR15, lmPR14)
#no p value

anova(lmPR15, lmPR13)
# no p value

anova(lmPR15, lmPR12)
# no p value

anova(lmPR15, lmPR11)
# p < 2.2e-16 ***

anova(lmPR15, lmPR10)
# < 2.2e-16 ***

anova(lmPR10, lmPR9)
# no p value

anova(lmPR10, lmPR8)
# no p value

anova(lmPR10, lmPR7)
#no p value

anova(lmPR10, lmPR6)
#no p value

anova(lmPR10, lmPR5)
#3.521e-08 ***

anova(lmPR5, lmPR4)
# no p value

anova(lmPR5, lmPR3)
#no p value

anova(lmPR5, lmPR2)
#< 2.2e-16 ***

anova(lmPR2, lmPR1)
#no p value

# M2 appears to be the best model, significantly different and better than the other models..

