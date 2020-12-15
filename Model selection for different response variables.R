Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep =";")

# response on parasitism rate
parasitism_rate　<- Data1$aphid_parasitized/((Data1$aphid_live)+(Data1$aphid_parasitized))
Data1$parasitism_rate <- parasitism_rate


lm2 <- lm(Data1$parasitism_rate ~ as.numeric(as.factor(Data1$Field_Mgmt)) + Data1$Pt.seminatural + as.numeric(as.factor(Data1$Date)) + as.numeric(as.factor(Data1$Treatment)) + I((as.numeric(as.factor(Data1$Field_Mgmt)))*(as.numeric(as.factor(Data1$Date))))+I(Data1$Pt.seminatural*as.numeric(as.factor(Data1$Date))) +I(Data1$Pt.seminatural*as.numeric(as.factor(Data1$Treatment))))
summary(lm2)
lm3 <- lm(Data1$parasitism_rate ~ as.numeric(as.factor(Data1$Field_Mgmt)) + Data1$Pt.seminatural + as.numeric(as.factor(Data1$Date)) + as.numeric(as.factor(Data1$Treatment)) + I((as.numeric(as.factor(Data1$Field_Mgmt)))*(as.numeric(as.factor(Data1$Date))))+I(Data1$Pt.seminatural*as.numeric(as.factor(Data1$Treatment))))
summary(lm3)
lm4 <- lm(Data1$parasitism_rate ~ Data1$Pt.seminatural + as.numeric(as.factor(Data1$Date)) + as.numeric(as.factor(Data1$Treatment)) +I(Data1$Pt.seminatural*as.numeric(as.factor(Data1$Date))) +I(Data1$Pt.seminatural*as.numeric(as.factor(Data1$Treatment))))
summary(lm4)
lm5 <- lm(Data1$parasitism_rate ~ as.numeric(as.factor(Data1$Field_Mgmt)) + Data1$Pt.seminatural + as.numeric(as.factor(Data1$Date)) + as.numeric(as.factor(Data1$Treatment)) +I(Data1$Pt.seminatural*as.numeric(as.factor(Data1$Date))) +I(Data1$Pt.seminatural*as.numeric(as.factor(Data1$Treatment))))
summary(lm5)
lm6 <- lm(Data1$parasitism_rate ~ as.numeric(as.factor(Data1$Field_Mgmt)) + Data1$Pt.seminatural + as.numeric(as.factor(Data1$Date)) + as.numeric(as.factor(Data1$Treatment)) + I((as.numeric(as.factor(Data1$Field_Mgmt)))*(as.numeric(as.factor(Data1$Date))))+I(Data1$Pt.seminatural*as.numeric(as.factor(Data1$Date))) +I((as.numeric(as.factor(Data1$Treatment)))*(as.numeric(as.factor(Data1$Date))))+ I(Data1$Pt.seminatural*(as.numeric(as.factor(Data1$Treatment)))))
summary(lm6)
lm7 <- lm(Data1$parasitism_rate ~ as.numeric(as.factor(Data1$Field_Mgmt)) + Data1$Pt.seminatural + as.numeric(as.factor(Data1$Date)) + as.numeric(as.factor(Data1$Treatment)) + I((as.numeric(as.factor(Data1$Field_Mgmt)))*(as.numeric(as.factor(Data1$Date))))+I(Data1$Pt.seminatural*as.numeric(as.factor(Data1$Date))))
summary(lm7)

# AIC (comparaison of models)
AIC(lm2, lm3, lm4, lm5, lm6, lm7)
AIC(lm2)


# response on syrphid fraction

syrphid_fraction <- (Data1$syrphidl_p /((Data1$aphid_live)+(Data1$syrphidl_p)))
Data1$syrphid_fraction <- syrphid_fraction

lm8 <- lm(Data1$syrphid_fraction ~ as.numeric(as.factor(Data1$Field_Mgmt)) + Data1$Pt.seminatural + as.numeric(as.factor(Data1$Date)) + as.numeric(as.factor(Data1$Treatment))+I(Data1$Pt.seminatural*as.numeric(as.factor(Data1$Treatment))))
summary(lm8)
lm9 <- lm(Data1$syrphid_fraction ~ Data1$Pt.seminatural + as.numeric(as.factor(Data1$Date)) + as.numeric(as.factor(Data1$Treatment))+I(Data1$Pt.seminatural*as.numeric(as.factor(Data1$Treatment))))
summary(lm9)
lm10 <-  lm(Data1$syrphid_fraction ~ as.numeric(as.factor(Data1$Field_Mgmt)) + Data1$Pt.seminatural + as.numeric(as.factor(Data1$Date)) + as.numeric(as.factor(Data1$Treatment))+I((as.numeric(as.factor(Data1$Field_Mgmt)))*(as.numeric(as.factor(Data1$Date)))) + I(Data1$Pt.seminatural*as.numeric(as.factor(Data1$Treatment))))
summary(lm10)
lm11 <- lm(Data1$syrphid_fraction ~ as.numeric(as.factor(Data1$Field_Mgmt)) + Data1$Pt.seminatural + as.numeric(as.factor(Data1$Date)) + as.numeric(as.factor(Data1$Treatment))+I((as.numeric(as.factor(Data1$Field_Mgmt)))*(as.numeric(as.factor(Data1$Date)))) + I(Data1$Pt.seminatural*as.numeric(as.factor(Data1$Date))) +I(Data1$Pt.seminatural*as.numeric(as.factor(Data1$Treatment))))
summary(lm11)
lm12 <- lm(Data1$syrphid_fraction ~ as.numeric(as.factor(Data1$Field_Mgmt)) + as.numeric(as.factor(Data1$Date)) + as.numeric(as.factor(Data1$Treatment))+I((as.numeric(as.factor(Data1$Field_Mgmt)))*(as.numeric(as.factor(Data1$Date)))))
summary(lm12)
lm13 <- lm()