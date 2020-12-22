# Retrieve data
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep = ";")

#6. Biomass ~ Aphid density
lm6 <- lm(Data1$Biomass_fin ~ Data1$aphid_live)
summary(lm6) 
# P value = 0.0523 

# Interaction effect with Field Management
lm6a <- lm(Data1$Biomass_fin ~ Data1$aphid_live + as.numeric(as.factor(Data1$Field_Mgmt)) + I(as.numeric(as.factor(Data1$aphid_live))* as.numeric(as.factor(Data1$Field_Mgmt))))
summary(lm6a)
# Interaction significant (p = 3.74e-05)

# Model comparison
AIC(lm6, lm6a)
# lm6a is better because lower AIC values indicate a better-fit model. 
# When taking into account Field Management, the model is better.

anova(lm6, lm6a)
# lm6a is significantly better than lm6.


#7. Aphids density ~ Field Management 
lm7 <- lm(Data1$aphid_live ~ Data1$Field_Mgmt)
summary(lm7)
# p value <2e-16

# Interaction effect with crop type
lm7a <- lm(Data1$aphid_live ~ as.numeric(as.factor(Data1$Field_Mgmt)) + as.numeric(as.factor(Data1$croptype)) + I(as.numeric(as.factor(Data1$Field_Mgmt))* as.numeric(as.factor(Data1$croptype))))
summary(lm7a)
# Interaction significant (p = 1.91e-13)

# Model comparison
AIC(lm7, lm7a)
# lm7a is better because lower AIC values indicate a better-fit model. 
# When taking into account crop type, the model is better.

anova(lm7, lm7a)
# lm7a is significantly better than lm7.


#8. Syrphid fraction ~ Field Management 
syrphid_fraction <- (Data1$syrphidl_p /((Data1$aphid_live)+(Data1$syrphidl_p)))
Data1$syrphid_fraction <- syrphid_fraction

lm8 <- lm(Data1$syrphid_fraction ~ Data1$Field_Mgmt)
summary(lm8)
# p value = 1.23e-08

# Interaction effect with crop type

lm8a <- lm(Data1$syrphid_fraction ~ as.numeric(as.factor(Data1$Field_Mgmt)) + as.numeric(as.factor(Data1$croptype)) + I(as.numeric(as.factor(Data1$Field_Mgmt))* as.numeric(as.factor(Data1$croptype))))
summary(lm8a)
# Interaction significant (p = 5.54e-05)

# Model comparison
AIC(lm8, lm8a)
# lm8a is better because lower AIC values indicate a better-fit model. 
# When taking into account crop type, the model is better.

anova(lm8,lm8a)
# lm8a is significantly better than lm8


#9. Aphids parasitized ~ Field Management
lm9 <- lm(Data1$aphid_parasitized ~ Data1$Field_Mgmt)
summary(lm9)
# p value <2e-16

# Interaction effect with crop type
lm9a <- lm(Data1$aphid_parasitized ~ as.numeric(as.factor(Data1$Field_Mgmt)) + as.numeric(as.factor(Data1$croptype)) + I(as.numeric(as.factor(Data1$Field_Mgmt))* as.numeric(as.factor(Data1$croptype))))
summary(lm9a)
# Interaction significant (p < 2e-16)

# Model comparison
AIC(lm9, lm9a)
# lm9a is better because lower AIC values indicate a better-fit model. 
# When taking into account crop type, the model is better.

anova(lm9, lm9a)
# lm9a is significantly better than lm9

#10. Aphid density ~ Crop maturity 
lm10 <- lm(Data1$aphid_live ~ Data1$cropmaturity_init)
summary(lm10)
# p value <2e-16 

# (a) Interaction effect with Field Management 
lm10a <- lm(Data1$aphid_live ~ Data1$cropmaturity_init + as.numeric(as.factor(Data1$Field_Mgmt)) + I(as.numeric(as.factor(Data1$cropmaturity_init))* as.numeric(as.factor(Data1$Field_Mgmt))))
summary(lm10a)

# Model comparison
AIC(lm10, lm10a)
# lm10a is better because lower AIC values indicate a better-fit model. 
# When taking into account Field management, the model is better

anova(lm10, lm10a)
# lm10a is significantly better than lm10

# (b) Interaction effect with syrphid fraction 
lm10b <- lm(Data1$aphid_live ~ Data1$cropmaturity_init + Data1$syrphid_fraction + I(as.numeric(as.factor(Data1$cropmaturity_init))* Data1$syrphid_fraction))
summary(lm10b)
# Interaction significant(p <2e-16)

# Model comparison
AIC(lm10, lm10b)
#lm10b appears to be better as lower AIC values indicate a better-fit model.
# When taking into account syrphid fraction, the model is better. 


#11. Syrphid fraction ~ Crop initial maturity 
lm11 <- lm(Data1$syrphid_fraction ~ Data1$cropmaturity_init)
summary(lm11)
# p value = 0.0537


# Interaction effet with Field management
lm11a <- lm(Data1$syrphid_fraction ~ Data1$cropmaturity_init + as.numeric(as.factor(Data1$Field_Mgmt)) + I(as.numeric(as.factor(Data1$cropmaturity_init))* as.numeric(as.factor(Data1$Field_Mgmt))))
summary(lm11a)
# Interaction not significant (p =  0.87794)


# Model comparison
AIC(lm11, lm11a)
# lm11a appears to be better as lower AIC values indicate a better-fit model.
# When taking into account field management, the model is better. 

anova (lm11, lm11a)
# lm11a is significantly better than lm11


#12. Aphid density ~ Crop type
lm12 <- lm(Data1$aphid_live ~ Data1$croptype)
summary(lm12)
# p value  = 3.65e-10

# Interaction effect with Field management
lm12a <- lm(Data1$aphid_live ~ Data1$croptype + as.numeric(as.factor(Data1$Field_Mgmt)) + I(as.numeric(as.factor(Data1$croptype))* as.numeric(as.factor(Data1$Field_Mgmt))))
summary(lm12a)
# Interaction significant (p = 1.91e-13)

# Model comparison
AIC(lm12, lm12a)
# lm12a appears to be better as lower AIC values indicate a better-fit model.
# When taking into account field management, the model is better. 

anova(lm12, lm12a)
# lm12a is significantly better than lm12


# Syrphid fraction ~ crop type
lm13 <- lm(Data1$syrphid_fraction ~ Data1$croptype)
summary(lm13)
# p = 0.00119

# Interaction effect with Field management 
lm13a <- lm(Data1$syrphid_fraction ~ Data1$croptype + as.numeric(as.factor(Data1$Field_Mgmt)) + I(as.numeric(as.factor(Data1$croptype))* as.numeric(as.factor(Data1$Field_Mgmt))))
summary(lm13a)
# Interaction significant (p = 5.54e-05)

# Model comparison
AIC(lm13, lm13a)
# lm13a is better because lower AIC values indicate a better-fit model. 
# When taking into account field management, the model is better.

anova(lm13, lm13a)
# lm13a is significantly better than lm13
