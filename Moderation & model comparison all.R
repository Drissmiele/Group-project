#6 aphids and biomass
lm6 <- lm(Data1$Biomass_fin ~ Data1$aphid_live)
summary(lm6) 
# P value = 0.0523 = effect of aphid density on biomass is not significant, by a little. 

# plot
plot(Data1$Biomass_fin ~ Data1$aphid_live, ylab = "Biomass", xlab = "Aphids density")
plot(Data1$Biomass_fin ~ as.factor(Data1$Field_Mgmt), xlab = "Field Management", ylab = "Final Biomass")

# moderation effect: aphids and biomass with Field management
lm6a <- lm(Data1$Biomass_fin ~ Data1$aphid_live + as.numeric(as.factor(Data1$Field_Mgmt)) + I(as.numeric(as.factor(Data1$aphid_live))* as.numeric(as.factor(Data1$Field_Mgmt))))
summary(lm6a)

#model comparison
AIC(lm6, lm6a)
# lm6 is better because lower AIC values indicate a better-fit model. 
# there is no moderation, the direct relationship is able to explain what is going on better

# 7 Field Management (conventional/organic) - aphids
lm7 <- lm(Data1$aphid_live ~ Data1$Field_Mgmt)
summary(lm7)
#Given the p-value approximately equal to 0, we can say there is a relationship between aphids density and field  management and it is statistically significant
#aphids density is more on conventional fields than on organic

# plot
plot(Data1$aphid_live ~ as.factor(Data1$Field_Mgmt), xlab = "Field Management", ylab = "Aphids Density")

# moderation effect: Field Management (conventional/organic) and aphids with a possible interaction with crop type
unique(Data1$croptype)
lm7a <- lm(Data1$aphid_live ~ as.numeric(as.factor(Data1$Field_Mgmt) + as.numeric(as.factor(Data1$croptype)) + I((as.numeric(as.factor(Data1$Field_Mgmt)))* (as.numeric(as.factor(Data1$croptype))))))
summary(lm7a)

#model comparison
AIC(lm7, lm7a)
# lm7 is better because lower AIC values indicate a better-fit model. 
# there is no moderation, the direct relationship is able to explain what is going on better

#8 Management and syruphids ll
lm8 <- lm(Data1$syrphidl_p ~ Data1$Field_Mgmt)
summary(lm8)
# p value =  <2e-16
#Plot
plot(Data1$syrphidl_p ~ as.factor(Data1$Field_Mgmt), ylab = "Syrphid", xlab = "Field Management")
