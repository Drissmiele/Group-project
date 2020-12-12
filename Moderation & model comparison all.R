#6 aphids and biomass
lm6 <- lm(Data1$Biomass_fin ~ Data1$aphid_live)
summary(lm6)  

plot(Data1$Biomass_fin ~ Data1$aphid_live, ylab = "Biomass", xlab = "APhids density")
#P value = 0.0523
plot(Data1$Biomass_fin ~ as.numeric(as.factor(Data1$Field_Mgmt)))
plot(Data1$Biomass_fin ~ as.factor(Data1$Field_Mgmt), xlab = "Field Management", ylab = "Final Biomass")

# moderation effect aphids and biomass with Field management
lm6a <- lm(Data1$Biomass_fin ~ Data1$aphid_live + as.numeric(as.factor(Data1$Field_Mgmt)) + I(as.numeric(as.factor(Data1$aphid_live))* as.numeric(as.factor(Data1$Field_Mgmt))))
summary(lm6a)

#model comparison
AIC(lm6, lm6a)
# lm6 is better because lower AIC values indicate a better-fit model. 
# there is moderation