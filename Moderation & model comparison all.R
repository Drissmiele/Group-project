Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep = ";")

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
lm7a <- lm(Data1$aphid_live ~ as.numeric(as.factor(Data1$Field_Mgmt)) + as.numeric(as.factor(Data1$croptype)) + I(as.numeric(as.factor(Data1$Field_Mgmt))* as.numeric(as.factor(Data1$croptype))))
summary(lm7a)

#model comparison
AIC(lm7, lm7a)
# lm7a is better. 
# there is a little bit of moderation.

#8 Management and syruphids
lm8 <- lm(Data1$syrphidl_p ~ Data1$Field_Mgmt)
summary(lm8)
# p value =  <2e-16
#Plot
plot(Data1$syrphidl_p ~ as.factor(Data1$Field_Mgmt), ylab = "Syrphid", xlab = "Field Management")

# moderation effect: Field Management (conventional/organic) and Syrphid with a possible interaction with crop type
unique(Data1$croptype)
lm8a <- lm(Data1$syrphidl_p ~ as.numeric(as.factor(Data1$Field_Mgmt)) + as.numeric(as.factor(Data1$croptype)) + I(as.numeric(as.factor(Data1$Field_Mgmt))* as.numeric(as.factor(Data1$croptype))))
summary(lm8a)

#model comparison
AIC(lm8, lm8a)
# lm8a is better. 
# there is large moderation because some effects from A to B pass through C.

#9Management - aphids_parasitized
lm9 <- lm(Data1$aphid_parasitized ~ Data1$Field_Mgmt)
summary(lm9)
#Given the p-value approximately equal to 0, we can say there is a relationship between parasitized aphids and field  management and it is statistically significant
#parasitized aphids are more on organic fields than on conventional

#Plot
plot(Data1$aphid_parasitized ~ as.factor(Data1$Field_Mgmt), xlab = "Field Management", ylab = "Parasitized Aphids")

# moderation effect: Field Management (conventional/organic) and Aphid_parasitized with a possible interaction with crop type
unique(Data1$croptype)
lm9a <- lm(Data1$aphid_parasitized ~ as.numeric(as.factor(Data1$Field_Mgmt)) + as.numeric(as.factor(Data1$croptype)) + I(as.numeric(as.factor(Data1$Field_Mgmt))* as.numeric(as.factor(Data1$croptype))))
summary(lm9a)

#model comparison
AIC(lm9, lm9a)
# lm9a is better. 
# there is some moderation because some effects from A to B pass through C.

#10Crop_mat	and aphids
lm10 <- lm(Data1$aphid_live ~ Data1$cropmaturity_init)
summary(lm10)
#p value = <2e-16, ***

#plot
plot(Data1$aphid_live ~ Data1$cropmaturity_init, ylab = "Aphids density", xlab = "crop maturity")

# moderation effect: Crop_mat and aphids with Field management
lm10a <- lm(Data1$aphid_live ~ Data1$cropmaturity_init + as.numeric(as.factor(Data1$Field_Mgmt)) + I(as.numeric(as.factor(Data1$cropmaturity_init))* as.numeric(as.factor(Data1$Field_Mgmt))))
summary(lm10a)

#model comparison
AIC(lm10, lm10a)
# lm10a is better. 
# there is low moderation and some effects from A to B pass through C.

# moderation effect: Crop_mat and aphids with Syrphidl_p
lm10b <- lm(Data1$aphid_live ~ Data1$cropmaturity_init + as.numeric(as.factor(Data1$syrphidl_p)) + I(as.numeric(as.factor(Data1$cropmaturity_init))* as.numeric(as.factor(Data1$syrphidl_p))))
summary(lm10b)

#model comparison
AIC(lm10, lm10b)
#lm10b is better

#11Crop Maturity - Syrphids
lm11 <- lm(Data1$syrphidl_p ~ Data1$cropmaturity_init)
summary(lm11)
#p value = < 2.2e-16

#plot
plot(Data1$syrphidl_p ~ as.factor(Data1$cropmaturity_init), xlab = "Crop Maturity", ylab = "Syrphids")

# moderation effect: Crop_mat and syrphids with Field management
lm11a <- lm(Data1$syrphidl_p ~ Data1$cropmaturity_init + as.numeric(as.factor(Data1$Field_Mgmt)) + I(as.numeric(as.factor(Data1$cropmaturity_init))* as.numeric(as.factor(Data1$Field_Mgmt))))
summary(lm11a)

#model comparison
AIC(lm11, lm11a)
# lm11a is better. 
# there is low moderation and some effects from A to B pass through C.

#12Crop_type and	aphids
lm12 <- lm(Data1$aphid_live ~ Data1$croptype)
summary(lm12)
#p value  = 3.65e-10, ***

#plot
plot(Data1$aphid_live ~ as.factor(Data1$croptype), xlab = "Aphids density", ylab = "Crop type")


# moderation effect: Crop_type and aphids with Field management
lm12a <- lm(Data1$aphid_live ~ Data1$croptype + as.numeric(as.factor(Data1$Field_Mgmt)) + I(as.numeric(as.factor(Data1$croptype))* as.numeric(as.factor(Data1$Field_Mgmt))))
summary(lm12a)

#model comparison
AIC(lm12, lm12a)
# lm12a is better. 
# there is low moderation and some effects from A to B pass through C.