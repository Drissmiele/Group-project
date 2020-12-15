#14/12/2020

#Checking for direct relationship Crop Type - Syrphids with a simple linear model
lm13 <- lm(Data1$syrphidl_p ~ Data1$croptype)
summary(lm13)
#visualize the relationship
plot(Data1$syrphidl_p ~ as.factor(Data1$croptype), xlab = "Crop Type", ylab = "Syrphids")
library(ggplot2)                
p <- ggplot(Data1, aes(x = croptype , y = syrphidl_p, color = Treatment)) +
  geom_point() + 
  geom_smooth(method = "lm")
print(p)                             
#remarks: Syrphids were more on other crop types and slightly on Brassicaceae (impact on crop biomass) this could depend on the Field Management 

#Moderation effect of Syrphids - crop type with the Interaction of Field Mgt
lm13a <- lm(Data1$syrphidl_p ~ Data1$croptype + as.numeric(as.factor(Data1$Field_Mgmt)) + I(as.numeric(as.factor(Data1$aphid_live))* as.numeric(as.factor(Data1$Field_Mgmt))))
summary(lm13a)
#model comparison
AIC(lm13, lm13a)
# lm13a is better because lower AIC values indicate a better-fit model. 
# there is moderation 

