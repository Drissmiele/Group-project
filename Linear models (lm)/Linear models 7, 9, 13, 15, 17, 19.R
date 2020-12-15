
##########################################################

# Field Management (conventional/organic) - aphids
lm7 <- lm(Data1$aphid_live ~ Data1$Field_Mgmt)
summary(lm7)
plot(Data1$aphid_live ~ as.numeric(as.factor(Data1$Field_Mgmt)))
plot(Data1$aphid_live ~ as.factor(Data1$Field_Mgmt), xlab = "Field Management", ylab = "Aphids Density")
#Given the p-value approximately equal to 0, we can say there is a relationship between aphids density and field  management and it is statistically significant
#aphids density is more on conventional fields than on organic
#Next we will try to detect and understand interactions between both variables
#NO IDEA?

#Management - aphids_parasitized
lm9 <- lm(Data1$aphid_parasitized ~ Data1$Field_Mgmt)
summary(lm9)
plot(Data1$aphid_parasitized ~ as.numeric(as.factor(Data1$Field_Mgmt)))
plot(Data1$aphid_parasitized ~ as.factor(Data1$Field_Mgmt), xlab = "Field Management", ylab = "Parasitized Aphids")
#Given the p-value approximately equal to 0, we can say there is a relationship between parasitized aphids and field  management and it is statistically significant
#parasitized aphids are more on organic fields than on conventional
#why?

#Crop Maturity - Syrphids
lm11 <- lm(Data1$syrphidl_p ~ Data1$cropmaturity_init)
summary(lm11)
plot(Data1$syrphidl_p~ as.numeric(as.factor(Data1$cropmaturity_init)))
plot(Data1$syrphidl_p ~ as.factor(Data1$cropmaturity_init), xlab = "Crop Maturity", ylab = "Syrphids")

#Crop Type - Syrphids
lm13 <- lm(Data1$syrphidl_p ~ Data1$croptype)
summary(lm13)
plot(Data1$syrphidl_p ~ as.factor(Data1$croptype), xlab = "Crop Type", ylab = "Syrphids")


#seminatural - Syrphids 
lm15 <- lm(Data1$syrphidl_p ~ Data1$Pt.seminatural)
summary(lm15)
plot(Data1$syrphidl_p~ as.numeric(as.factor(Data1$Pt.seminatural)))
plot(Data1$syrphidl_p ~ as.factor(Data1$Pt.seminatural), xlab = "Pt.seminatural", ylab = "Syrphids")

#Seminatural - Biomass
lm17 <- lm(Data1$Biomass_fin ~ Data1$Pt.seminatural)
summary(lm17)
#it is not statistically significant
plot(Data1$Biomass_fin ~ as.numeric(as.factor(Data1$Pt.seminatural)))
plot(Data1$Biomass_fin ~ as.factor(Data1$Pt.seminatural), xlab = "Pt.seminatural", ylab = "Biomass")
#Tried to plot this with ggplot but its not working 
#library(ggplot2)
#p <- ggplot(Data1, aes(x= Pt.seminatural)), y = (Biomass_fin)) +
         #geom_point()+
  #geom_smooth(method="lm")
#print(p)

# Plot - Aphids 
lm19 <- lm(Data1$aphid_live ~ Data1$Plot_ID)
summary(lm19)
#it is not statistically significant
plot(Data1$aphid_live ~ as.numeric(as.factor(Data1$Plot_ID)))
plot(Data1$aphid_live ~ as.factor(Data1$Plot_ID), xlab = "Plot", ylab = "Aphids")
                  
                  
                  
                  
                  
                  
                  