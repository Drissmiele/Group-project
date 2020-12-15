
library(readr)
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep = ";")
View(Data1)

#6 aphids and biomass
lm6 <- lm(Data1$Biomass_fin ~ Data1$aphid_live)
summary(lm6)  

plot(Data1$Biomass_fin ~ Data1$aphid_live, ylab = "Biomass", xlab = "APhids density")
#P value = 0.0523


# 7 Field Management (conventional/organic) - aphids
lm7 <- lm(Data1$aphid_live ~ Data1$Field_Mgmt)
summary(lm7)
plot(Data1$aphid_live ~ as.numeric(as.factor(Data1$Field_Mgmt)))
plot(Data1$aphid_live ~ as.factor(Data1$Field_Mgmt), xlab = "Field Management", ylab = "Aphids Density")

#Given the p-value approximately equal to 0, we can say there is a relationship between aphids density and field  management and it is statistically significant
#aphids density is more on conventional fields than on organic
#Next we will try to detect and understand interactions between both variables
#NO IDEA?

#8 Management and syruphids
lm8 <- lm(Data1$syrphidl_p ~ Data1$Field_Mgmt)
summary(lm8)
plot(Data1$syrphidl_p ~ as.factor(Data1$Field_Mgmt), ylab = "Syrphid", xlab = "Field Management")
# p value =  <2e-16

#9Management - aphids_parasitized
lm9 <- lm(Data1$aphid_parasitized ~ Data1$Field_Mgmt)
summary(lm9)
plot(Data1$aphid_parasitized ~ as.numeric(as.factor(Data1$Field_Mgmt)))
plot(Data1$aphid_parasitized ~ as.factor(Data1$Field_Mgmt), xlab = "Field Management", ylab = "Parasitized Aphids")
#Given the p-value approximately equal to 0, we can say there is a relationship between parasitized aphids and field  management and it is statistically significant
#parasitized aphids are more on organic fields than on conventional
#why?

#10Crop_mat	and aphids
lm10 <- lm(Data1$aphid_live ~ Data1$cropmaturity_init)
summary(lm10)
plot(Data1$aphid_live ~ Data1$cropmaturity_init, ylab = "Aphids density", xlab = "crop maturity")
#p value = <2e-16, ***

#11Crop Maturity - Syrphids
lm11 <- lm(Data1$syrphidl_p ~ Data1$cropmaturity_init)
summary(lm11)
plot(Data1$syrphidl_p~ as.numeric(as.factor(Data1$cropmaturity_init)))
plot(Data1$syrphidl_p ~ as.factor(Data1$cropmaturity_init), xlab = "Crop Maturity", ylab = "Syrphids")


#12Crop_type and	aphids
lm12 <- lm(Data1$aphid_live ~ Data1$croptype)
summary(lm12)
plot(Data1$aphid_live ~ Data1$croptype, ylab = "Aphids density", xlab ="Crop type" )
#p value  = 3.65e-10, ***

#13Crop Type - Syrphids
lm13 <- lm(Data1$syrphidl_p ~ Data1$croptype)
summary(lm13)
plot(Data1$syrphidl_p ~ as.factor(Data1$croptype), xlab = "Crop Type", ylab = "Syrphids")


#14 Seminat	and aphids
lm14 <- lm(Data1$aphid_live ~ Data1$Pt.seminatural)
summary(lm14)
plot(Data1$aphid_live ~ Data1$Pt.seminatural, ylab = "Aphids density", xlab = "Seminatural habitat")
# p value <2e-16 ***

#15seminatural - Syrphids 
lm15 <- lm(Data1$syrphidl_p ~ Data1$Pt.seminatural)
summary(lm15)
plot(Data1$syrphidl_p~ as.numeric(as.factor(Data1$Pt.seminatural)))
plot(Data1$syrphidl_p ~ as.factor(Data1$Pt.seminatural), xlab = "Pt.seminatural", ylab = "Syrphids")


#16 Seminat	and aphids_parasitized
lm16 <- lm(Data1$aphid_parasitized ~ Data1$Pt.seminatural)
summary(lm16)
plot(Data1$aphid_parasitized ~ Data1$Pt.seminatural,ylab = "Aphids parasitized", xlab = "Seminatural habitat")
#p value < 2e-16 ***

#17Seminatural - Biomass
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


#18 Management and Biomass
lm18 <- lm(Data1$Biomass_fin ~ Data1$Field_Mgmt)
summary(lm18)
plot(Data1$Biomass_fin ~ as.factor(Data1$Field_Mgmt), ylab = "Biomass", xlab = "Field management")
#p value  <2e-16 ***

#19Plot - Aphids 
lm19 <- lm(Data1$aphid_live ~ Data1$Plot_ID)
summary(lm19)
#it is not statistically significant
plot(Data1$aphid_live ~ as.numeric(as.factor(Data1$Plot_ID)))
plot(Data1$aphid_live ~ as.factor(Data1$Plot_ID), xlab = "Plot", ylab = "Aphids")


#20 aphids and Date
lm20 <- lm(Data1$aphid_live ~ Data1$Date)
summary(lm20)
plot(Data1$aphid_live ~ Data1$Date, ylab = "Aphids", xlab = "Date", col="red")
#p value  <2e-16 ***


ANOVAlm1 <- aov(Data1$aphid_live ~ Data1$Treatment, data = Data1)
summary(ANOVAlm1)
library("agricolae")
HSDtest <- HSD.test(ANOVAlm1, "Treatment" )
HSDtest
boxplot(Data1$aphid_live ~ Data1$Treatment, data = Data1)
