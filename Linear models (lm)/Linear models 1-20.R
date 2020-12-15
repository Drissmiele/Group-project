Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep = ";")

#1. Aphids density ~ Treatment

lm1 <- lm(Data1$aphid_live ~ Data1$Treatment)
summary(lm1)

# ggplot
library(ggplot2)
p <- ggplot(Data1, aes(x=Treatment,y=(aphid_live), color= Treatment)) +
  geom_point() + 
  geom_smooth(method = "lm")
print(p)

# Aphids density ~ Treatment (D1, D2, D3)

# Date 1
Date_1 <- Data1[Data1$Date == "1",]
lm1_1 <- lm(Date_1$aphid_live ~ Date_1$Treatment)

# ggplot
p <- ggplot(Data_1, aes(x=Treatment,y=(aphid_live), color= Treatment)) +
  geom_point() + 
  geom_smooth(method = "lm")
print(p)

# Date 2
Date_2 <- Data1[Data1$Date == "2",]
lm1_2 <- lm(Date_2$aphid_live ~ Date_2$Treatment)
plot(Date_2$aphid_live ~ as.numeric(as.factor(Date_2$Treatment)))

# ggplot
p <- ggplot(Date_2, aes(x=Treatment,y=(aphid_live), color= Treatment)) +
  geom_point() + 
  geom_smooth(method = "lm")
print(p)

# DATE3
Date_3 <- Data1[Data1$Date == "3",]
lm1_3 <- lm(Date_3$aphid_live ~ Date_3$Treatment)
plot(Date_3$aphid_live ~ as.numeric(as.factor(Date_3$Treatment)))

#ggplot
p <- ggplot(Date_3, aes(x=Treatment,y=(aphid_live), color= Treatment)) +
  geom_point() + 
  geom_smooth(method = "lm")
print(p)

# Aphid population growth (APG) ~ Treatment (D1, D2, D3)

# Date 1
Date_1 <- Data1[Data1$Date == "1",]
logNaphids_all <- log(Date_1$aphid_live + 1) - log(Date_1$aphidsinoculated_init + 1) 
APG_date1 <- logNaphids_all/10
Date_1$APG_date1 <- APG_date1

lm1_4 <- lm(Date_1$APG_date1 ~ Date_1$Treatment)
summary(lm1_4)

# ggplot
p <- ggplot(Date_1, aes(x=Treatment,y=(APG_date1), color= Treatment)) +
  geom_point() + 
  geom_smooth(method = "lm")
print(p)

# Date 2
Date_2 <- Data1[Data1$Date == "2",]
logNaphids_all <- log(Date_2$aphid_live + 1) - log(Date_2$aphidsinoculated_init + 1) 
APG_date2 <- logNaphids_all/20
Date_2$APG_date2 <- APG_date2

lm1_5 <- lm(Date_2$APG_date2 ~ Date_2$Treatment)
summary(lm1_5)

# ggplot
p <- ggplot(Date_2, aes(x=Treatment,y=(APG_date2), color= Treatment)) +
  geom_point() + 
  geom_smooth(method = "lm")
print(p)

# Date 3
Date_3 <- Data1[Data1$Date == "3",]
logNaphids_all <- log(Date_3$aphid_live + 1) - log(Date_3$aphidsinoculated_init + 1) 
APG_date3 <- logNaphids_all/30
Date_3$APG_date3 <- APG_date3

lm1_6 <- lm(Date_3$APG_date3 ~ Date_3$Treatment)
summary(lm1_6)


# ggplot
p <- ggplot(Date_3, aes(x=Treatment,y=(APG_date3), color= Treatment)) +
  geom_point() + 
  geom_smooth(method = "lm")
print(p)

#2. Aphid density ~ Aphid parasitized
lm2 <- lm(Data1$aphid_live ~ Data1$aphid_parasitized)
summary(lm2)

# ggplot
p <- ggplot(Data1, aes(x=aphid_parasitized,y=(aphid_live), color= aphid_parasitized)) +
  geom_point() + 
  geom_smooth(method="lm")
print(p)


#3. Aphids parasitized ~ aphid density
lm3 <- lm(Data1$aphid_parasitized ~ Data1$aphid_live)
summary(lm3)

# ggplot
p <- ggplot(Data1,aes(x=aphid_live,y=(aphid_parasitized), color= aphid_live)) +
  geom_point() + 
  geom_smooth(method="lm")
print(p)

#4. Aphid density ~ Syrphid fraction
syrphid_fraction <- (Data1$syrphidl_p /((Data1$aphid_live)+(Data1$syrphidl_p)))
Data1$syrphid_fraction <- syrphid_fraction

Data2 <- na.omit(Data1)
# => lots of NaN, Data2 has only half of the observations. 

lm4 <- lm(Data1$aphid_live~Data1$syrphid_fraction)
summary(lm4)

## ggplot
p <- ggplot(Data1,aes(x=syrphid_fraction,y=(aphid_live), color= aphid_live)) +
  geom_point() + 
  geom_smooth(method="lm")
print(p)

#5. Aphid density ~ Syrphid fraction 
lm4_2 <- lm(Data1$aphid_live ~Data1$syrphid_fraction)
summary(lm4_2)

# ggplot
p <- ggplot(Data1,aes(x=syrphid_fraction,y=(aphid_live), color= aphid_live)) +
  geom_point() + 
  geom_smooth(method="lm")
print(p)

#6. Biomass ~ Aphid density
lm6 <- lm(Data1$Biomass_fin ~ Data1$aphid_live)
summary(lm6)  
# p value = 0.0523

# plot
plot(Data1$Biomass_fin ~ Data1$aphid_live, ylab = "Biomass", xlab = "Aphids density")


#7. Aphids ~ Field Management (conventional/organic)
lm7 <- lm(Data1$aphid_live ~ Data1$Field_Mgmt)
summary(lm7)
# p value <2e-16

# plot
plot(Data1$aphid_live ~ as.factor(Data1$Field_Mgmt), xlab = "Field Management", ylab = "Aphids Density")


#8. Syrphid fraction ~ Field Management 
lm8 <- lm(Data1$syrphid_fraction ~ Data1$Field_Mgmt)
summary(lm8)
# p value <2e-16

# plot
plot(Data1$syrphid_fraction ~ as.factor(Data1$Field_Mgmt), ylab = "Syrphid", xlab = "Field Management")


#9. Aphids parasitized ~ Management 
lm9 <- lm(Data1$aphid_parasitized ~ Data1$Field_Mgmt)
summary(lm9)
# p value <2e-16

# plot
plot(Data1$aphid_parasitized ~ as.numeric(as.factor(Data1$Field_Mgmt)))
plot(Data1$aphid_parasitized ~ as.factor(Data1$Field_Mgmt), xlab = "Field Management", ylab = "Parasitized Aphids")

#10. Aphid density ~ crop maturity
lm10 <- lm(Data1$aphid_live ~ Data1$cropmaturity_init)
summary(lm10)
# p value <2e-16

# plot
plot(Data1$aphid_live ~ Data1$cropmaturity_init, ylab = "Aphids density", xlab = "crop maturity")


#11. Syrphid fraction ~ Crop maturity
lm11 <- lm(Data1$syrphid_fraction ~ Data1$cropmaturity_init)
summary(lm11)
# p value = 0.0537

#plot
plot(Data1$syrphid_fraction~ as.numeric(as.factor(Data1$cropmaturity_init)))
plot(Data1$syrphid_fraction ~ as.factor(Data1$cropmaturity_init), xlab = "Crop Maturity", ylab = "Syrphids")


#12. Aphid density ~ Crop type
lm12 <- lm(Data1$aphid_live ~ Data1$croptype)
summary(lm12)
# p value = 3.65e-10

# plot
plot(Data1$aphid_live ~ Data1$croptype, ylab = "Aphids density", xlab ="Crop type" )


#13. Syrphid fraction ~ Crop type
lm13 <- lm(Data1$syrphid_fraction ~ Data1$croptype)
summary(lm13)
# p value = 0.001193

# plot
plot(Data1$syrphid_fraction ~ as.factor(Data1$croptype), xlab = "Crop Type", ylab = "Syrphids")


#14. Aphids density ~ % Seminatural habitat
lm14 <- lm(Data1$aphid_live ~ Data1$Pt.seminatural)
summary(lm14)
# p value <2e-16

# plot
plot(Data1$aphid_live ~ Data1$Pt.seminatural, ylab = "Aphids density", xlab = "Seminatural habitat")


#15. Syrphid fraction ~ % Seminatural habitat
lm15 <- lm(Data1$syrphid_fraction ~ Data1$Pt.seminatural)
summary(lm15)
# p value = 7.02e-08

# plot
plot(Data1$syrphid_fraction~ as.numeric(as.factor(Data1$Pt.seminatural)))
plot(Data1$syrphid_fraction ~ as.factor(Data1$Pt.seminatural), xlab = "Pt.seminatural", ylab = "Syrphids")


#16. Aphids parasitized ~ % Seminatural habitat
lm16 <- lm(Data1$aphid_parasitized ~ Data1$Pt.seminatural)
summary(lm16)
#p value < 2e-16

# plot
plot(Data1$aphid_parasitized ~ Data1$Pt.seminatural,ylab = "Aphids parasitized", xlab = "Seminatural habitat")


#17. Cabbage biomass ~ % Seminatural habitat
lm17 <- lm(Data1$Biomass_fin ~ Data1$Pt.seminatural)
summary(lm17)
# p value = 0.98

# plot
plot(Data1$Biomass_fin ~ as.numeric(as.factor(Data1$Pt.seminatural)))
plot(Data1$Biomass_fin ~ as.factor(Data1$Pt.seminatural), xlab = "Pt.seminatural", ylab = "Biomass")


#18. Biomass ~ Field management 
lm18 <- lm(Data1$Biomass_fin ~ Data1$Field_Mgmt)
summary(lm18)
# p value = <2e-16

# plot
plot(Data1$Biomass_fin ~ as.factor(Data1$Field_Mgmt), ylab = "Biomass", xlab = "Field management")


#19. Aphids density ~ Plot ID 
lm19 <- lm(Data1$aphid_live ~ Data1$Plot_ID)
summary(lm19)
# p value = 0.025

# plot
plot(Data1$aphid_live ~ as.numeric(as.factor(Data1$Plot_ID)))
plot(Data1$aphid_live ~ as.factor(Data1$Plot_ID), xlab = "Plot", ylab = "Aphids")


#20 Aphids density ~ Date
lm20 <- lm(Data1$aphid_live ~ Data1$Date)
summary(lm20)
#p value <2e-16

# plot
plot(Data1$aphid_live ~ Data1$Date, ylab = "Aphids", xlab = "Date", col="red")


