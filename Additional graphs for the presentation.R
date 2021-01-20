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


# 1. APG ~ parasitism_rate

plot(APG ~ parasitism_rate, data = DataOG, xlab="Parasitism rate", main = "Effect of parasitism rate on APG across all dates")
abline(lm(APG ~ parasitism_rate, data = DataOG))

# ggplot

Plot1<- ggplot(DataOG, aes(parasitism_rate, APG)) +
  geom_point() +
  stat_smooth(method = lm)
print(Plot1 + ggtitle("Effect of parasitism rate on APG across all dates") + labs(y ="APG", x = "Parasitism rate"))

# 2. APG ~ syrphid_fraction
plot(APG ~ syrphid_fraction, data = DataOG, xlab ="Syrphid fraction", main = "Effect of syrphid fraction on APG across all dates")
abline(lm(APG ~ syrphid_fraction, data = DataOG))

# ggplot
Plot2<- ggplot(DataOG, aes(syrphid_fraction, APG)) +
  geom_point() +
  stat_smooth(method = lm)
print(Plot2 + ggtitle("Effect of syrphid fraction on APG across all dates") + labs(y ="APG", x = "Syrphid fraction"))


# 3. Biomass_fin ~ syrphid_fraction
plot(Biomass_fin ~ syrphid_fraction, data = DataOG, xlab ="Syrphid fraction", ylab = "Final Cabbage Biomass (g)", main = "Effect of syrphid fraction on final cabbage biomass")
abline(lm(Biomass_fin ~ syrphid_fraction, data = DataOG))


# ggplot

Plot3<- ggplot(DataOG, aes(syrphid_fraction, Biomass_fin)) +
  geom_point() +
  stat_smooth(method = lm)
print(Plot3 + ggtitle("Effect of syrphid fraction on crop biomass across all dates") + labs(y ="Biomass (g)", x = "Syrphid fraction"))



# combine the scatter plots into 1 graph

attach(DataOG)
par(mfrow=c(3,1))
plot(APG ~ parasitism_rate, data = DataOG, xlab="Parasitism rate", main = "Effect of parasitism rate on APG across all dates")
abline(lm(APG ~ parasitism_rate, data = DataOG))
plot(APG ~ syrphid_fraction, data = DataOG, xlab ="Syrphid fraction", main = "Effect of syrphid fraction on APG across all dates")
abline(lm(APG ~ syrphid_fraction, data = DataOG))
plot(Biomass_fin ~ syrphid_fraction, data = DataOG, xlab ="Syrphid fraction", ylab = "Final Cabbage Biomass (g)", main = "Effect of syrphid fraction on final cabbage biomass")
abline(lm(Biomass_fin ~ syrphid_fraction, data = DataOG))



# Chi square test for correlation 

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
DataOG[is.nan(DataOG)] <- 0


# APG - parasitism rate
chisq.test(DataOG$APG, DataOG$parasitism_rate, simulate.p.value = TRUE)
# p value < 0.01

# APG - syrphid fraction 
chisq.test(DataOG$APG,DataOG$syrphid_fraction, simulate.p.value = TRUE)
# p value < 0.01


# Biomass - syrphid fraction
chisq.test(DataOG$syrphid_fraction, DataOG$Biomass_fin, simulate.p.value = TRUE)
# p value < 0.01

