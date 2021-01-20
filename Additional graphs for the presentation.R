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


# APG ~ parasitism_rate
plot(APG ~ parasitism_rate, data = DataOG, xlab="Parasitism rate", abline(lm(APG ~ parasitism_rate)))

# APG ~ syrphid_fraction
plot(APG ~ syrphid_fraction, data = DataOG, xlab ="Syrphid fraction", abline(lm(APG ~ syrphid_fraction)))

# Biomass_fin ~ syrphid_fraction
plot(Biomass_fin ~ syrphid_fraction, data = DataOG, xlab ="Syrphid fraction", abline(lm(Biomass_fin ~ syrphid_fraction)))

attach(DataOG)
par(mfrow=c(3,1))
plot(APG ~ parasitism_rate, data = DataOG, xlab="Parasitism rate", abline(lm(APG ~ parasitism_rate)))
plot(APG ~ syrphid_fraction, data = DataOG, xlab ="Syrphid fraction",abline(lm(APG ~ syrphid_fraction)))
plot(Biomass_fin ~ syrphid_fraction, data = DataOG, xlab ="Syrphid fraction", abline(lm(Biomass_fin ~ syrphid_fraction)))