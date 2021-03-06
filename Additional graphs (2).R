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

library(ggplot2)
ggplot(DataOG, aes(x = APG, y = Biomass_fin, color = Treatment)) +
  labs(x = "APG", y = "Cabbage biomass(g)",
       title = "Effects of APG on final cabbage biomass") +
  geom_smooth(method = "lm") 
  

Plot1 <- plot(DataOG$Biomass_fin ~ DataOG$APG)
abline(lm(Biomass_fin ~ APG, data = DataOG))

# no significant effect 

Plot2 <- plot(DataOG$Biomass_fin ~ DataOG$aphid_live)
abline(lm(Biomass_fin ~ aphid_live, data = DataOG))

Plot3 <- plot(DataOG$Biomass_fin ~ DataOG$syrphid_fraction)
abline(lm(Biomass_fin ~ syrphid_fraction, data = DataOG))


Plot4 <- plot(DataOG$Biomass_fin ~ DataOG$s)
abline(lm(Biomass_fin ~ syrphid_fraction, data = DataOG))


       