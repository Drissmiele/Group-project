Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep = ";")
Data1$APG <- NA 
A <- Data1$Date == "1"
Data1[A, "APG"] <- ((log(Data1$aphid_live[A] + 1) - log(Data1$aphidsinoculated_init[A] + 1))/10)
A <- Data1$Date == "2"
Data1[A, "APG"] <- ((log(Data1$aphid_live[A] + 1) - log(Data1$aphidsinoculated_init[A] + 1))/20)
A <- Data1$Date == "3"
Data1[A, "APG"] <- ((log(Data1$aphid_live[A] + 1) - log(Data1$aphidsinoculated_init[A] + 1))/30)
DataOG <- Data1

parasitism_rate <- (Data1$aphid_parasitized/Data1$aphid_live + Data1$aphid_parasitized)
DataOG$parasitism_rate <- parasitism_rate

syrphid_fraction <- (Data1$syrphidl_p / ((Data1$aphid_live) + (Data1$syrphidl_p)))
DataOG$syrphid_fraction <- syrphid_fraction

# new data frame with BUFF = 900

A <- DataOG$BUFF_DIST == "900"
Data900 <-DataOG[A,]   


# plot

library(ggplot2)
ggplot(Data900, aes(x = Treatment, y = syrphid_fraction , fill = Treatment)) +
  geom_boxplot() +
  labs(x = "Enemy exclution treatment", y = "Syrphid fraction",
       title = "Effects of Treatment on syrphid fraction at different dates") +
  geom_hline(yintercept = 0, colour = "grey") +
  facet_grid(.~Date)