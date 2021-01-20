
# APG, Treatment, semi-natural habitat 

Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep = ";")
Data1$APG <- NA 
A <- Data1$Date == "1"
Data1[A, "APG"] <- ((log(Data1$aphid_live[A] + 1) - log(Data1$aphidsinoculated_init[A] + 1))/10)
A <- Data1$Date == "2"
Data1[A, "APG"] <- ((log(Data1$aphid_live[A] + 1) - log(Data1$aphidsinoculated_init[A] + 1))/20)
A <- Data1$Date == "3"
Data1[A, "APG"] <- ((log(Data1$aphid_live[A] + 1) - log(Data1$aphidsinoculated_init[A] + 1))/30)
DataOG <- Data1


A <- DataOG$BUFF_DIST == "700"
Data700 <-DataOG[A,]                       

# plot APG ~ % of seminatural habitat for each treatment
library(ggplot2)
ggplot(Data700, aes(x = Pt.seminatural, y = APG, color = Treatment, alpha="0")) +
  labs(x = "Landscape complexity (% of seminatural habitat)", y = "APG",
       title = "Effects of landscape complexity on APG in each treatment at 3 different dates") +
  geom_smooth(method = "lm") +
  facet_grid(.~Date) 



