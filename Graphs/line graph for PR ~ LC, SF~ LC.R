
#line graph for PR ~ LC, SF~ LC
##import data
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

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
DataOG[is.nan(DataOG)] <- 0

#convert inf to 1
is.infinite.data.frame <- function(y)
  do.call(cbind, lapply(y, is.infinite))
DataOG[is.infinite(DataOG)] <- 1


#####parasitism_rate ~ Pt.seminatural
library(ggplot2)
A <- DataOG$BUFF_DIST == "200"
Data200 <-DataOG[A,]   

G1 <- ggplot(Data200, aes(Pt.seminatural, parasitism_rate)) + 
  labs(x = "% seminatural habitat", y = "Parasitism rate", title = " Effect of landscape complexity on PR")+
   geom_point() +
  geom_smooth(method = "lm") 
G1

###syrphid_fraction ~ Pt.seminatural)

B <- DataOG$BUFF_DIST == "900"
Data900 <-DataOG[B,]   

G2 <- ggplot(Data900, aes(Pt.seminatural, syrphid_fraction)) + 
 labs(x = "% seminatural habitat", y = "Syrphid fraction", title = " Effect of landscape complexity on SF")+
  geom_point() +
  geom_smooth(method = "lm")
G2


# install.packages("gridExtra")
library(gridExtra)
grid.arrange(G1, G2, nrow = 1)

