Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep =";")
Date_2 <- Data1[Data1$Date == "2", ]
syrphid_fraction <- (Date_2$syrphidl_p /((Date_2$aphid_live)+(Date_2$syrphidl_p)))
Date_2$syrphid_fraction <- syrphid_fraction

Date2buff <- subset(Date_2, BUFF_DIST ==900)


# lm using Date_2
lm1 <- lm(Date_2$syrphid_fraction ~ Date_2$Pt.seminatural)
plot(Date_2$syrphid_fraction ~ Date_2$Pt.seminatural)

# ggplot using Date2buff
library(ggplot2)
ggplot(Date2buff, aes(x= Pt.seminatural, y= syrphid_fraction, 
                      color = Treatment)) +
  geom_jitter()+
  geom_smooth(method="exp")

#barplot using Date_2
library(ggplot2)
G1 <- ggplot(Date_2, aes(x= Treatment, y= syrphid_fraction, fill = Treatment))+
  geom_bar(stat="identity")+
  labs(x="Treatment", y="Syrphid fraction", title="Effect of NE treatment on syrphid fraction at Date 2")
print(G1)




