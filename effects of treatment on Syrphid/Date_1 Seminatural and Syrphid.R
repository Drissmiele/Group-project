Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep =";")
Date_1 <- Data1[Data1$Date == "1", ]
syrphid_fraction <- (Date_1$syrphidl_p /((Date_1$aphid_live)+(Date_1$syrphidl_p)))
Date_1$syrphid_fraction <- syrphid_fraction

Date1buff <- subset(Date_1, BUFF_DIST ==900)


# lm using Date_1
lm1 <- lm(Date1buff$syrphid_fraction ~ Date1buff$Pt.seminatural)
plot(Date1buff$syrphid_fraction ~ Date1buff$Pt.seminatural)

# ggplot using Date1buff
library(ggplot2)
ggplot(Date1buff, aes(x= Pt.seminatural, y= syrphid_fraction, 
                   color = Treatment)) +
  geom_jitter()+
  geom_smooth(method="exp")

#barplot using Date_1
G1 <- ggplot(Date_1, aes(x= Treatment, y= syrphid_fraction, fill = Treatment))+
  geom_bar(stat="identity")+
  labs(x="Treatment", y="Syrphid fraction", title="Effect of NE treatment on syrphid fraction at Date 1")
print(G1)




