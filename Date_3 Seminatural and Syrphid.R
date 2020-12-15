Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep =";")
Date_3 <- Data1[Data1$Date == "3", ]
syrphid_fraction <- (Date_3$syrphidl_p /((Date_3$aphid_live)+(Date_3$syrphidl_p)))
Date_3$syrphid_fraction <- syrphid_fraction

Date3buff <- subset(Date_3, BUFF_DIST ==900)


# lm using Date_3
lm1 <- lm(Date_3$syrphid_fraction ~ Date_3$Pt.seminatural)
plot(Date_3$syrphid_fraction ~ Date_3$Pt.seminatural)

# ggplot using Date3buff
library(ggplot2)
ggplot(Date3buff, aes(x= Pt.seminatural, y= syrphid_fraction, 
                      color = Treatment)) +
  geom_jitter()+
  geom_smooth(method="exp")

#barplot using Date_3
library(ggplot2)
G1 <- ggplot(Date_3, aes(x= Treatment, y= syrphid_fraction, fill = Treatment))+
  geom_bar(stat="identity")+
  labs(x="Treatment", y="Syrphid fraction", title="Effect of NE treatment on syrphid fraction at Date 3")
print(G1)

# one inconsistency: second highest number should not be HPGD, but HGD according to the article. 

