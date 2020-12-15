Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep =";")
Date_1 <- Data1[Data1$Date == "1", ]
syrphid_fraction <- (Date_1$syrphidl_p /((Date_1$aphid_live)+(Date_1$syrphidl_p)))
Date_1$syrphid_fraction <- syrphid_fraction

Date2 <- subset(Date_1, BUFF_DIST ==200 | BUFF_DIST ==900)


boxplot(syrphid_fraction ~ Treatment, data = Date2, ylim =c(0,0.5))


# lm using Date_1
lm1 <- lm(Date_1$syrphid_fraction ~ Date_1$Pt.seminatural)
plot(Date2$syrphid_fraction ~ Date2$Pt.seminatural)

# ggplot (d1)
library(ggplot2)
ggplot(Date2, aes(x= Pt.seminatural, y= syrphid_fraction, 
                   color = Treatment)) +
  geom_smooth() 

