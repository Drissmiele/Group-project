# 5.	We will see whether the rate of parasitism increase with landscape complexity or not.

# Ho: no significant difference in parasitism rate between landscape of high and low complexity
# H1: significant difference in parasitism rate between landscapes if high and low

library(readr)
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep = ";")

# created a new variable called plant_ID that is highly specific (plant number)
Data1$plant_ID <- paste(Data1$Treatment, Data1$Plant, sep = "_")

#selecting in Data 2 all the rows with buff = 100
BUFF <- Data1[Data1$BUFF_DIST == "100",] 
View(BUFF)


#calculate the aphids parasitism rate (n = 1271)
parasitism_rate　<- BUFF$aphid_parasitized/(BUFF$aphid_live + BUFF$aphid_parasitized)
BUFF$parasitism_rate <- parasitism_rate

# calculate the aphids parasitism rate (n = 12710)
parasitism_rate <- Data1$aphid_parasitized/(Data1$aphid_live + Data1$aphid_parasitized)
Data1$parasitism_rate <- parasitism_rate

# Pearson test for correlation between landscape complexity and parasitism rate
cor.test(Data1$parasitism_rate, Data1$Pt.seminatural, method = "pearson")
plot(x = Data1$Pt.seminatural, y = Data1$parasitism_rate)

# result: the correlation coefficient is 0.087. p < 0.05: the correlation is statistically significant. 
# LC and PR are significantly correlated.
library(ggplot2)
p <- ggplot(Data1, aes(x = Pt.seminatural,y = parasitism_rate, color = Treatment)) +
  geom_point() + 
  geom_smooth(method = "lm")
print(p)
# horizontal

library(ggplot2)
p <- ggplot(Data1, aes(x = Pt.seminatural,y = (aphid_parasitized), color = Treatment)) +
  geom_point() + 
  geom_smooth(method = "lm")
print(p)


lm1 <- lm(Data1$aphid_parasitized ~ Data1$Pt.seminatural)
summary(lm1)
lm2 <- lm(Data1$aphid_parasitized ~ Data1$Pt.seminatural + as.numeric(as.factor(Data1$Treatment)) + I(Data1$Pt.seminatural* as.numeric(as.factor(Data1$Treatment))))
summary(lm2)

# there is an interaction between treatment and pt. seminatural having an effect on aphid_parasitized 
# the more stars, the more statistically signficant
