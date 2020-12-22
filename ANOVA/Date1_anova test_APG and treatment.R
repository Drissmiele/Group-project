# aphid population growth (APG) ~ Treatment (D1, D2, D3)

#D1 anova test
anovalm1_4 <- aov(Date_1$APG_date1 ~ Date_1$Treatment, data = Date_1)
summary(anovalm1_4)
TukeyHSD(anovalm1_4)
library("agricolae")
A4 <- HSD.test(anovalm1_4, "Date_1$Treatment", unbalanced = TRUE, group = T)
A4