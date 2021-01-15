#ANOVA
#PR and treatment
#Parasitism rate ~ Treatment (D1, D2, D3)
#Day2
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep = ";")
Date_2 <- Data1[Data1$Date == "2",]
SF_D2 <- (Date_2$syrphidl_p / ((Date_2$aphid_live) + (Date_2$syrphidl_p)))
Date_2_SF <- data.frame(Date_2, SF_D2)

#D1 anova test
anovaD2_SF <- aov(Date_2_SF$SF_D2 ~ Date_2$Treatment, data = Date_2_SF)
summary(anovaD2_SF)
TukeyHSD(anovaD2_SF)
library("agricolae")
HSD_D2_SF <- HSD.test(anovaD2_SF, "Date_2$Treatment", unbalanced = TRUE, group = T)
HSD_D2_SF