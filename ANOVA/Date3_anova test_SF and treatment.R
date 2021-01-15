#ANOVA
#PR and treatment
#Parasitism rate ~ Treatment (D1, D2, D3)
#Day3
Data3 <- read.table("Project data.csv", header = TRUE, dec = ",", sep = ";")
Date_3 <- Data1[Data1$Date == "3",]
SF_D3 <- (Date_3$syrphidl_p / ((Date_3$aphid_live) + (Date_3$syrphidl_p)))
Date_3_SF <- data.frame(Date_3, SF_D3)

#D1 anova test
anovaD3_SF <- aov(Date_3_SF$SF_D3 ~ Date_3$Treatment, data = Date_3_SF)
summary(anovaD3_SF)
TukeyHSD(anovaD3_SF)
library("agricolae")
HSD_D3_SF <- HSD.test(anovaD3_SF, "Date_3$Treatment", unbalanced = TRUE, group = T)
HSD_D3_SF