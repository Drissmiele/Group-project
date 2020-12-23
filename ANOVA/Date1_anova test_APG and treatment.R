# aphid population growth (APG) ~ Treatment (D1, D2, D3)
#import data set
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep = ";")
Date_1 <- Data1[Data1$Date == "1",]
Date_1 <- Data1[Data1$Date == "1",]
logNaphids_all <- log(Date_1$aphid_live + 1) - log(Date_1$aphidsinoculated_init + 1) 
APG_date1 <- logNaphids_all/10
Date_1$APG_date1 <- APG_date1

#D1 anova test
anovalm1_4 <- aov(Date_1$APG_date1 ~ Date_1$Treatment, data = Date_1)
summary(anovalm1_4)
TukeyHSD(anovalm1_4)
library("agricolae")
A4 <- HSD.test(anovalm1_4, "Date_1$Treatment", unbalanced = TRUE, group = T)
A4
