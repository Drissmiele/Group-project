# aphid population growth (APG) ~ Treatment (D1, D2, D3)
#D3
#import data set
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep = ";")
Date_3 <- Data1[Data1$Date == "3",]
logNaphids_all <- log(Date_3$aphid_live + 1) - log(Date_3$aphidsinoculated_init + 1) 
APG_date3 <- logNaphids_all/30
Date_3$APG_date3 <- APG_date3

#D1 anova test
anovaD3_APG <- aov(Date_3$APG_date3 ~ Date_3$Treatment, data = Date_3)
summary(anovaD3_APG)
TukeyHSD(anovaD3_APG)
library("agricolae")
HSD_D3_APG <- HSD.test(anovaD3_APG, "Date_3$Treatment", unbalanced = TRUE, group = T)
HSD_D3_APG
