# aphid population growth (APG) ~ Treatment (D1, D2, D3)
#D1
#import data set
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep = ";")
Date_1 <- Data1[Data1$Date == "1",]
logNaphids_all <- log(Date_1$aphid_live + 1) - log(Date_1$aphidsinoculated_init + 1) 
APG_date1 <- logNaphids_all/10
Date_1$APG_date1 <- APG_date1

#D1 anova test
anovaD1_APG <- aov(Date_1$APG_date1 ~ Date_1$Treatment, data = Date_1)
summary(anovaD1_APG)
TukeyHSD(anovaD1_APG)
library("agricolae")
HSD_D1_APG <- HSD.test(anovaD1_APG, "Date_1$Treatment", unbalanced = TRUE, group = T)
HSD_D1_APG
