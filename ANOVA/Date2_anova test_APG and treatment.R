# aphid population growth (APG) ~ Treatment (D1, D2, D3)
#D2
#import data set
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep = ";")
Date_2 <- Data1[Data1$Date == "2",]
logNaphids_all <- log(Date_2$aphid_live + 1) - log(Date_2$aphidsinoculated_init + 1) 
APG_date2 <- logNaphids_all/20
Date_2$APG_date2 <- APG_date2

#D1 anova test
anovaD2_APG <- aov(Date_2$APG_date2 ~ Date_2$Treatment, data = Date_2)
summary(anovaD2_APG)
TukeyHSD(anovaD2_APG)
library("agricolae")
HSD_D2_APG <- HSD.test(anovaD2_APG, "Date_2$Treatment", unbalanced = TRUE, group = T)
HSD_D2_APG
