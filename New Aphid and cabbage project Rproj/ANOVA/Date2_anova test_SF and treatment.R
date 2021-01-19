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



# `Date_2$Treatment`
# diff         lwr         upr     p adj
# HBP-H     0.25114943  0.17220426  0.33009460 0.0000000
# HGD-H     0.12063626  0.04592883  0.19534369 0.0000638
# HP-H      0.37542896  0.29932127  0.45153665 0.0000000
# HPGD-H    0.22804726  0.14910209  0.30699244 0.0000000
# O-H       0.28616261  0.20655505  0.36577016 0.0000000
# HGD-HBP  -0.13051317 -0.20706550 -0.05396083 0.0000184
# HP-HBP    0.12427953  0.04636008  0.20219898 0.0000832
# HPGD-HBP -0.02310216 -0.10379541  0.05759109 0.9645522
# O-HBP     0.03501318 -0.04632822  0.11635458 0.8232988
# HP-HGD    0.25479270  0.18117001  0.32841539 0.0000000
# HPGD-HGD  0.10741100  0.03085867  0.18396334 0.0009180
# O-HGD     0.16552635  0.08829111  0.24276159 0.0000000
# HPGD-HP  -0.14738170 -0.22530115 -0.06946225 0.0000011
# O-HP     -0.08926635 -0.16785683 -0.01067587 0.0153740
# O-HPGD    0.05811534 -0.02322605  0.13945674 0.3210907



# groups
# Date_2_SF$SF_D2 groups
# HP        0.38782287      a
# O         0.29855652      b
# HBP       0.26354334      b
# HPGD      0.24044118      b
# HGD       0.13303017      c
# H         0.01239391      d
