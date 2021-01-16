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
anovaD3_SF <- aov(Date_3_SF$SF_D3 ~ Date_3$Treatment, data = Date_3_SF)

summary(anovaD3_SF)
TukeyHSD(anovaD3_SF)
library("agricolae")
HSD_D3_SF <- HSD.test(anovaD3_SF, "Date_3$Treatment", unbalanced = TRUE, group = T)
HSD_D3_SF


# $`Date_3$Treatment`
# diff          lwr         upr     p adj
# HBP-H     0.36678920  0.275731249  0.45784715 0.0000000
# HGD-H     0.15208746  0.067269986  0.23690493 0.0000052
# HP-H      0.33604668  0.247065439  0.42502792 0.0000000
# HPGD-H    0.25151142  0.117300101  0.38572274 0.0000015
# O-H       0.39187403  0.294044346  0.48970371 0.0000000
# HGD-HBP  -0.21470174 -0.307564982 -0.12183850 0.0000000
# HP-HBP   -0.03074252 -0.127423640  0.06593860 0.9446975
# HPGD-HBP -0.11527778 -0.254713270  0.02415771 0.1715319
# O-HBP     0.02508483 -0.079797126  0.12996678 0.9838953
# HP-HGD    0.18395922  0.093131405  0.27478703 0.0000001
# HPGD-HGD  0.09942396 -0.036018681  0.23486661 0.2905168
# O-HGD     0.23978657  0.140274371  0.33929876 0.0000000
# HPGD-HP  -0.08453526 -0.222623514  0.05355300 0.5008067
# O-HP      0.05582735 -0.047256766  0.15891146 0.6347610
# O-HPGD    0.14036260 -0.003586527  0.28431174 0.0608492
# 
# 
# groups
# Date_3_SF$SF_D3 groups
# O         0.43202927      a
# HBP       0.40694444      a
# HP        0.37620192     ab
# HPGD      0.29166667     bc
# HGD       0.19224270      c
# H         0.04015524      d