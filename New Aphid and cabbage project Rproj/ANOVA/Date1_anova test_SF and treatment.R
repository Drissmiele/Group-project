#ANOVA
#PR and treatment
#Parasitism rate ~ Treatment (D1, D2, D3)
#Day1
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep = ";")
Date_1 <- Data1[Data1$Date == "1",]
SF_D1 <- (Date_1$syrphidl_p / ((Date_1$aphid_live) + (Date_1$syrphidl_p)))
Date_1_SF <- data.frame(Date_1, SF_D1)

#D1 anova test
anovaD1_SF <- aov(Date_1_SF$SF_D1 ~ Date_1$Treatment, data = Date_1_SF)
anovaD1_SF_FM <- aov(Date_1_SF$SF_D1 ~ Date_1$Treatment + Date_1$fieldma, data = Date_1_SF)


summary(anovaD1_SF)
TukeyHSD(anovaD1_SF)
library("agricolae")
HSD_D1_SF <- HSD.test(anovaD1_SF, "Date_1$Treatment", unbalanced = TRUE, group = T)
HSD_D1_SF


#graph


# Date_1$Treatment`
# diff         lwr        upr     p adj
# HBP-H     0.033691279 -0.02372580 0.09110835 0.5495413
# HGD-H     0.030505660 -0.02380546 0.08481678 0.5973337
# HP-H      0.115742549  0.05912631 0.17235879 0.0000001
# HPGD-H    0.119460676  0.06018496 0.17873639 0.0000002
# O-H       0.193313646  0.13980718 0.24682011 0.0000000
# HGD-HBP  -0.003185619 -0.05907130 0.05270006 0.9999843
# HP-HBP    0.082051271  0.02392288 0.14017966 0.0008293
# HPGD-HBP  0.085769397  0.02504772 0.14649107 0.0008201
# O-HBP     0.159622367  0.10451835 0.21472639 0.0000000
# HP-HGD    0.085236889  0.03017432 0.14029946 0.0001534
# HPGD-HGD  0.088955016  0.03116143 0.14674860 0.0001719
# O-HGD     0.162807986  0.11094827 0.21466770 0.0000000
# HPGD-HP   0.003718127 -0.05624686 0.06368311 0.9999762
# O-HP      0.077571096  0.02330203 0.13184016 0.0006711
# O-HPGD    0.073852970  0.01681489 0.13089105 0.0031002

# groups
# Date_1_SF$SF_D1 groups
# O         0.23005277      a
# HPGD      0.15619980      b
# HP        0.15248167      b
# HBP       0.07043040      c
# HGD       0.06724479      c
# H         0.03673913      c


l