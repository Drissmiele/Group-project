#ANOVA
#PR and treatment
#Parasitism rate ~ Treatment (D1, D2, D3)
#Day1
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep = ";")
Date_1 <- Data1[Data1$Date == "1",]
PR_D1 <- (Date_1$aphid_parasitized/(Date_1$aphid_live + Date_1$aphid_parasitized))

Date_1_PR <- data.frame(Date_1, PR_D1)

#D1 anova test
anovaD1_PR <- aov(Date_1_PR$PR_D1 ~ Date_1$Treatment, data = Date_1_PR)
summary(anovaD1_PR)
TukeyHSD(anovaD1_PR)
library("agricolae")
HSD_D1_PR <- HSD.test(anovaD1_PR, "Date_1$Treatment", unbalanced = TRUE, group = T)
HSD_D1_PR

# $`Date_1$Treatment`
# diff         lwr         upr     p adj
# HBP-H     0.1724195184  0.11033550  0.23450353 0.0000000
# HGD-H     0.0101242978 -0.05090558  0.07115418 0.9970622
# HP-H      0.1637869656  0.10006727  0.22750667 0.0000000
# HPGD-H    0.2650896620  0.20091187  0.32926745 0.0000000
# O-H       0.1628068057  0.10241201  0.22320160 0.0000000
# HGD-HBP  -0.1622952205 -0.22293400 -0.10165645 0.0000000
# HP-HBP   -0.0086325527 -0.07197776  0.05471265 0.9988561
# HPGD-HBP  0.0926701436  0.02886416  0.15647613 0.0005086
# O-HBP    -0.0096127127 -0.06961226  0.05038684 0.9975112
# HP-HGD    0.1536626678  0.09135026  0.21597508 0.0000000
# HPGD-HGD  0.2549653642  0.19218460  0.31774613 0.0000000
# O-HGD     0.1526825079  0.09377438  0.21159064 0.0000000
# HPGD-HP   0.1013026964  0.03590408  0.16670131 0.0001513
# O-HP     -0.0009801599 -0.06267069  0.06071037 1.0000000
# O-HPGD   -0.1022828563 -0.16444643 -0.04011928 0.0000419


# Date_1_PR$PR_D1 groups
# HPGD      0.30918893      a
# HBP       0.21651878      b
# HP        0.20788623      b
# O         0.20690607      b
# HGD       0.05422356      c
# H         0.04409926      c