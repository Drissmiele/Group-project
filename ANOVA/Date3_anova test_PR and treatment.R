#ANOVA
#PR and treatment
#Parasitism rate ~ Treatment (D1, D2, D3)
#Day3
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep = ";")
Date_3 <- Data1[Data1$Date == "3",]
PR_D3 <- (Date_3$aphid_parasitized/(Date_3$aphid_live + Date_3$aphid_parasitized))

Date_3_PR <- data.frame(Date_3, PR_D3)

#D1 anova test
anovaD3_PR <- aov(Date_3_PR$PR_D3 ~ Date_3$Treatment, data = Date_3_PR)
summary(anovaD3_PR)
TukeyHSD(anovaD3_PR)
library("agricolae")
HSD_D3_PR <- HSD.test(anovaD3_PR, "Date_3$Treatment", unbalanced = TRUE, group = T)
HSD_D3_PR


# Date_3$Treatment`
# diff          lwr         upr     p adj
# HBP-H     0.41229128  0.323874784  0.50070778 0.0000000
# HGD-H     0.06178108 -0.023862367  0.14742452 0.3099768
# HP-H      0.49762461  0.416814108  0.57843512 0.0000000
# HPGD-H    0.56202022  0.452585643  0.67145479 0.0000000
# O-H       0.36387461  0.262132135  0.46561709 0.0000000
# HGD-HBP  -0.35051020 -0.442853563 -0.25816684 0.0000000
# HP-HBP    0.08533333 -0.002546325  0.17321299 0.0627933
# HPGD-HBP  0.14972894  0.034975190  0.26448269 0.0027980
# O-HBP    -0.04841667 -0.155859807  0.05902647 0.7929225
# HP-HGD    0.43584354  0.350754422  0.52093265 0.0000000
# HPGD-HGD  0.50023914  0.387608125  0.61287015 0.0000000
# O-HGD     0.30209354  0.196920577  0.40726649 0.0000000
# HPGD-HP   0.06439560 -0.044605696  0.17339691 0.5414694
# O-HP     -0.13375000 -0.235026302 -0.03247370 0.0023532
# O-HPGD   -0.19814560 -0.323455132 -0.07283608 0.0001010

# Date_3_PR$PR_D3 groups
# HPGD      0.60439560      a
# HP        0.54000000     ab
# HBP       0.45466667     bc
# O         0.40625000      c
# HGD       0.10415646      d
# H         0.04237539      d