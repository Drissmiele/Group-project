#ANOVA
#PR and treatment
#Parasitism rate ~ Treatment (D1, D2, D3)
#Day2
Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep = ";")
Date_2 <- Data1[Data1$Date == "2",]
PR_D2 <- (Date_2$aphid_parasitized/(Date_2$aphid_live + Date_2$aphid_parasitized))

Date_2_PR <- data.frame(Date_2, PR_D2)

#D1 anova test
anovaD2_PR <- aov(Date_2_PR$PR_D2 ~ Date_2$Treatment, data = Date_2_PR)
summary(anovaD2_PR)
TukeyHSD(anovaD2_PR)
library("agricolae")
HSD_D2_PR <- HSD.test(anovaD2_PR, "Date_2$Treatment", unbalanced = TRUE, group = T)
HSD_D2_PR

# Date_2$Treatment`
# diff         lwr         upr     p adj
# HBP-H     0.32080217  0.24454616  0.39705819 0.0000000
# HGD-H     0.05418821 -0.01774275  0.12611918 0.2626894
# HP-H      0.49660199  0.42510391  0.56810007 0.0000000
# HPGD-H    0.14712715  0.06870684  0.22554746 0.0000014
# O-H       0.28144786  0.20583476  0.35706095 0.0000000
# HGD-HBP  -0.26661396 -0.34150066 -0.19172726 0.0000000
# HP-HBP    0.17579982  0.10132883  0.25027081 0.0000000
# HPGD-HBP -0.17367502 -0.25481501 -0.09253503 0.0000000
# O-HBP    -0.03935432 -0.11778450  0.03907587 0.7079074
# HP-HGD    0.44241378  0.37237798  0.51244957 0.0000000
# HPGD-HGD  0.09293894  0.01584950  0.17002838 0.0078378
# O-HGD     0.22725965  0.15302773  0.30149157 0.0000000
# HPGD-HP  -0.34947484 -0.42616052 -0.27278916 0.0000000
# O-HP     -0.21515413 -0.28896666 -0.14134161 0.0000000
# O-HPGD    0.13432071  0.05378464  0.21485678 0.0000309


# $groups
# Date_2_PR$PR_D2 groups
# HP        0.53401987      a
# HBP       0.35822006      b
# O         0.31886574      b
# HPGD      0.18454503      c
# HGD       0.09160610      d
# H         0.03741788      d
