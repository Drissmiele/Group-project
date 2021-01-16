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


# Date_2$Treatment`
# diff          lwr          upr     p adj
# HBP-H    -0.0637330159 -0.074897701 -0.052568331 0.0000000
# HGD-H    -0.0404737735 -0.051638458 -0.029309088 0.0000000
# HP-H     -0.0616621122 -0.072826797 -0.050497427 0.0000000
# HPGD-H   -0.0634693950 -0.074634080 -0.052304710 0.0000000
# O-H      -0.0711512493 -0.082315934 -0.059986564 0.0000000
# HGD-HBP   0.0232592424  0.012094557  0.034423927 0.0000001
# HP-HBP    0.0020709037 -0.009093781  0.013235589 0.9950314
# HPGD-HBP  0.0002636209 -0.010901064  0.011428306 0.9999998
# O-HBP    -0.0074182334 -0.018582918  0.003746452 0.4057165
# HP-HGD   -0.0211883387 -0.032353024 -0.010023654 0.0000010
# HPGD-HGD -0.0229956215 -0.034160306 -0.011830937 0.0000001
# O-HGD    -0.0306774759 -0.041842161 -0.019512791 0.0000000
# HPGD-HP  -0.0018072828 -0.012971968  0.009357402 0.9973915
# O-HP     -0.0094891371 -0.020653822  0.001675548 0.1484100
# O-HPGD   -0.0076818543 -0.018846539  0.003482831 0.3647986



# $groups
# Date_3$APG_date3 groups
# H        -0.003506008      a
# HGD      -0.029997118      b
# HP       -0.038132331      c
# HBP      -0.043225928     cd
# O        -0.045342344     de
# HPGD     -0.050370815      e