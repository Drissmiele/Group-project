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


# groups
# Date_2$APG_date2 groups
# H          0.02077945      a
# HGD       -0.01969432      b
# HP        -0.04088266      c
# HPGD      -0.04268995      c
# HBP       -0.04295357      c
# O         -0.05037180      c
