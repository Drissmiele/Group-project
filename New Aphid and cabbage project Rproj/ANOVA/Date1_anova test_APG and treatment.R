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


# 
# $`Date_1$Treatment`
# diff          lwr           upr     p adj
# HBP-H    -0.10713038 -0.134100436 -0.0801603192 0.0000000
# HGD-H    -0.02625237 -0.053222429  0.0007176872 0.0616814
# HP-H     -0.08478103 -0.111845886 -0.0577161730 0.0000000
# HPGD-H   -0.16233466 -0.189304723 -0.1353646067 0.0000000
# O-H      -0.12645646 -0.153426522 -0.0994864050 0.0000000
# HGD-HBP   0.08087801  0.053907948  0.1078480647 0.0000000
# HP-HBP    0.02234935 -0.004715509  0.0494142045 0.1728991
# HPGD-HBP -0.05520429 -0.082174346 -0.0282342292 0.0000001
# O-HBP    -0.01932609 -0.046296144  0.0076439725 0.3180927
# HP-HGD   -0.05852866 -0.085593515 -0.0314638019 0.0000001
# HPGD-HGD -0.13608229 -0.163052352 -0.1091122356 0.0000000
# O-HGD    -0.10020409 -0.127174150 -0.0732340339 0.0000000
# HPGD-HP  -0.07755364 -0.104618492 -0.0504887787 0.0000000
# O-HP     -0.04167543 -0.068740290 -0.0146105769 0.0001686
# O-HPGD    0.03587820  0.008908143  0.0628482600 0.0020922


# $groups
# Date_1$APG_date1 groups
# H          0.08929248      a
# HGD        0.06304011      a
# HP         0.00451145      b
# HBP       -0.01783790     bc
# O         -0.03716398      c
# HPGD      -0.07304219      d