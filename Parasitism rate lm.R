#model comparison for Parasitism rate lm

####DataOG

# creation of DataOG with all variables

Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep = ";")
Data1$APG <- NA 
A <- Data1$Date == "1"
Data1[A, "APG"] <- ((log(Data1$aphid_live[A] + 1) - log(Data1$aphidsinoculated_init[A] + 1))/10)
A <- Data1$Date == "2"
Data1[A, "APG"] <- ((log(Data1$aphid_live[A] + 1) - log(Data1$aphidsinoculated_init[A] + 1))/20)
A <- Data1$Date == "3"
Data1[A, "APG"] <- ((log(Data1$aphid_live[A] + 1) - log(Data1$aphidsinoculated_init[A] + 1))/30)
DataOG <- Data1

parasitism_rate <- (Data1$aphid_parasitized/Data1$aphid_live + Data1$aphid_parasitized)
DataOG$parasitism_rate <- parasitism_rate

syrphid_fraction <- (Data1$syrphidl_p / ((Data1$aphid_live) + (Data1$syrphidl_p)))
DataOG$syrphid_fraction <- syrphid_fraction

# conversion of character vectors into numeric vectors !

B <- lapply(DataOG[c(2,3,11,13)], as.factor)
B <- lapply(B, as.numeric)
DataOG[c(2,3,11,13)] <- B


#convert NaN to 0
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

DataOG[is.nan(DataOG)] <- 0


#convert inf to 1
is.infinite.data.frame <- function(y)
  do.call(cbind, lapply(y, is.infinite))

DataOG[is.infinite(DataOG)] <- 1

#linear model Parasitism Rate
#dataOG
lmPR1 <- lm(DataOG$parasitism_rate ~ DataOG$Treatment)
lmPR2 <- lm(DataOG$parasitism_rate ~ DataOG$cropmaturity_init)
lmPR3 <- lm(DataOG$parasitism_rate ~ DataOG$Pt.seminatural)
lmPR4 <- lm(DataOG$parasitism_rate ~ DataOG$Field_Mgmt)
lmPR5 <- lm(DataOG$parasitism_rate ~ DataOG$Treatment + DataOG$cropmaturity_init + DataOG$Field_Mgmt + DataOG$Pt.seminatural)
lmPR6 <- lm(DataOG$parasitism_rate ~ DataOG$cropmaturity_init + DataOG$Treatment + DataOG$Field_Mgmt + DataOG$Pt.seminatural)
lmPR7 <- lm(DataOG$parasitism_rate ~ DataOG$Pt.seminatural + DataOG$Treatment + DataOG$cropmaturity_init + DataOG$Field_Mgmt)
lmPR8 <- lm(DataOG$parasitism_rate ~ DataOG$Field_Mgmt + DataOG$Treatment + DataOG$cropmaturity_init + DataOG$Pt.seminatural)
#lm from the article
lmPR9 <- lm(DataOG$parasitism_rate ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I((DataOG$Field_Mgmt)*(DataOG$Date)) + I((DataOG$Pt.seminatural)*(DataOG$Date)) + I((DataOG$Field_Mgmt)*(DataOG$Treatment)))
lmPR10 <- lm(DataOG$parasitism_rate ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I((DataOG$Field_Mgmt)*(DataOG$Date)) + I((DataOG$Pt.seminatural)*(DataOG$Treatment)))
lmPR11 <- lm(DataOG$parasitism_rate ~ DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I((DataOG$Pt.seminatural)*(DataOG$Date)) + I((DataOG$Pt.seminatural)*(DataOG$Treatment)))
lmPR12 <- lm(DataOG$parasitism_rate ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I((DataOG$Pt.seminatural)*(DataOG$Date)) + I((DataOG$Pt.seminatural)*(DataOG$Treatment)))
lmPR13 <- lm(DataOG$parasitism_rate ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I((DataOG$Field_Mgmt)*(DataOG$Date)) + I((DataOG$Pt.seminatural)*(DataOG$Date)) + I((DataOG$Treatment)*(DataOG$Date) + I((DataOG$Pt.seminatural)*(DataOG$Treatment))))
lmPR14 <- lm(DataOG$parasitism_rate ~ DataOG$Field_Mgmt + DataOG$Pt.seminatural + DataOG$Date + DataOG$Treatment + I((DataOG$Field_Mgmt)*(DataOG$Date)) + I((DataOG$Pt.seminatural)*(DataOG$Date)))

AIC(lmPR1, lmPR2, lmPR3, lmPR4, lmPR5, lmPR6, lmPR7, lmPR8, lmPR9, lmPR10, lmPR11, lmPR12, lmPR13, lmPR14)
#lowest
#lmPR12  8 72278.21

