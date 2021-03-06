
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

parasitism_rate <- (Data1$aphid_parasitized/(Data1$aphid_live + Data1$aphid_parasitized))
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


#linear model with biomass	

lmM1 <- lm(DataOG$Biomass_fin ~ DataOG$aphid_live)
lmM2 <- lm(DataOG$Biomass_fin ~ DataOG$APG)
lmM3 <- lm(DataOG$Biomass_fin ~ DataOG$syrphidl_p)
lmM4 <- lm(DataOG$Biomass_fin ~ DataOG$syrphid_fraction)
lmM5 <- lm(DataOG$Biomass_fin ~ DataOG$aphid_parasitized)
lmM6 <- lm(DataOG$Biomass_fin ~ DataOG$parasitism_rate)


AIC(lmM1, lmM2, lmM3, lmM4, lmM5, lmM6)
# df      AIC
# lmM1  3 163970.1  #rank 6
# lmM2  3 163957.9  #rank 4
# lmM3  3 163579.3  #rank 1
# lmM4  3 163790.5  #rank 2
# lmM5  3 163954.1  #rank 3
# lmM6  3 163969.3  #rank 5
