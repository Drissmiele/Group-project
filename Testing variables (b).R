Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep = ";")

# Aphids parasitized - aphid density

lm2 <- lm(Data1$aphid_live ~ Data1$aphid_parasitized)
summary(lm2)

p <- ggplot(Data1, aes(x=aphid_parasitized,y=(aphid_live), color= aphid_parasitized)) +
  geom_point() + 
  geom_smooth(method="lm")
print(p)


# aphid density - aphids parasitized
lm3 <- lm(Data1$aphid_parasitized ~ Data1$aphid_live)
summary(lm3)
p <- ggplot(Data1,aes(x=aphid_live,y=(aphid_parasitized), color= aphid_live)) +
  geom_point() + 
  geom_smooth(method="lm")
print(p)

# syrphid fraction - aphid

syrphid_fraction <- (Data1$syrphidl_p /((Data1$aphid_live)+(Data1$syrphidl_p)))
Data1$syrphid_fraction <- syrphid_fraction
Data2 <- na.omit(Data1)
# => lots of NaN, dataframe 2 has only half of the obs. 

lm4 <- lm(Data1$aphid_live~Data1$syrphid_fraction)
summary(lm4)
p <- ggplot(Data1,aes(x=syrphid_fraction,y=(aphid_live), color= aphid_live)) +
  geom_point() + 
  geom_smooth(method="lm")
print(p)

#syrphid larvae - aphids

lm4_2 <- lm(Data1$aphid_live ~Data1$syrphidl_p)
summary(lm4_2)
p <- ggplot(Data1,aes(x=syrphidl_p,y=(aphid_live), color= aphid_live)) +
  geom_point() + 
  geom_smooth(method="lm")
print(p)

# the effect of syrphid larvae on aphid is significant (not by chance)
