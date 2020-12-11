Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep = ";")

# Aphids parasitized - aphid density

lm1 <- lm(Data1$aphid_live ~ Data1$aphid_parasitized)
summary(lm1)

p <- ggplot(Data1, aes(x=aphid_parasitized,y=(aphid_live), color= aphid_parasitized)) +
  geom_point() + 
  geom_smooth(method="lm")
print(p)

# aphid parasitized - aphid pop growth 

logNaphids_all <- log(Data1$aphid_live + 1) - log(Data1$aphidsinoculated_init + 1) 
APG_all <- logNaphids_all/10
Data1$APG_all <- APG_all

lm2 <- lm(Data1$APG_all ~ Data1$aphid_parasitized)
summary(lm2)

p <- ggplot(Data1, aes(x=aphid_parasitized,y=(APG_all), color= aphid_parasitized)) +
  geom_point() + 
  geom_smooth(method="lm")
print(p)


# aphid density - aphids parasitized 
lm3 <- lm(Data1$aphid_parasitized ~ Data1$aphid_live)
summary(lm3)


