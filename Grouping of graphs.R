library(readr)
Data0 <- Data1 <- read.table("Project data.csv", header = TRUE, dec = ",", sep = ";")

cowplot:: # Other function to put several figures together
  
# APG
  
Data1$APG <- NA 
A <- Data1$Date == "1"
Data1[A, "APG"] <- ((log(Data1$aphid_live[A] + 1) - log(Data1$aphidsinoculated_init[A] + 1))/10)
A <- Data1$Date == "2"
Data1[A, "APG"] <- ((log(Data1$aphid_live[A] + 1) - log(Data1$aphidsinoculated_init[A] + 1))/20)
A <- Data1$Date == "3"
Data1[A, "APG"] <- ((log(Data1$aphid_live[A] + 1) - log(Data1$aphidsinoculated_init[A] + 1))/30)
Data1$APG

# new data frame with BUFF = 700
A <- Data1$BUFF_DIST == "700"
Data700 <-Data1[A,]  

#plot
library(ggplot2)
ggplot(Data700, aes(x = Treatment, y = APG, fill = Treatment)) +
  geom_boxplot() +
  labs(x = "Enemy exclution treatment", y = "Aphids population growth",
       title = "Effects of Treatment on APG at Different dates") +
  geom_hline(yintercept = 0, colour = "grey") +
  facet_grid(.~Date) # same date but one factor changes (e.g. date)



# parasitism rate 

Data1$parasitism_rate <- NA
A <-  Data1$BUFF_DIST == "200"
Data1[A, "parasitism_rate"] <- (Data1$aphid_parasitized[A]/(Data1$aphid_live[A] + Data1$aphid_parasitized[A]))
Data1$parasitism_rate


# plot for BUFF DIST = 200 

library(ggplot2)
ggplot(Data1, aes(x = Treatment, y = parasitism_rate, fill = Treatment)) +
  geom_boxplot() +
  labs(x = "Enemy exclution treatment", y = "Parasitism rate",
       title = "Effects of Treatment on Parasitism rate at different dates") +
  geom_hline(yintercept = 0, colour = "grey") +
  facet_grid(.~Date) 




# syrphid fraction 

# New data frame with BUFF DIST = 900

A <- Data1$BUFF_DIST == "900"
Data900 <-Data1[A,]                       # new data frame with BUFF_DIST = 900
Data900$syrphid_fraction <- (Data900$syrphidl_p / ((Data900$aphid_live) + (Data900$syrphidl_p)))

#Data900$syrphid_fraction[is.na(Data900$syrphid_fraction)] <- 0 #(used to assign 0 to NaN)

# Removing all NaN
Data900 <- na.omit(Data900)


#plot for BUFF DIST = 900

ggplot(Data900, aes(x = Treatment, y = syrphid_fraction , fill = Treatment)) +
  geom_boxplot() +
  labs(x = "Enemy exclution treatment", y = "Syrphid fraction",
       title = "Effects of Treatment on syrphid fraction at different dates") +
  geom_hline(yintercept = 0, colour = "grey") +
  facet_grid(.~Date)




# Plot syrphid fraction ~ Treatment for all buffer distance 

Data1 <- Data0 # Data0 defined as the raw data imported
Data1$syrphid_fraction <- (Data1$syrphidl_p / ((Data1$aphid_live) + (Data1$syrphidl_p)))

# plot including all buffer distances 
ggplot(Data1, aes(x = Treatment, y = syrphid_fraction , fill = Treatment)) +
  geom_boxplot() +
  labs(x = "Enemy exclution treatment", y = "Syrphid fraction",
       title = "Effects of Treatment on syrphid fraction at different dates") +
  geom_hline(yintercept = 0, colour = "grey") +
  facet_grid(BUFF_DIST~Date) 

# no influence of buffer distance on syrphid fraction


