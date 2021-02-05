# Length of the longest stem

# creation of Data1 with all variables
# Data1 <- read.table("~/GitHub/Greenhouse/Length of the longest stem.csv", header = TRUE, dec = ",", sep = ";")
library(readr)
longest_stem <- read_csv("~/GitHub/Greenhouse/Length of the longest stem.csv")
average_length <- ave(longest_stem$`sample 1`, longest_stem$`sample 2`, longest_stem$`sample 3`, longest_stem$`sample 4`, longest_stem$`sample 5`, longest_stem$`sample 6`, FUN = mean)
longest_stem$average_length <- average_length


#plot
library(ggplot2)
ggplot(longest_stem, aes(x = Day, y = longest_stem$average_length, color = Treatment)) +
  labs(x = "Day", y = "Length of the longest stem",
       title = "Effects of Treatment on the length of the longest stem") +
  geom_smooth(method = "lm") 
facet_grid(.~Date) 

library(ggplot2)
ggplot(Data700, aes(x = Pt.seminatural, y = APG, color = Treatment, alpha="0")) +
  labs(x = "Landscape complexity (% of seminatural habitat)", y = "APG",
       title = "Effects of landscape complexity on APG in each treatment at 3 different dates") +
  geom_smooth(method = "lm") +
  facet_grid(.~Date) 
