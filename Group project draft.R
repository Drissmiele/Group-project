

Data <- read.table("Data_for_project.csv", dec = ",", sep = ";")
mean(Data_for_project$aphidsinoculated_init)

#Aphid population growth was modelled using a linear mixed model
install.packages("nlme")
#Parasitism rates (the ratio of parasitized to all aphids) 
#and syrphid fractions (the ratio of syrphids to total aphids + syrphids) 
#were modelled using a binomial response with logit link
install.packages("lme4")
#using sequential AICc testing with the function “dredge” 
install.packages("MuMIn")

