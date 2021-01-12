install.packages(c("poppr", "mmod", "magrittr", "treemap"), repos = "http://cran.rstudio.com", dependencies = TRUE)
library("poppr")
getwd()

# Data preparation
monpop <- read.genalex("C:/Users/ACER/Documents/M1 Valencia/Data acquisition/Group-project/monpop.csv")
(monpop)
splitStrata(monpop) <- ~Tree/Year/Symptom

# Genotype accumulation curve
data("Pinf")
gac <- genotype_curve(Pinf, sample = 1000, quiet = TRUE)

data("microbov")
microbov
gac <- genotype_curve(microbov, sample = 1000, quiet = TRUE)
# with only 7 loci, all the genetic diversity was captured. 

data("nancycats")
nancycats
gac <- genotype_curve(nancycats, sample = 1000, quiet = TRUE)
# this figure is used to show how well we are able to identify all existing genotypes with a specific set of molecular markers.
# If the 100% plateau is not reached, it means that we have missing some MLG. 

# Allele frequency calculation (genotypic diversity)

poppr(Pinf)
# NA has a higher sample size --> must be corrected. Rarefaction analysis used to correct data when there are different sample sizes. 
# Allows to check things as if the sample size was the same.

P.tab <- mlg.table(Pinf)
# shows # of individuals belonging to each genotype: indicator of evenness 


poppr(nancycats)
P.tab <- mlg.table(nancycats)
# in this data set, every ind is different. There is one MLG per ind. 

data(Pram)
poppr(Pram)
P.tab <- mlg.table(Pram)

# data split and clone correction
monpop
splitStrata(monpop) <- ~Tree/Year/Symptom
mcc_TY <- clonecorrect(monpop, strata = ~Tree/Year, keep = 1:2)
mcc_TY
# clone correction removes clones: Will allelic diversity increase or decrease with clone-censored data? correct influence of overrepresentation of some alleles. isolate isolates that have a specific variance. 
setPop(monpop) <- ~Tree/Year

# locus_table function is used to compare the diversity of alleles at each locus using Simpson’s index.
# Below we are doing: (1) Calculating diversity of the clone corrected data, (2) Calculating diversity of the uncorrected data and (3) taking the difference of step 1 from step 2.
cc <- locus_table(mcc_TY, info = FALSE)
cc
mp <- locus_table(monpop, info = FALSE)
mp
# the allele section shows the # of different alleles identified at that marker.
mp - cc
# frequencies are changed in corrected version because the # of isolates is reduced. The difference (mp-cc) is negative.

# Genotypic richness, diversity, and evenness

# according to the expected MLG (eMLG), BB is more diverse than fruit rot (FR). Hypothesis is confirmed. 

# rarefaction analysis

library("poppr")
data(monpop)
splitStrata(monpop) <- ~Tree/Year/Symptom
setPop(monpop) <- ~Symptom
monpop

library("vegan")
mon.tab <- mlg.table(monpop, plot = FALSE)
min_sample <- min(rowSums(mon.tab))
rarecurve(mon.tab, sample = min_sample, xlab = "Sample Size", ylab = "Expected MLGs")
title("Rarefaction of Fruit Rot and Blossom Blight")
poppr(monpop)

# the vertical line is drawn at N = 113, the limiting sample size of the BB population. 



# Linkage disequilibrium (related to reproduction)
# This test is useful to determine if populations are clonal (where significant disequilibrium is expected due to linkage among loci) or sexual (where linkage among loci is not expected). 
# The null hypothesis tested is that alleles observed at different loci are not linked if populations are sexual.

# P > 0,01 -> Ho --> sexual reproduction
# P < 0.01 --> Reject Ho --> asexual reproduction

library("poppr")
library("magrittr")
data(Pinf)
MEXICO <- popsub(Pinf, "North America")
ia(MEXICO, sample = 999)
# p > 0.01 => sexual reproduction

# clone correction
MX %>% clonecorrect(strata= ~Continent/Country) %>% ia(sample = 999)
# we failed to reject Ho => sexual reproduction. 

SOUTHAMERICA <- popsub(Pinf, "South America")
ia(SOUTHAMERICA, sample = 999)
# p < 0.001 => asexual reproduction 

# Genetic distance and clustering 

library("poppr")
library("ape") # To visualize the tree using the "nj" function
library("magrittr")

data(microbov)
set.seed(10) # random number generator
ten_samples <- sample(nInd(microbov), 10)
mic10       <- microbov[ten_samples]
(micdist    <- provesti.dist(mic10))

#The above represents the pairwise distances between these 10 samples. 
# We will use this distance matrix to create a neighbor-joining tree

theTree <- micdist %>%
nj() %>%    # calculate neighbor-joining tree
  ladderize() # organize branches by clade
plot(theTree)
add.scale.bar(length = 0.05) # add a scale bar showing 5% difference.
# not a phylogenetic tree. 

set.seed(999)
aboot(mic10, dist = provesti.dist, sample = 200, tree = "nj", cutoff = 50, quiet = TRUE)




