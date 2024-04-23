#' ---
#' title: "Typology"
#' author: "Frédéric Baudron"
#' date: "April 9th, 2024"
#' ---


rm(list=ls())


# INSTALLING & LOADING NECESSARY PACKAGES----------------------------------------------------

if (!require('vegan')) install.packages("vegan")
if (!require('ade4')) install.packages("ade4")
if (!require('dendextend')) install.packages("dendextend")
if (!require('gtsummary')) install.packages("gtsummary")
if (!require('splitstackshape')) install.packages("splitstackshape")


library(vegan)
library(ade4)
library(dendextend)
library(gtsummary)
library(splitstackshape)


# SETTING UP THE DIRECTORY & LOADING THE DATA-----------------------------------

setwd('D:\\Mes Donnees\\2. CIMMYT\\2. Agroeco IDT\\zz. R training\\')

data = read.csv("Input files\\Data for typo.csv")

data = subset(data, subset = district == "Mbire")


# DATA MANIPULATION-------------------------------------------------------------

data$prop_fallow = data$fallow * 100/ (data$cropped_area + data$fallow + 0.001)

data$prop_non_cereals = (data$legumes + data$other) * 100 /
  (data$maize + data$small_grains + data$legumes + data$other+ 0.001)

names(data)[31] = "bee_hives"

names(data)[32] = "fish_pond"

data$fish_pond = as.numeric(data$fish_pond == "Yes")

names(data)[34] = "ind_garden"

names(data)[35] = "com_garden"

names(data)[36] = "own_prod"

names(data)[37] = "inc_source"

data$Community.seed.banks = as.numeric(data$Community.seed.banks == "Yes")
data$Small.grains = as.numeric(data$Small.grains == "Yes")
data$Crop.rotation = as.numeric(data$Crop.rotation == "Yes")
data$Intercropping = as.numeric(data$Intercropping == "Yes")
data$Cover.crops = as.numeric(data$Cover.crops == "Yes")
data$Mulching = as.numeric(data$Mulching == "Yes")
data$Integrated.pest.management = as.numeric(data$Integrated.pest.management == "Yes")
data$Compost.and.or.manure = as.numeric(data$Compost.and.or.manure == "Yes")
data$Tree.planting.on.farm = as.numeric(data$Tree.planting.on.farm == "Yes")
data$Tree.retention..natural.regeneration..on.farm = as.numeric(data$Tree.retention..natural.regeneration..on.farm == "Yes")
data$Homemade.animal.feed = as.numeric(data$Homemade.animal.feed == "Yes")
data$Fodder.production = as.numeric(data$Fodder.production == "Yes")
data$Fodder.preservation = as.numeric(data$Fodder.preservation == "Yes")
data$Survival.feeding = as.numeric(data$Survival.feeding == "Yes")

data$soil_fertility = as.numeric(data$soil_fertility != "Improving")
data$soil_erosion = as.numeric(data$soil_erosion != "Improving")
data$soil_compaction = as.numeric(data$soil_compaction != "Improving")
data$tree_cover = as.numeric(data$tree_cover != "Improving")

data$fertilizers = data$Basal.fertilizer + data$Top.dressing.fertilizer
data$amendments = data$Manure + data$Compost

data$bee_hives = ifelse(data$bee_hives > 0, 1, 0)

data$eq_value = 10000 * data$nb_of_tractors + 2000 * data$nb_of_two_wheel_tractors +
  100 * data$nb_of_ploughs + 150 * data$nb_of_cultivators + 500 * data$nb_of_scotchcarts +
  50 * data$nb_of_wheelbarows + 50 * data$nb_of_solar_panels +
  300 * data$nb_of_motorized_grinding_mill

data = data[, c(1:6, 10, 22:23, 94:95, 82:83, 28:32, 34:39, 41, 43, 45:50, 53:54, 65:68, 74, 87, 90, 97)]


# Removing outliers

boxplot(data$age_hhh)
boxplot(data$family_size)
data = data[data$family_size < 20,]
boxplot(data$family_size)
boxplot(data$cropped_area)
data = data[data$cropped_area < 15,]
boxplot(data$cropped_area)
boxplot(data$fallow)
boxplot(data$prop_non_cereals)
boxplot(data$fertilizers)
boxplot(data$Manure)
boxplot(data$Compost)
boxplot(data$cattle)
boxplot(data$small_rum)
data = data[data$small_rum < 100,]
boxplot(data$small_rum)
boxplot(data$poultry)
data = data[data$poultry < 300,]
boxplot(data$poultry)
boxplot(data$food_security)
boxplot(data$hdds)
boxplot(data$tot_div)
boxplot(data$cereals)
boxplot(data$offtake)
data = data[data$offtake < 6,]
boxplot(data$offtake)
boxplot(data$eq_value)
data = data[data$eq_value < 8000,]
boxplot(data$eq_value)


# Normalization of continuous variables

typo = data

hist(typo$age_hhh)

hist(typo$family_size)
typo$family_size = log10(typo$family_size+(0.5 * min(typo$family_size[typo$family_size > 0])))
hist(typo$family_size)

hist(typo$cropped_area)
typo$cropped_area = log10(typo$cropped_area+(0.5 * min(typo$cropped_area[typo$cropped_area > 0])))
hist(typo$cropped_area)

hist(typo$fallow)
typo$fallow = log10(typo$fallow+(0.5 * min(typo$fallow[typo$fallow > 0])))
hist(typo$fallow)

hist(typo$prop_non_cereals)

hist(typo$fertilizers)
typo$fertilizers = log10(typo$fertilizers+(0.5 * min(typo$fertilizers[typo$fertilizers > 0])))
hist(typo$fertilizers)

hist(typo$Manure)
typo$Manure = log10(typo$Manure+(0.5 * min(typo$Manure[typo$Manure > 0])))
hist(typo$Manure)

hist(typo$Compost)
typo$Compost = log10(typo$Compost+(0.5 * min(typo$Compost[typo$Compost > 0])))
hist(typo$Compost)

hist(typo$cattle)
typo$cattle = log10(typo$cattle+(0.5 * min(typo$cattle[typo$cattle > 0])))
hist(typo$cattle)

hist(typo$small_rum)
typo$small_rum = log10(typo$small_rum+(0.5 * min(typo$small_rum[typo$small_rum > 0])))
hist(typo$small_rum)

hist(typo$poultry)
typo$poultry = log10(typo$poultry+(0.5 * min(typo$poultry[typo$poultry > 0])))
hist(typo$poultry)

hist(typo$food_security)

hist(typo$hdds)

hist(typo$tot_div)

hist(typo$cereals)
typo$cereals = log10(typo$cereals+(0.5 * min(typo$cereals[typo$cereals > 0])))
hist(typo$cereals)

hist(typo$offtake)
typo$offtake = log10(typo$offtake+(0.5 * min(typo$offtake[typo$offtake > 0])))
hist(typo$offtake)

hist(typo$eq_value)
typo$eq_value = log10(typo$eq_value+(0.5 * min(typo$eq_value[typo$eq_value > 0])))
hist(typo$eq_value)


# PRINCIPAL COORDINATE ANALYSIS (PCO) FOLLOWED BY HIERARCHICHAL CLUSTER---------
# ANALYSIS (HCA)

# the scaled Euclidean dissimilarity between continuous STRUCTURAL variables

dEucs = vegdist(typo[,c("age_hhh", "family_size", "cropped_area", "fallow", "prop_non_cereals",
                        "cattle", "small_rum", "poultry", "eq_value")], method="gower") 

# the scaled Euclidean dissimilarity between continuous FUNCTIONAL variables

dEucf = vegdist(typo[,c("fertilizers", "Manure", "Compost", "food_security", "hdds",
                        "tot_div", "cereals", "offtake")], method="gower") 

# dissimilarity between binary STRUCTURAL variables

dBin_ppls = dist.binary(typo[,c("sex_hhh", "edu_hhh", "bee_hives", "fish_pond", "ind_garden", "com_garden")], method=2)

# dissimilarity between binary FUNCTIONAL variables

dBin_pplf = dist.binary(typo[,c("own_prod", "inc_source")], method=2)

# dissimilarity between binary ADOPTION variables

d_practices = dist.binary(typo[,c("Community.seed.banks", "Small.grains", "Crop.rotation",
                                "Intercropping", "Cover.crops", "Mulching", "Integrated.pest.management",
                                "Compost.and.or.manure", "Tree.planting.on.farm", "Tree.retention..natural.regeneration..on.farm",
                                "Homemade.animal.feed", "Fodder.production", "Fodder.preservation", "Survival.feeding")], method=2)


# Combine the 2 dissimilarities of stuctural variables, weighted by number of variables in each dissimilarity

d_structural = (9 * dEucs^2 + 6 * dBin_ppls^2)/15

# Combine the 2 dissimilarities of functional variables, weighted by number of variables in each dissimilarity

d_functional = (8 * dEucf^2 + 2 * dBin_pplf^2)/10

# Combine the 3 categories of dissimilarities

dAll = (d_structural + d_functional + d_practices^2)/3


#Transforming the matrix of dissimilarities to a matrix of distances

distAll = sqrt(2 * dAll)
pco = cmdscale(distAll, eig = TRUE, k = 10) 
cumsum(pco$eig) / sum(pco$eig) 
barplot(pco$eig[1:20])

# choosing 3 dimensions

pco_var = pco$points[, 1:4]

hc_pco = hclust(dist(pco_var), method = "complete")
plot(hc_pco, hang = -1)
grpPCO = cutree(hc_pco, k = 4)


hdend = as.dendrogram(hc_pco)
hdend = color_branches(hdend, k = 4)
hdend = color_labels(hdend, k = 4)
plot(hdend)


plot(pco$points[,1], pco$points[,2], col=grpPCO)
plot(pco$points[,1], pco$points[,3], col=grpPCO)
plot(pco$points[,2], pco$points[,3], col=grpPCO)

data$Type = grpPCO

data$Type = ifelse(data$Type == "3", "Type 1",
                   ifelse(data$Type == "2", "Type 2",
                          ifelse(data$Type == "1", "Type 3", "Type 4")))


# INTERPRETATION OF THE FARM TYPES----------------------------------------------

data[, -c(1:3)] %>%
  tbl_summary(
    by = Type,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p}%"),
    type = list(hdds ~ "continuous"),
    digits = all_continuous() ~ 1,
    label = list(age_hhh ~  "Age of the head of the household",
                 sex_hhh ~  "Female-headed households",
                 edu_hhh ~  "Education of the head of the household higher than primary",
                 family_size ~  "Family size",
                 eq_value ~  "Equipment value (USD)",
                 cropped_area ~  "Total cropped area (ha)",
                 fallow ~ "Fallow land (ha)",
                 prop_non_cereals ~  "Non-cereal crops (% total cropped area)",
                 fertilizers ~  "Total fertilizer used (kg)",
                 Manure ~  "Total manure used (kg)",
                 Compost ~  "Total compost used (kg)",
                 cattle ~  "Cattle (n)",
                 small_rum ~  "Small ruminants (n)",
                 poultry ~  "Poultry (n)",
                 bee_hives ~  "Bee keeping",
                 fish_pond ~  "Owning a fish pond",
                 ind_garden ~  "Owning an individual garden",
                 com_garden ~  "Having access to a communal garden",
                 own_prod ~  "Own production as main source of food",
                 inc_source ~  "Farming as main source of income",
                 food_security ~ "Proportion of the year being food secured",
                 hdds ~  "24H household dietary diversity score (0-12)",
                 Community.seed.banks ~  "Using a community seed bank",
                 Small.grains ~  "Using small grains",
                 Crop.rotation ~  "Using crop rotation",
                 Intercropping ~  "Using intercropping",
                 Cover.crops ~  "Using cover crops",
                 Mulching ~  "Using mulching",
                 Integrated.pest.management ~  "Using integrated pest management",
                 Compost.and.or.manure ~  "Using compost and manure",
                 Tree.planting.on.farm ~  "Planting trees on-farm",
                 Tree.retention..natural.regeneration..on.farm	 ~  "Retaining naturally regenerated trees on-farm",
                 Homemade.animal.feed ~  "Using homemade animal feed",
                 Fodder.production ~  "Produccing fodder",
                 Fodder.preservation ~  "Preserving fodder",
                 Survival.feeding ~  "Using survival feeding",
                 tot_div ~  "Total farm diversity (n species)",
                 cereals ~  "Total cereal production (kg/yr)",
                 offtake ~  "Total livestock offtake (TLU/yr)"))


#SELECTION OF A STRATIFIED SAMPLE-----------------------------------------------

sample1 <- as.data.frame(stratified(data, "Type", 30/nrow(data)))

table(sample1$Type)


sample2 <- as.data.frame(stratified(data, c("ward", "Type"), 30/nrow(data)))

table(sample2$Type)
table(sample2$ward)


# save

write.csv(sample1, "Stratified sample - Types.csv")
write.csv(sample1, "Stratified sample - Wards & Types.csv")

