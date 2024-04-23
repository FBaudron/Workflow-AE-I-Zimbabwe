#' ---
#' title: "Typology - PCA"
#' author: "Frédéric Baudron"
#' date: "April 9th, 2024"
#' ---


rm(list=ls())


# INSTALLING & LOADING NECESSARY PACKAGES----------------------------------------------------

if (!require('ade4')) install.packages("ade4")
if (!require('ggplot2')) install.packages("ggplot2")
if (!require('fastcluster')) install.packages("fastcluster")
if (!require('factoextra')) install.packages("factoextra")


library(ade4)
library(ggplot2)
library(fastcluster)
library(factoextra)



# SETTING UP THE DIRECTORY & LOADING THE DATA-----------------------------------

setwd('D:\\Mes Donnees\\2. CIMMYT\\2. Agroeco IDT\\zz. R training\\')

data = read.csv("Input files\\Data for typo.csv")

data = subset(data, subset = district == "Mbire")


# DATA MANIPULATION-------------------------------------------------------------

data$prop_fallow = data$fallow * 100/ (data$cropped_area + data$fallow + 0.001)

data$prop_non_cereals = (data$legumes + data$other) * 100 /
  (data$maize + data$small_grains + data$legumes + data$other+ 0.001)

data$fertilizers = data$Basal.fertilizer + data$Top.dressing.fertilizer
data$amendments = data$Manure + data$Compost

data$eq_value = 10000 * data$nb_of_tractors + 2000 * data$nb_of_two_wheel_tractors +
  100 * data$nb_of_ploughs + 150 * data$nb_of_cultivators + 500 * data$nb_of_scotchcarts +
  50 * data$nb_of_wheelbarows + 50 * data$nb_of_solar_panels +
  300 * data$nb_of_motorized_grinding_mill

data = data[, c(1:4, 10, 22:23, 94:95, 82:83, 28:30, 38:39, 74, 87, 90, 97)]


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

names(typo)

#Selection of data for the PCA

typo = typo[, c(4:20)]


#PCA

typo = scale(typo)

delta.pca = dudi.pca(typo)


delta.pca = dudi.pca(df = typo, scannf = FALSE, nf = 5)

delta.pca$eig


#cumulated percentage of variability explained by the PC

cumsum(delta.pca$eig) / sum(delta.pca$eig)


#correlation coefficients between the PCs and the variables

delta.pca$co

fviz_pca_var(delta.pca, col.var="contrib") + 
  scale_color_gradient2(low = "blue", mid = "purple", high = "red", midpoint = 5) +
  theme_minimal()

s.label(delta.pca$li, xax=1, yax=2)
scatter(delta.pca)

#applying the Hierarchical Clustering (HC) on the PCA results


delta.cah = hclust(dist(delta.pca$li), method="ward.D")

hcd = as.dendrogram(delta.cah)

plot(hcd, type = "rectangle", ylab = "Height", leaflab = "none")


# typology 

delta.type = cutree(delta.cah, k=4)


#Visualising and interpreting the clusters in the PCi-PCj

s.class(delta.pca$li, fac = as.factor(delta.type), col = c("black", "darkred", "orangered", "orange"))

data$Type = delta.type

data$Type = as.factor(data$Type)

names(data)

#Plotting differences between types (e.g., age)

theme_set(theme_bw(base_size = 18))

ggplot(data, aes(x = Type, y = age_hhh)) + 
  geom_point(shape = 21, size = 3, position = position_jitter(width = 0.2, height = 0.2), color = "black", fill = "grey")+ 
  geom_boxplot(fill = "white", size = 0.9, width = 0.4, alpha = 0.5, outlier.shape = NA) + 
  theme(axis.text = element_text(size = 10, face = "bold"), axis.title = element_text(size = 10, face  = "bold")) + 
  ylab("Age of the head of the household") + xlab("") +
  scale_x_discrete(labels = c('T1', 'T2', 'T3', 'T4'))


#Distribution of farm types

head(data)
TOTAL = data[,-c(1:20)]
summary(TOTAL)

WD2 = data[ which(data$ward == "Ward 2"), ]
WD3 = data[ which(data$ward == "Ward 3"), ]

names(WD2)
sWD2 = WD2[,-c(1:20)]
summary(sWD2)

names(WD3)
sWD3 = WD3[,-c(1:20)]
summary(sWD3)
