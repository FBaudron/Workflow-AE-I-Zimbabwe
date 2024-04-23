#' ---
#' title: "Data manipulation"
#' author: "Frédéric Baudron"
#' date: "April 9th, 2024"
#' ---


rm(list=ls())


if (!require('readxl')) install.packages("readxl")
if (!require('tidyr')) install.packages("tidyr")
if (!require('dplyr')) install.packages("dplyr")


library(readxl)
library(tidyr)
library(dplyr)


setwd('D:\\Mes Donnees\\2. CIMMYT\\2. Agroeco IDT\\zz. R training\\')


# load the different sheet of the Excel file as separated dataframe

data = read_excel("Input files\\AE-I_Pests_and_Diseases_Assessment_Cereal_-_all_versions_-_labels_-_2023-05-03-10-33-06.xlsx", sheet = 1)

sc1 = read_excel("Input files\\AE-I_Pests_and_Diseases_Assessment_Cereal_-_all_versions_-_labels_-_2023-05-03-10-33-06.xlsx", sheet = 2)
sc2 = read_excel("Input files\\AE-I_Pests_and_Diseases_Assessment_Cereal_-_all_versions_-_labels_-_2023-05-03-10-33-06.xlsx", sheet = 3)
sc3 = read_excel("Input files\\AE-I_Pests_and_Diseases_Assessment_Cereal_-_all_versions_-_labels_-_2023-05-03-10-33-06.xlsx", sheet = 4)


# Create a variable "Period"

data = separate(data = data, col = "_submission_time", into = c("Period", "Time"), sep = " ")

data$Period = as.Date(data$Period, format = "%Y-%m-%d")

data$Period = ifelse(data$Period < "2023-03-15", "First", "Second")

data = separate(data = data, col = "Scan the QR code of the 1st plot", into = c("Farmer", "1st plot"), sep = "-")


data = data[, c(11, 18, 5:7)]

names(data)[2] = "ID"


# Create 3 columns for the 3 plots

data = separate(data = data, col = "Scan the QR code of the 2nd plot", into = c("Farmer", "2nd plot"), sep = "-")

data = data[, c(1:3, 5:6)]

data = separate(data = data, col = "Scan the QR code of the 3rd plot", into = c("Farmer", "3rd plot"), sep = "-")

data = data[, c(1, 5, 2:4, 6)]


# Extracting 6 separate dataframes

data1 = data[, c(1:4)]
data2 = data[, c(1:3, 5)]
data3 = data[, c(1:3, 6)]

sc1 = sc1[, c(11, 1:8)]
sc2 = sc2[, c(11, 1:8)]
sc3 = sc3[, c(11, 1:8)]


# Merging dataframes by plot (requiring the same "ID")

names(sc1)[1] = "ID"
names(sc2)[1] = "ID"
names(sc3)[1] = "ID"

data1 = merge(data1, sc1, by = "ID", all.y = TRUE)
data2 = merge(data2, sc2, by = "ID", all.y = TRUE)
data3 = merge(data3, sc3, by = "ID", all.y = TRUE)


# Binding the 3 dataset (same row names are required for that)

names(data1)[4] = "Plot"
names(data2)[4] = "Plot"
names(data3)[4] = "Plot"

data = rbind(data1, data2, data3)

data = data[, -c(1)]


# Saving

write.csv(data, "Input files\\Pest damage assessment - total.csv", row.names = FALSE)

