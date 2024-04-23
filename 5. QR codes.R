#' ---
#' title: "Typology"
#' author: "Frédéric Baudron"
#' date: "April 9th, 2024"
#' ---


rm(list=ls())


if (!require('baRcodeR')) install.packages("baRcodeR")


library(baRcodeR)


setwd('D:\\Mes Donnees\\2. CIMMYT\\2. Agroeco IDT\\zz. R training\\')


farm = read.csv("Input files\\Hosting farmers.csv")


# FARM QR CODES-----------------------------------------------------------------

# names(farm)

create_PDF(Labels = farm$Name, numcol = 2, numrow = 4, Fsz = 8, name = 'farms_qrcodes.pdf')


# PLOT QR CODES-----------------------------------------------------------------

l <- expand.grid(FR = farm$Name, TR = c("CONV", "CA", "PPULL"))
l <- paste(l$FR, l$TR, sep = "-")

create_PDF(Labels = l, numcol = 2, numrow = 4, Fsz = 8, name = 'plots_qrcodes.pdf')




