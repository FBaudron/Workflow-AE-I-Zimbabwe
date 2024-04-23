#' ---
#' title: "Graphs & tables"
#' author: "Frédéric Baudron"
#' date: "April 9th, 2024"
#' ---


rm(list=ls())


if (!require('lme4')) install.packages("lme4")
if (!require('lmerTest')) install.packages("lmerTest")
if (!require('broom')) install.packages("broom")
if (!require('dotwhisker')) install.packages("dotwhisker")
if (!require('ggplot2')) install.packages("ggplot2")
if (!require('ggthemes')) install.packages("ggthemes")
if (!require('ggeffects')) install.packages("ggeffects")


library(lme4)
library(lmerTest)
library(broom)
library(dotwhisker)
library(ggplot2)
library(ggthemes)
library(ggeffects)
library(dplyr)


setwd('D:\\Mes Donnees\\2. CIMMYT\\2. Agroeco IDT\\zz. R training\\')

data = read.csv("Input files\\Pest damage assessment - total.csv")
cor = read.csv("Input files\\Correspondance demos baseline.csv")

names(cor)[5] = "Farmer"

data = merge(data, cor, by = "Farmer")


# Make "CONV" the reference

data$Plot = ifelse(data$Plot == "CONV", "0.CONV", data$Plot)


# Dataframe with means per plot

datasum = data[,c(13, 1:11)] %>%            
  group_by(District, Period, Farmer, Plot) %>%
  summarise_each(funs(mean))


# Generalized linear model

GLM_LFD_FAW_mean = glm(Number.of.plants.displaying.leaf.damage.by.fall.armyworm/10
                      ~ District + Farmer + Period + Vstage + Plot,
                      data = datasum)

summary(GLM_LFD_FAW_mean)
plot(GLM_LFD_FAW_mean)


# Dotwhisker plots

tGLM_LFD_FAW_mean = tidy(GLM_LFD_FAW_mean)

dwplot(tGLM_LFD_FAW_mean, dot_args = list(size = 3, pch = 21, fill = "#BB3754FF", 
                                  stroke = 1, color = "#BB3754FF"),
             whisker_args = list(size = 1, color = "#BB3754FF")) +
  theme_few() +
  xlab("") + ylab("") + 
  ggtitle("Leaf damage due to FAW") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2, size = 1.2) +
  theme(legend.position = "none",
        plot.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 10, face = "bold"),
        axis.title = element_blank())


# Generalized linear mixed model

# What is a random effect (vs. a fixed factor)?
# 1. A categorical variable whose levels are chosen at random from a larger
# population
# 2. A source of random variation, whereas fixed effects are generally something
# the experimenter directly manipulates and are often repeatable
# 3. Factors that you aren't interested in

GLMM_LFD_FAW_mean = lmer(Number.of.plants.displaying.leaf.damage.by.fall.armyworm/10
                  ~ District + (1|Farmer) + Period + Vstage + Plot,
                  data = datasum)

summary(GLMM_LFD_FAW_mean)
plot(GLMM_LFD_FAW_mean)


# Generalized linear mixed model, binomial distribution

M_LFD_FAW = glmer(Number.of.plants.displaying.leaf.damage.by.fall.armyworm/10
                  ~ District + (1|Farmer) + Period + Vstage + Plot,
                  data = data, family = binomial(link = "logit"))

summary(M_LFD_FAW)
plot(M_LFD_FAW)


# Generalized linear mixed model, Poisson distribution

M_LFD_FAW = glmer(Number.of.plants.displaying.leaf.damage.by.fall.armyworm/10
                  ~ District + (1|Farmer) + Period + Vstage + Plot,
                  data = data, family = binomial(link = "logit"))

summary(M_LFD_FAW)
plot(M_LFD_FAW)


# Predicted values

pred_LFD_FAW_treatment = ggpredict(M_LFD_FAW, term = "Plot")
plot(pred_LFD_FAW_treatment)

pred_LFD_FAW_vstage = ggpredict(M_LFD_FAW, term = "Vstage")
plot(pred_LFD_FAW_vstage)





