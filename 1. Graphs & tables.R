#' ---
#' title: "Graphs & tables"
#' author: "Frédéric Baudron"
#' date: "April 9th, 2024"
#' ---


rm(list=ls())


if (!require('ggplot2')) install.packages("ggplot2")
if (!require('ggthemes')) install.packages("ggthemes")
if (!require('gtsummary')) install.packages("gtsummary")
if (!require('webshot2')) install.packages("webshot2")
if (!require('tidyr')) install.packages("tidyr")
if (!require('dplyr')) install.packages("dplyr")
if (!require('egg')) install.packages("egg")
if (!require('cowplot')) install.packages("cowplot")


library(ggplot2)
library(ggthemes)
library(gtsummary)
library(webshot2)
library(tidyr)
library(dplyr)
library(egg)
library(cowplot)


setwd('D:\\Mes Donnees\\2. CIMMYT\\2. Agroeco IDT\\zz. R training\\')

data = read.csv("Input files\\Survey.csv")

data = subset(data, subset = district == "Mbire")


# DATA PREPARATION--------------------------------------------------------------

comp = data[, c(1, 7, 20:29)]
comp$elderly = comp$total_number_of_adult_males_aged_61_or_more + comp$total_number_of_adult_females_aged_61_or_more
comp$adult = comp$total_number_of_adult_males_aged_25_60 + comp$total_number_of_adult_females_aged_25_60
comp$youth = comp$total_number_of_young_males_aged_15_25 + comp$total_number_of_young_females_aged_15_25
comp$family_size = comp$elderly + comp$adult + comp$youth + comp$total_number_of_children_and_teens_aged_3_14 + comp$total_number_of_infants_of_age_0_to_2


comp_long = comp[, c(1, 13:15, 9:10)]
names(comp_long) = c("ID", "Elderly", "Adults", "Youth", "Children & teens", "Infants")
comp_long = gather(comp_long, class, nb, "Elderly":"Infants")


maiz = data[, c(1, 7, 48, 177:198)]
maiz = maiz[maiz$area_ha_cropped_in_maize_during_the_season_2021_22 > 0,]
maiz$quantity_kg_of_basal_fertilizer_applied_to_maize_in_the_season_2021_22 = 
  maiz$quantity_kg_of_basal_fertilizer_applied_to_maize_in_the_season_2021_22 /
  maiz$area_ha_cropped_in_maize_during_the_season_2021_22
maiz$quantity_kg_of_topdressing_fertilizer_applied_to_maize_in_the_season_2021_22 = 
  maiz$quantity_kg_of_topdressing_fertilizer_applied_to_maize_in_the_season_2021_22 /
  maiz$area_ha_cropped_in_maize_during_the_season_2021_22
maiz$quantity_kg_of_manure_applied_to_maize_in_the_season_2021_22 = 
  maiz$quantity_kg_of_manure_applied_to_maize_in_the_season_2021_22 /
  maiz$area_ha_cropped_in_maize_during_the_season_2021_22
maiz$total_maize_production_in_the_season_2021_22_kg = 
  maiz$total_maize_production_in_the_season_2021_22_kg /
  maiz$area_ha_cropped_in_maize_during_the_season_2021_22
maiz$quantity_kg_of_compost_applied_to_maize_in_the_season_2021_22 = 
  maiz$quantity_kg_of_compost_applied_to_maize_in_the_season_2021_22 /
  maiz$area_ha_cropped_in_maize_during_the_season_2021_22


maiz_res = maiz[, c(1:2, 13:19)]
names(maiz_res) = c("ID", "Ward", "Burnt", "Retained in the field", "Grazed in the field",
                    "Harvested for feed", "Harvested for compost", "Harvested for fuel",
                    "Harvested for fencing")
maiz_res = gather(maiz_res, use, perc, "Burnt":"Harvested for fencing")
maiz_res$perc = as.numeric(maiz_res$perc)
maiz_res$perc = 100 * maiz_res$perc
maiz_res = aggregate(perc ~ use, FUN = mean, na.rm = TRUE, data = maiz_res)
maiz_res = maiz_res[maiz_res$perc > 0,]


maiz_pest1 = maiz[, c(1:2, 20)]
maiz_pest2 = maiz[, c(1:2, 21)]
maiz_pest3 = maiz[, c(1:2, 22)]
names(maiz_pest1) = c("ID", "Ward", "Pest")
names(maiz_pest2) = c("ID", "Ward", "Pest")
names(maiz_pest3) = c("ID", "Ward", "Pest")
maiz_pest = rbind(maiz_pest1, maiz_pest2, maiz_pest3)
maiz_pest$n = rep(1, nrow(maiz_pest))
maiz_pest = aggregate(n ~ Pest, FUN = sum, na.rm = TRUE, data = maiz_pest[, c(3:4)])
maiz_pest = maiz_pest[which(maiz_pest$Pest != "None"), ]


imp_crp = data[, c(1, 7, 294:308)]
names(imp_crp) = c("ID", "Ward",
                   "Quality certified seeds", "Community seed banks", "Drought tolerant varieties",
                   "Small grains", "Early planting", "Crop rotation", "Intercropping",
                   "Cover crops", "Mulching", "Integrated pest management", "Compost and/or manure",
                   "Drip- and/or micro-irrigation market", "Optimum plant density", "Tree planting on-farm",
                   "Tree retention (natural regeneration) on-farm")
imp_crp_long = gather(imp_crp, practice, perc, "Quality certified seeds":"Tree retention (natural regeneration) on-farm")
imp_crp_long$perc = as.numeric(imp_crp_long$perc == "Yes")
imp_crp_long = aggregate(perc ~ practice, FUN = mean, na.rm = TRUE, data = imp_crp_long[, c(3:4)])
imp_crp_long$perc = round(imp_crp_long$perc * 100, 1)
names(imp_crp_long) = c("Practice", "Adopters")
imp_crp_long$"Non-adopters" = 100 - imp_crp_long$Adopters
imp_crp_long = gather(imp_crp_long, Category, Percentage, "Adopters":"Non-adopters")


# TABLE-------------------------------------------------------------------------

names(comp)

comp[, c(2, 16, 13:15, 9:12)] %>%
  tbl_summary(
    by = ward,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p}%"))


# removing missing data and specifying digit number for continuous variables

comp[, c(2, 16, 13:15, 9:12)] %>%
  tbl_summary(
    by = ward,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p}%"),
    missing = "no",
    digits = all_continuous() ~ 1)


# specifying continuous and categorical variables

comp[, c(2, 16, 13:15, 9:12)] %>%
  tbl_summary(
    by = ward,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p}%"),
    type = list(family_size ~ "continuous",
                elderly ~ "continuous",
                adult ~ "continuous",
                youth ~ "continuous",
                total_number_of_children_and_teens_aged_3_14 ~ "continuous",
                total_number_of_infants_of_age_0_to_2 ~ "continuous"),
    missing = "no",
    digits = all_continuous() ~ 1)


# add a column for overall sample

comp[, c(2, 16, 13:15, 9:12)] %>%
  tbl_summary(
    by = ward,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p}%"),
    type = list(family_size ~ "continuous",
                elderly ~ "continuous",
                adult ~ "continuous",
                youth ~ "continuous",
                total_number_of_children_and_teens_aged_3_14 ~ "continuous",
                total_number_of_infants_of_age_0_to_2 ~ "continuous"),
    missing = "no",
    digits = all_continuous() ~ 1)%>%
  add_overall()


# change labels and add caption

comp[, c(2, 16, 13:15, 9:12)] %>%
  tbl_summary(
    by = ward,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p}%"),
    type = list(family_size ~ "continuous",
                elderly ~ "continuous",
                adult ~ "continuous",
                youth ~ "continuous",
                total_number_of_children_and_teens_aged_3_14 ~ "continuous",
                total_number_of_infants_of_age_0_to_2 ~ "continuous"),
    missing = "no",
    digits = all_continuous() ~ 1,
    label = list(family_size ~ "Total family size",
                 elderly ~ "Total number of elderly (aged 61 or more)",
                 adult ~ "Total number of adults (aged 26 to 60)",
                 youth ~ "Total number of youth (aged 15 to 25)",
                 total_number_of_children_and_teens_aged_3_14 ~ "Total number of children and teens (aged 3 to 14)",
                 total_number_of_infants_of_age_0_to_2 ~ "Total number of infants (aged 0 to 2)",
                 are_there_relatives_outside_the_household_who_help_financially ~ "Relatives outside the household helping finantially",
                 are_there_relatives_outside_the_household_who_depends_on_the_household_financially_e_g_elderly_sick ~ "Relatives outside the household depending on it finantially"))%>%
  add_overall()%>%
  modify_caption("Household composition")


# save

table = comp[, c(2, 16, 13:15, 9:12)] %>%
  tbl_summary(
    by = ward,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p}%"),
    type = list(family_size ~ "continuous",
                elderly ~ "continuous",
                adult ~ "continuous",
                youth ~ "continuous",
                total_number_of_children_and_teens_aged_3_14 ~ "continuous",
                total_number_of_infants_of_age_0_to_2 ~ "continuous"),
    missing = "no",
    digits = all_continuous() ~ 1,
    label = list(family_size ~ "Total family size",
                 elderly ~ "Total number of elderly (aged 61 or more)",
                 adult ~ "Total number of adults (aged 26 to 60)",
                 youth ~ "Total number of youth (aged 15 to 25)",
                 total_number_of_children_and_teens_aged_3_14 ~ "Total number of children and teens (aged 3 to 14)",
                 total_number_of_infants_of_age_0_to_2 ~ "Total number of infants (aged 0 to 2)",
                 are_there_relatives_outside_the_household_who_help_financially ~ "Relatives outside the household helping finantially",
                 are_there_relatives_outside_the_household_who_depends_on_the_household_financially_e_g_elderly_sick ~ "Relatives outside the household depending on it finantially"))%>%
  add_overall()%>%
  modify_caption("Household composition")

table %>%
  as_gt() %>%             # convert to gt table
  gt::gtsave(             # save table as image
    filename = "Table.png")


# BAR DIAGRAM-------------------------------------------------------------------

comp_long %>%
  ggplot(aes(x = ID, y = nb, fill = as.factor(class))) +
  geom_bar(stat = 'identity')


# change width and add black lines around bars with a specific width

comp_long %>%
  ggplot(aes(x = ID, y = nb, fill = as.factor(class))) +
  geom_bar(stat = 'identity', width = 1, color = "black", linewidth = 0.1)


# changing the order

comp_long %>%
  ggplot(aes(x = ID, y = nb, fill = as.factor(class))) +
  geom_bar(stat = 'identity', width = 1, color = "black", linewidth = 0.1, position = position_stack(reverse = TRUE))


# reorder bars by decreasing order

comp_long %>%
  ggplot(aes(x = reorder(ID, -nb, sum), y = nb, fill = as.factor(class))) +
  geom_bar(stat = 'identity', width = 1, color = "black", linewidth = 0.1, position = position_stack(reverse = TRUE))


# change color palette and legend title

comp_long %>%
  ggplot(aes(x = reorder(ID, -nb, sum), y = nb, fill = as.factor(class))) +
  geom_bar(stat = 'identity', width = 1, color = "black", linewidth = 0.1, position = position_stack(reverse = TRUE)) +
  scale_fill_viridis_d(option = "B", name = "Age class")
  

# add horizontal line with the mean family size

mean(comp$family_size)

comp_long %>%
  ggplot(aes(x = reorder(ID, -nb, sum), y = nb, fill = as.factor(class))) +
  geom_bar(stat = 'identity', width = 1, color = "black", linewidth = 0.1, position = position_stack(reverse = TRUE)) +
  scale_fill_viridis_d(option = "B", name = "Age class") +
  geom_hline(yintercept = mean(comp$family_size), linetype = "dashed", color = "grey50", linewidth = 1)
  

# change axis titles

comp_long %>%
  ggplot(aes(x = reorder(ID, -nb, sum), y = nb, fill = as.factor(class))) +
  geom_bar(stat = 'identity', width = 1, color = "black", linewidth = 0.1, position = position_stack(reverse = TRUE)) +
  scale_fill_viridis_d(option = "B", name = "Age class") +
  geom_hline(yintercept = mean(comp$family_size), linetype = "dashed", color = "grey50", linewidth = 1) +
  xlab("Households") + ylab("Number of family members")


# modifying the theme (check other themes: https://ggplot2.tidyverse.org/reference/ggtheme.html)

comp_long %>%
  ggplot(aes(x = reorder(ID, -nb, sum), y = nb, fill = as.factor(class))) +
  geom_bar(stat = 'identity', width = 1, color = "black", linewidth = 0.1, position = position_stack(reverse = TRUE)) +
  scale_fill_viridis_d(option = "B", name = "Age class") +
  geom_hline(yintercept = mean(comp$family_size), linetype = "dashed", color = "grey50", linewidth = 1) +
  xlab("Households") + ylab("Number of family members") + 
  theme_few()


# modify plot title, axis titles, axis text, and legend

comp_long %>%
  ggplot(aes(x = reorder(ID, -nb, sum), y = nb, fill = as.factor(class))) +
  geom_bar(stat = 'identity', width = 1, color = "black", linewidth = 0.1, position = position_stack(reverse = TRUE)) +
  scale_fill_viridis_d(option = "B", name = "Age class") +
  geom_hline(yintercept = mean(comp$family_size), linetype = "dashed", color = "grey50", linewidth = 1) +
  xlab("Households") + ylab("Number of family members") + 
  theme_few() +
  theme(plot.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12,  face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)),
        axis.ticks.x = element_blank(),
        panel.border = element_blank(),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        legend.position = c(0.9, 0.9), legend.justification = c(0.9, 0.9)) 


# save

ggsave("Bar diagram.jpeg", units = "cm", width = 35, height = 15, dpi = 320)


# PIE CHART---------------------------------------------------------------------

ggplot(maiz_res, aes(x = "", y = perc, fill = use)) + 
  geom_bar(alpha = 0.5, width = 1, stat = "identity", color = "black")


# wrapping the bar as a circle

ggplot(maiz_res, aes(x = "", y = perc, fill = use)) + 
  geom_bar(alpha = 0.5, width = 1, stat = "identity", color = "black") +
  coord_polar("y", start = 0)


# changing color palette and legend title

ggplot(maiz_res, aes(x = "", y = perc, fill = use)) + 
  geom_bar(alpha = 0.5, width = 1, stat = "identity", color = "black") +
  coord_polar("y", start = 0) + 
  scale_fill_viridis_d(option = "B", name = "Residue use")


# changing theme, titles, text, etc

ggplot(maiz_res, aes(x = "", y = perc, fill = use)) + 
  geom_bar(alpha = 0.5, width = 1, stat = "identity", color = "black") +
  coord_polar("y", start = 0) + theme_few() + 
  scale_fill_viridis_d(option = "B", name = "Residue use") +
  theme(plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),  
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14, face = "bold"),
        legend.position = "right") 


# save

ggsave("Pie chart.jpeg", units = "cm", width = 25, height = 15, dpi = 320)


# LOLLYPOP DIAGRAM--------------------------------------------------------------

maiz_pest %>%
  ggplot(aes(x = Pest, y = n)) + 
  geom_point(color="#781C6DFF", size = 8, alpha = 0.5)


# add segments

maiz_pest %>%
  ggplot(aes(x = Pest, y = n)) + 
  geom_segment(aes(x = Pest, xend = Pest, y = 0, yend = n), color = "#781C6DFF", linewidth = 1) +
  geom_point(color="#781C6DFF", size = 8, alpha = 0.5)


# order by increasing nb

maiz_pest %>%
  arrange(n) %>%
  mutate(order = factor(Pest, Pest)) %>%
  ggplot(aes(x = Pest, y = n)) + 
  geom_segment(aes(x = order, xend = Pest, y = 0, yend = n), color = "#781C6DFF", linewidth = 1) +
  geom_point(color="#781C6DFF", size = 8, alpha = 0.5)


# change axis titles

maiz_pest %>%
  arrange(n) %>%
  mutate(order = factor(Pest, Pest)) %>%
  ggplot(aes(x = Pest, y = n)) + 
  geom_segment(aes(x = order, xend = Pest, y = 0, yend = n), color = "#781C6DFF", linewidth = 1) +
  geom_point(color="#781C6DFF", size = 8, alpha = 0.5) +
  xlab("Species causing damage to maize") + ylab("Nb of farms affected")


# flip axes

maiz_pest %>%
  arrange(n) %>%
  mutate(order = factor(Pest, Pest)) %>%
  ggplot(aes(x = Pest, y = n)) + 
  geom_segment(aes(x = order, xend = Pest, y = 0, yend = n), color = "#781C6DFF", linewidth = 1) +
  geom_point(color="#781C6DFF", size = 8, alpha = 0.5) +
  xlab("Species causing damage to maize") + ylab("Nb of farms affected") +
  coord_flip()


# changing theme, titles, text, etc

maiz_pest %>%
  arrange(n) %>%
  mutate(order = factor(Pest, Pest)) %>%
  ggplot(aes(x = Pest, y = n)) + 
  geom_segment(aes(x = order, xend = Pest, y = 0, yend = n), color = "#781C6DFF", linewidth = 1) +
  geom_point(color="#781C6DFF", size = 8, alpha = 0.5) +
  xlab("Species causing damage to maize") + ylab("Nb of farms affected") +
  theme_few() +
  coord_flip() +
  theme(axis.title.x = element_text(size = 14, face = "bold", margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"))


# save

ggsave("Lollypop diagram.jpeg", units = "cm", width = 25, height = 15, dpi = 320)


# BUBBLE CHART------------------------------------------------------------------

ggplot(imp_crp_long, aes(x = Practice, y = Category, fill = Category, size = Percentage)) +
  geom_point(shape = 21, colour = "black", stroke = 1.1, alpha = 0.6)


# modify size range

ggplot(imp_crp_long, aes(x = Practice, y = Category, fill = Category, size = Percentage)) +
  geom_point(shape = 21, colour = "black", stroke = 1.1, alpha = 0.6) +
  scale_size_continuous(range = c(5, 30))


# changing colors

ggplot(imp_crp_long, aes(x = Practice, y = Category, fill = Category, size = Percentage)) +
  geom_point(shape = 21, colour = "black", stroke = 1.1, alpha = 0.6) +
  scale_size_continuous(range = c(5, 30)) + 
  scale_fill_manual(values = c("#781C6DFF", "#ED6925FF"))


# adding text

ggplot(imp_crp_long, aes(x = Practice, y = Category, fill = Category, size = Percentage)) +
  geom_point(shape = 21, colour = "black", stroke = 1.1, alpha = 0.6) +
  geom_text(aes(label = Percentage), colour = "black", size = 3) +
  scale_size_continuous(range = c(5, 30)) + 
  scale_fill_manual(values = c("#781C6DFF", "#ED6925FF"))


# changing theme, titles, text, etc

ggplot(imp_crp_long, aes(x = Practice, y = Category, fill = Category, size = Percentage)) +
  geom_point(shape = 21, colour = "black", stroke = 1.1, alpha = 0.6) +
  geom_text(aes(label = Percentage), colour = "black", size = 3) +
  scale_size_continuous(range = c(5, 30)) + 
  scale_fill_manual(values = c("#781C6DFF", "#ED6925FF")) +
  theme_few() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text.x = element_text(colour = "black", size = 10, face = "bold", angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 12, face = "bold"))


# save

ggsave("Bubble chart.jpeg", units = "cm", width = 40, height = 15, dpi = 320)



