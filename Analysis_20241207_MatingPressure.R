# --- Load packages ----
library(tidyverse)
library(dplyr)
library(emmeans)
library(multcomp) # post-hoc test

# --- Experiments 1 ----

## --- Load data ----
path_1 <- "C:/R/Rproject/ Introduction2Rstudio/data/Woojeong_Park_experiment1.csv"
exp_1 <- read.csv(path_1)

## --- EDA ----
head(exp_1, 3)
str(exp_1) # 100 obs of 3 variables
summary(exp_1)

unique(exp_1$species) # "F.albicollis" "F.hypoleuca" 
exp_1 |>
  count(species)  # balanced

# plot the exp_1 data
exp_1 |>
  ggplot(aes(x = tarsus_length_mm, y = sperm_length_um, col = species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + # line plot
  theme_minimal() +
  NULL

## --- Modeling ----
# fit the model to exp_1 data
mol_exp_1 <- lm(sperm_length_um ~ species * tarsus_length_mm, exp_1)

# diagnostic plots
par(mfrow = c(2, 2))
plot(mol_exp_1)
par(mfrow = c(1, 1))

# ANCOVA
anova(mol_exp_1)

# coefficient table
summary(mol_exp_1)

# post-hac test
emmeans(mol_exp_1, pairwise ~ species | tarsus_length_mm , adjust = "tukey")

# # refit the model to exp_1_new data
# exp_1_new <- exp_1 |>
#   mutate(speceis = relevel(factor(species), ref = "F.albicollis"))
# 
# mol_exp_1_new <- lm(sperm_length_um ~ species * tarsus_length_mm, exp_1_new)
# 
# # diagnostic plots
# par(mfrow = c(2, 2))
# plot(mol_exp_1_new)
# par(mfrow = c(1, 1))
# 
# # ANCOVA
# anova(mol_exp_1_new)
# 
# # coefficient table
# summary(mol_exp_1_new)

## ---- Results ----
# No significant relationship between sperm length and species.
# No significant interaction between species and leg length on sperm length.
# A significant relationship between sperm length and leg length.

# --- Experiment 2 ----

## --- Load data ----
path_2 <- "C:/R/Rproject/ Introduction2Rstudio/data/Woojeong_Park_experiment2.csv"
exp_2 <- read.csv(path_2)

## --- EDA  ----
head(exp_2, 3)
str(exp_2) # 30 obs. of  4 variables
summary(exp_2)

## --- Tidy data ----
exp_2_new <- exp_2 |>
  pivot_longer(cols = 2:4, names_to = "treatment", values_to = "vcl") |>
  select(-male_ID) |>
  mutate(treatment = relevel(factor(treatment), ref = "control"))

head(exp_2_new, 3)
str(exp_2_new)
unique(exp_2_new$treatment) # "control", "conspecific", "heterospecific"

## --- Plot the data ----
exp_2_new |>
  ggplot(aes(x = treatment, y = vcl)) +
  geom_boxplot() +
  NULL

## --- modeling ----
# fit the model to exp_2_new data
mol_exp_2 <- lm(vcl ~ treatment, exp_2_new)

# diagnostic plot
par(mfrow = c(2, 2))
plot(mol_exp_2)
par(mfrow = c(1, 1))

# ANOVA
anova(mol_exp_2)
## There is a statistically significant difference in vcl across treatments.

# coefficients table
summary(mol_exp_2)

# post-hac test
tukey.par <- glht(mol_exp_2, linfct = mcp(treatment = "Tukey"))
tukey_results <- summary(tukey.par)
tukey_results


## --- Results ----
# Conspecific shows a statistically significant difference when compared to 
# both heterospecific and control. However, heterospecific does not show a
# statistically significant difference when compared to control. 

## --- Visualization ----
# plot 
Figure_1 <- exp_2_new |>
  ggplot(aes(x = treatment, y = vcl, fill = treatment)) +
  geom_boxplot() +
  labs(x = " Treatment",
       y = "Curvilinear velocity (VCL, µm/second)") +
  scale_x_discrete(limits = c("conspecific", "heterospecific", "control")) +
  scale_y_continuous(limits = c(0, 700)) +
  theme_classic() +
  ggtitle("Sperm Swimming Speed in Conspecific, Heterospecific, and Control Groups") 
# theme(plot.title = element_text(hjust = 0.5),
# plot.title.position = "plot") 

Figure_1
ggsave("Rplot/StatsReport_Figure_1.pdf", plot = Figure_1 )

