library(haven)
library(tidyverse)
library(gt)
library(gtsummary)
library(gtExtras)
library(ggplot2)
library(reshape2)
library(chest)
library(ufs)
library(arm)
library(interactions)

######################################
# Table1. Background characteristics
######################################

data <- read_sav("./data.sav")

pr.scale = 5 # prior scale for Bayesian GLM

my_cramer_v <- function(data, variable, by, ...) {
  table(data[[variable]], data[[by]]) %>%
    rstatix::cramer_v()}


table_desc <-  data %>% 
  tbl_summary(by = Migration_status, 
              type = all_dichotomous() ~ "categorical")  %>%  
  add_overall(last = T) %>% 
  add_p(test = list(all_categorical() ~ "chisq.test",
                    c('Trauma_types', 'SPD') ~ "fisher.test")) %>% 
  add_stat(fns = all_categorical() ~ my_cramer_v) %>% 
  modify_header(add_stat_1 ~ "**Effect size**") %>% 
  modify_header(label = "**Variable**") %>% # 
  modify_spanning_header(c("stat_1", "stat_2", "stat_0") ~ "**Migration Status**") %>% 
  bold_labels() %>% 
  bold_p(t = 0.05) %>% 
  as_gt() %>% 
  tab_source_note(source_note = md("p <= 0.05"))



####################################################################################
# Figure 1. Comparative Overview: Traumatic Events Experiences by Migration Status
####################################################################################

data_lec <- read_sav("./data_lec.sav")

data_lec <- data_lec %>% 
  melt(id.vars = c('Migration_status'), variable.name = 'LEC', value.name = 'Total') %>%
  group_by(Migration_status, LEC) %>% summarise(total = sum(as.numeric(Total), na.rm = TRUE), .groups = "drop_last") %>% 
  mutate(percentage = round(total / sum(total) * 100, 1))                                                                

trauma_p <- ggplot(data_lec, aes(x = Migration_status, y = LEC, fill = percentage)) +
  geom_tile(color = "white", size = 0.5) +  # Create the tiles
  scale_fill_gradient(low = "white", high = "red") +  # Color gradient
  geom_text(aes(label = round(percentage, 1)), color = "black", size = 5) +  
  labs(title = "",
       x = " ",
       y = "Trauma types",
       fill = "Percentage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1, face = 'bold', size = 13),    
        axis.text.y = element_text(size = 13),
        axis.title.y = element_text(size = 13,face = 'bold'))                



#########################################################################################
# Cramers V effect size (Source: https://www.depts.ttu.edu/aged/toolbox/effect_size.pdf)
#########################################################################################

# Example for Gender and Migration Status (with Bootstrapped Confidence Interval)
set.seed(18)
confIntV(data$Gender, data$Migration_status)



###########################################################
# Supplementary Figure 1 - Change in Estimate Forest Plot 
###########################################################

conf <- chest_glm(
  crude = "SPD ~ Migration_status", xlist = c("Age", "Gender","Education", "Trauma_types", "Depression"),
  na_omit = TRUE, data = data)

chest_forest(conf)



###########################################################
# Supplementary Figure 2 - Change in Estimate Forest Plot 
###########################################################

conf_ind <- chest_glm(
  crude = "Employment ~ Migration_status", xlist = c("Age", "Gender","Education", "Trauma_types", "Depression", 'PTSD'),
  na_omit = TRUE, data = data)

chest_forest(conf_ind)



################################################################################
# Supplementary Figure 4 - Interaction effects between Migration Status and SPD
################################################################################

model <- bayesglm(SPD ~ Migration_status*Anxiety + Age + Gender + Employment + Trauma_types + Depression, 
                  family = binomial(link = 'logit'), data = data, prior.scale = pr.scale, prior.df = Inf)


moderator <- cat_plot(model, pred = Migration_status, modx = Anxiety, geom = 'bar', x.label = 'Migration Status', y.label = 'Somatoform Pain Disorder')










