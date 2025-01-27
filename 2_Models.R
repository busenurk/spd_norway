library(haven)
library(tidyverse)
library(arm)
library(gtsummary)

###################################################################################
# Table 2. Multivariable Bayesian Logistic Regression for SPD by Ethnic Background
###################################################################################

data <- read_sav("./data.sav")

pr.scale = 5 # prior scale for Bayesian GLM

# Crude model
################
model_cr <- bayesglm(SPD ~  Migration_status, family = binomial(link = 'logit'), data = data,
                     prior.scale = pr.scale, prior.df = Inf) %>% 
            tbl_regression(exponentiate = T) %>%  bold_labels() %>%  bold_p(t = 0.05) %>% as_gt()


# Adjusted for Age, Gender, Trauma types, Depression
######################################################
model1 <- bayesglm(SPD ~  Migration_status + Age + Gender + Trauma_types + Depression, family = binomial(link = 'logit'), data = data,
                   prior.scale = pr.scale, prior.df = Inf) %>% 
          tbl_regression(exponentiate = T) %>% add_vif %>% bold_labels() %>%  bold_p(t = 0.05) %>% as_gt()



# Adjusted for Age, Gender, Trauma types, Depression, Employment
###################################################################
model2 <- bayesglm(SPD ~  Migration_status + Age + Gender + Trauma_types + Depression + Employment, family = binomial(link = 'logit'), data = data,
                   prior.scale = pr.scale, prior.df = Inf) %>% 
          tbl_regression(exponentiate = T) %>% add_vif %>% bold_labels() %>%  bold_p(t = 0.05) %>% as_gt()



# Adjusted for Age, Gender, Trauma types, Depression, Employment with interaction term
#########################################################################################
model3 <- bayesglm(SPD ~  Migration_status*Anxiety + Age + Gender + Trauma_types + Depression + Employment, family = binomial(link = 'logit'), data = data,
                   prior.scale = pr.scale, prior.df = Inf) %>%
          tbl_regression(exponentiate = T) %>% add_vif %>% bold_labels() %>%  bold_p(t = 0.05) %>% as_gt()



# Additional model - PTSD explains Employment
##############################################

bayesglm(Employment ~ PTSD, family = binomial(link = 'logit'), data = data, prior.scale = pr.scale, prior.df = Inf) %>% 
tbl_regression(exponentiate = T) %>%  bold_labels() %>%  bold_p(t = 0.05) %>% as_gt()



#########################################################################################################################
# Table 3: Multivariable Bayesian Logistic Regression for Migration Status and SPD Stratified by Anxiety Status (Yes/No) 
#########################################################################################################################

data_anx_yes <- data %>% filter(Anxiety == 'Yes')

data_anx_no <- data %>% filter(Anxiety == 'No')


model_anx_yes <- bayesglm(SPD ~ Migration_status + Age + Gender + Employment + Trauma_types + Depression, family = binomial(link = 'logit'), data = data_anx_yes,
                          prior.scale = pr.scale, prior.df = Inf) %>% 
                 tbl_regression(exponentiate = T) %>% add_vif %>%   
                 bold_labels() %>%  bold_p(t = 0.0503) %>% as_gt()


model_anx_no <- bayesglm(SPD ~ Migration_status + Age + Gender + Employment + Trauma_types + Depression, family = binomial(link = 'logit'), data = data_anx_no,
                         prior.scale = pr.scale, prior.df = Inf) %>% 
                tbl_regression(exponentiate = T) %>% add_vif %>%   
                bold_labels() %>%  bold_p(t = 0.05) %>% as_gt()



#################################################
# Mediation Analysis for Employment
#################################################

data$Employment <- as.numeric(data$Employment)


indirect.model5.1 <- bayesglm(Employment ~ Migration_status, data = data, family = gaussian(link = "identity"),
                              prior.scale = pr.scale, prior.df = Inf) 

indirect.model5.2 <- bayesglm(SPD ~ Migration_status*Anxiety  + Age + Gender  + Employment + Trauma_types + Depression, data = data, family = binomial(link="logit"),
                              prior.scale = pr.scale, prior.df = Inf)

set.seed(25)
mediation <- mediate(indirect.model5.1, indirect.model5.2,
                     treat = 'Migration_status', mediator = 'Employment',
                     boot = TRUE, sims = 1000, control.value = 1, treat.value = 2) %>% summary()



## Control moderated mediation (no moderated mediation)

anx_yes <- mediate(indirect.model5.1, indirect.model5.2, treat = 'Migration_status', mediator = 'Employment',
                   covariates = list(Anxiety = 1), boot = T, sims = 1000, control.value = 0, treat.value = 1)

anx_no <- mediate(indirect.model5.1, indirect.model5.2, treat = 'Migration_status', mediator = 'Employment',
                  covariates = list(Anxiety = 0), boot = T, sims = 1000, control.value = 0, treat.value = 1)


























