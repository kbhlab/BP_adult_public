library(tidyverse)
library(here)
library(Hmisc)
library(lme4)
library(lmerTest)
library(beepr)
library(arm)
library(boot)
library(separationplot)
library(performance)
library(DescTools)

# load data

## "all" means all participants, incl those who did NOT use headphones
exp_full_all <- read_csv(here("data_frames/exp_full_all.csv"))
exp_trunc_all <- read_csv(here("data_frames/exp_trunc_all.csv"))

exp_full_headphones <- read_csv(here("data_frames/exp_full_headphones.csv"))  %>% 
  mutate(resp = case_when(
    response == "b" ~ 0,
    response == "p" ~ 1 ))
exp_trunc_headphones <- read_csv(here("data_frames/exp_trunc_headphones.csv"))  %>% 
  mutate(resp = case_when(
    response == "b" ~ 0,
    response == "p" ~ 1 ))

lhq <- read_csv(here("data_frames/lhq_final_headphones.csv"))

## load models
full <- readRDS(here("data_frames/model_full_prof_diff.Rds"))

trunc <- readRDS(here("data_frames/model_trunc_prof_diff.Rds"))

##### model predictions #####

### full model

# make new df with all predictor combos
newdata <- with(exp_full_headphones, expand.grid(VOT = unique(VOT), language_c = unique(language_c), onset_c = unique(onset_c), proficiency_diff = unique(proficiency_diff)))

# calculate predictions
newdata$predictions <- predict(full, newdata = newdata, type = "response", re.form = NA)

VOT_predictions_full <- newdata %>% 
  mutate(language_factor = as.factor(case_when(
    language_c < 0 ~ "English",
    language_c > 0 ~ "French"
  ))) 

#plot predictions and observed data
exp_full_headphones %>% 
  ggplot(aes(x = VOT, y = resp, color = language)) +
  stat_summary(fun.data = "mean_cl_boot", size = 1) +
  stat_summary(fun.data = "mean_cl_boot", geom = 'line', size = 1) +
  geom_smooth(data = VOT_predictions_full, aes(x = VOT, y = predictions, color = language_factor)) +
  facet_grid(. ~ onset_c) 

ggplot(VOT_predictions_full, aes(x = VOT, y = predictions, color = language_factor)) +
  geom_smooth() +
  facet_grid(. ~ onset_c)

# calculate boundaries?
boundaries <- VOT_predictions_full %>% 
  group_by(language_factor, onset_c, VOT) %>% 
  dplyr::summarize(mean_prediction = mean(predictions))


# binned residuals
binnedplot(predict(full), resid(full))
binned.resids(predict(full), resid(full))



#classification accuracy
lrAcc <- function(lrMod, responseVar, use.ranef=TRUE){
  if(!is.factor(model.frame(lrMod)[,responseVar])){ model.frame(lrMod)[,responseVar] <- as.factor(model.frame(lrMod)[,responseVar]) }
  if(use.ranef){ preds = predict(lrMod, newdata=model.frame(lrMod)) } else{ 
    preds = predict(lrMod, newdata=model.frame(lrMod), re.form=NA) }
  preds <- ((sign(preds)/2)+0.5)
  respVarValues <- model.frame(lrMod)[,responseVar] 
  if(is.numeric(respVarValues)){ y <- respVarValues } else{ 
    y <- (as.numeric(model.frame(lrMod)[,responseVar])-1) }
  acc <- sum(preds==y)/length(preds)
  return(acc)
}

baselineAcc <- function(lrMod, responseVar){ 
  response <- model.frame(lrMod)[,responseVar] 
  tab <- table(response) 
  return(max(tab)/sum(tab)) }

## full

lrAcc(full, as.factor("response")) # 88.68
baselineAcc(full, as.factor("response")) # 54.83

#classification accuracy is 89% compared to 54% in the baseline model

lrAcc(full, as.factor("response"), use.ranef = F) #82.6% 
#model prediction for an average participant and average item is still better than baseline

## trunc

lrAcc(trunc, as.factor("response")) # 83.73
baselineAcc(trunc, as.factor("response")) # 50.97

#classification accuracy is 84% compared to 50% in the baseline model

lrAcc(trunc, as.factor("response"), use.ranef = F) #81.7% 

#qqplots 
qqnorm(residuals(trunc))
qqline(residuals(trunc))

qqnorm(residuals(full))
qqline(residuals(full))

#Model criticism based on Greenhill, Ward & Sacks, 2011.



glm_trunc<- glm(as.factor(response) ~ VOT + language_c + onset_c + 
                  VOT * language_c + VOT * onset_c + proficiency_diff, data = exp_trunc_headphones, 
                family = "binomial")

glm_full <- glm(as.factor(response) ~ VOT + language_c + onset_c + 
                    VOT * language_c + VOT * onset_c + proficiency_diff + proficiency_diff * 
                    onset_c, data = exp_full_headphones, family = "binomial")

#calculate the Brier scores for the  models.
#Brier Scores = the mean value of the squared difference between the fitted and actual values of the dependent variable
#The closer the Brier score is to zero, the better the forecast.

BrierScore(glm_trunc) #.13
BrierScore(glm_full) #.12


#calculate the expected percentage of correct predictions for the models both using the Herron and Gelman Hill methods
#ePCP is essentially a measure of the average of the probabilities that the model assigns to the correct outcome category for each observation (whether that may be 0 or 1

performance_pcp(glm_trunc, ci=0.95) #  Full model: 74.18% [73.36% - 74.99%]
                                # Null model: 50.02% [49.09% - 50.95%]

performance_pcp(glm_trunc, ci=0.95, method="Gelman-Hill") #  Full model: 81.54% [80.82% - 82.26%]
                                                          #  Null model: 50.97% [50.04% - 51.90%]


performance_pcp(glm_full, ci=0.95)    #Full model: 76.38% [75.59% - 77.17%]
                                      #Null model: 50.47% [49.54% - 51.40%]

performance_pcp(glm_full, ci=0.95, method="Gelman-Hill")   #Full model: 82.54% [81.83% - 83.24%]
                                                            #Null model: 54.83% [53.91% - 55.76%]


#separation plot
#They are different lenghts :/
separationplot(newdata$predictions, exp_full_headphones$resp)
