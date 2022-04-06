##### This script builds logistic mixed effect models for BP-adult data (separated into full word v truncated word blocks) through an iterative process. Follows plans in pre-registration unless noted (https://osf.io/5jkxd/)

library(tidyverse)
library(here)
library(Hmisc)
library(lme4)
library(lmerTest)
library(beepr)

# load data

## "all" means all participants, incl those who did NOT use headphones
exp_full_all <- read_csv(here("data_frames/exp_full_all.csv"))
exp_trunc_all <- read_csv(here("data_frames/exp_trunc_all.csv"))

exp_full_headphones <- read_csv(here("data_frames/exp_full_headphones.csv")) 
exp_trunc_headphones <- read_csv(here("data_frames/exp_trunc_headphones.csv")) 

lhq <- read_csv(here("data_frames/lhq_final_headphones.csv"))

##### FULL BLOCKS #####

### Model 1 : as specified in pre-reg

full_base <- glmer(as.factor(response) ~ VOT + language_c + onset_c + VOT*language_c + VOT*onset_c + (1 | jatos_id) + (1 |target_word), glmerControl(optimizer = "bobyqa"), data=exp_full_headphones, family = "binomial")

summary(full_base)
beep()


### Model 2 : adding proficiency difference as fixed effect - different from pre-reg, since we combined proficiency in both languages into a single measure

full_prof <- glmer(as.factor(response) ~ VOT + language_c + onset_c + VOT*language_c + VOT*onset_c + proficiency_diff + (1 | jatos_id) + (1 |target_word), glmerControl(optimizer = "bobyqa"), data=exp_full_headphones, family = "binomial")

# summary(full_prof_all)
summary(full_prof)
beep()

anova(full_base, full_prof)
## retain proficiency


### Model 3 : adding interactions with proficiency 

### Model 3a: and language

full_prof_lang <- glmer(as.factor(response) ~ VOT + language_c + onset_c + VOT*language_c + VOT*onset_c + proficiency_diff + proficiency_diff*language_c + (1| jatos_id) + (1 |target_word), glmerControl(optimizer = "bobyqa"), data=exp_full_headphones, family = "binomial")

summary(full_prof_lang)
beep()

anova(full_prof, full_prof_lang)
## do not retain proficiency x lang interaction


### Model 3b : and target onset

full_prof_onset <- glmer(as.factor(response) ~ VOT + language_c + onset_c + VOT*language_c + VOT*onset_c + proficiency_diff + proficiency_diff*onset_c + (1| jatos_id) + (1 |target_word), glmerControl(optimizer = "bobyqa"), data=exp_full_headphones, family = "binomial")

summary(full_prof_onset)
beep()

anova(full_prof, full_prof_onset)
## retain interaction



### Model 4 : adding random effects - first for participant

### Model 4a : VOT

full_prof_onset_random1 <- glmer(as.factor(response) ~ VOT + language_c + onset_c + VOT*language_c + VOT*onset_c + proficiency_diff + proficiency_diff*onset_c + (1 +VOT || jatos_id) + (1 |target_word), glmerControl(optimizer = "bobyqa"), data=exp_full_headphones, family = "binomial")

summary(full_prof_onset_random1)
beep()

anova(full_prof_onset, full_prof_onset_random1)
# sig so continue


### Model 4b : language

full_prof_onset_random2 <- glmer(as.factor(response) ~ VOT + language_c + onset_c + VOT*language_c + VOT*onset_c + proficiency_diff + proficiency_diff*onset_c + (1 + VOT + language_c || jatos_id) + (1 |target_word), glmerControl(optimizer = "bobyqa"), data=exp_full_headphones, family = "binomial")

summary(full_prof_onset_random2)
beep()

anova(full_prof_onset_random1, full_prof_onset_random2)
# sig so continue


# ### Model 4c : onset

full_prof_onset_random3 <- glmer(as.factor(response) ~ VOT + language_c + onset_c + VOT*language_c + VOT*onset_c + proficiency_diff + proficiency_diff*onset_c + (1 + VOT + language_c + onset_c || jatos_id) + (1 |target_word), glmerControl(optimizer = "bobyqa"), data=exp_full_headphones, family = "binomial")

summary(full_prof_onset_random3)
beep()
# fails to converge, remove onset



### Model 5 : adding random effects for word

### Model 5a : VOT 
##### SELECTED AS FINAL MODEL #####

full_prof_onset_random2_1 <- glmer(as.factor(response) ~ VOT + language_c + onset_c + VOT*language_c + VOT*onset_c + proficiency_diff + proficiency_diff*onset_c + (1 + VOT + language_c || jatos_id) + (1 + VOT ||target_word), glmerControl(optimizer = "bobyqa"), data=exp_full_headphones, family = "binomial")

summary(full_prof_onset_random2_1)
beep()

anova(full_prof_onset_random2, full_prof_onset_random2_1)
# sig so continue



# save as Rds to reuse when knitting and save time
saveRDS(full_prof_onset_random2_1, file = here("data_frames/model_full_prof_diff.Rds"))

## rerun final model with those who didn't wear headphones to compare
full_prof_onset_random2_1_all <- glmer(as.factor(response) ~ VOT + language_c + onset_c + VOT*language_c + VOT*onset_c + proficiency_diff + proficiency_diff*onset_c + (1 + VOT + language_c || jatos_id) + (1 + VOT ||target_word), glmerControl(optimizer = "bobyqa"), data=exp_full_all, family = "binomial")

summary(full_prof_onset_random2_1_all)
beep()

# save as Rds to reuse when knitting and save time
saveRDS(full_prof_onset_random1_2_all, file = here("data_frames/model_full_prof_diff_all.Rds"))


### Model 5b : language

full_prof_onset_random2_2 <- glmer(as.factor(response) ~ VOT + language_c + onset_c + VOT*language_c + VOT*onset_c + proficiency_diff + proficiency_diff*onset_c + (1 + VOT + language_c || jatos_id) + (1 + VOT + language_c ||target_word), glmerControl(optimizer = "bobyqa"), data=exp_full_headphones, family = "binomial")

summary(full_prof_onset_random2_2)
beep()
#model is nearly identifiable





## Create new prediction data frame to be able to visualize model

# make new df with all predictor combos
newdata <- with(exp_full_headphones, expand.grid(VOT = unique(VOT), language_c = unique(language_c), onset_c = unique(onset_c), proficiency_diff = unique(proficiency_diff)))

# calculate predictions
newdata$predictions <- predict(full_prof_onset_random2_1, newdata = newdata, type = "response", re.form = NA)

VOT_predictions <- newdata %>% 
  mutate(language_factor = as.factor(case_when(
    language_c < 0 ~ "English",
    language_c > 0 ~ "French")),
    onset_factor = as.factor(case_when(
      onset_c < 0 ~ "b",
      onset_c > 0 ~ "p"
    ))) 

write_csv(VOT_predictions, here("data_frames/model_predictions_full.csv"))

ggplot(VOT_predictions, aes(x = VOT, y = predictions, color = factor(onset_c))) +
  geom_smooth() +
  facet_grid(. ~ language_factor)
geom_vline(xintercept = 4) +
  geom_vline(xintercept = 6)



##### TRUNCATED BLOCKS #####

### Model 1 : as specified in pre-reg

trunc_base <- glmer(as.factor(response) ~ VOT + language_c + onset_c + VOT*language_c + VOT*onset_c + (1 | jatos_id) + (1 |target_word), glmerControl(optimizer = "bobyqa"), data=exp_trunc_headphones, family = "binomial")

summary(trunc_base)
beep()

### Model 2 : adding proficiency as fixed effect
##### SELECTED AS FINAL MODEL #####

trunc_prof <- glmer(as.factor(response) ~ VOT + language_c + onset_c + VOT*language_c + VOT*onset_c + proficiency_diff + (1 | jatos_id) + (1 |target_word), glmerControl(optimizer = "bobyqa"), data=exp_trunc_headphones, family = "binomial")

summary(trunc_prof)
beep()

anova(trunc_prof, trunc_base)
# sig improves model

saveRDS(trunc_prof, file = here("data_frames/model_trunc_prof_diff.Rds"))


### Rerun final model with all participants

trunc_prof_all <- glmer(as.factor(response) ~ VOT + language_c + onset_c + VOT*language_c + VOT*onset_c + proficiency_diff + (1 | jatos_id) + (1 |target_word), glmerControl(optimizer = "bobyqa"), data=exp_trunc_all, family = "binomial")

summary(trunc_prof_all)
beep()

# save as Rds to reuse when knitting and save time
saveRDS(trunc_prof_all, file = here("data_frames/model_trunc_prof_diff_all.Rds"))


### Model 3 : proficiency x language 

trunc_prof_lang <- glmer(as.factor(response) ~ VOT + language_c + onset_c + VOT*language_c + VOT*onset_c + proficiency_diff + proficiency_diff*language_c+ (1 | jatos_id) + (1 |target_word), glmerControl(optimizer = "bobyqa"), data=exp_trunc_headphones, family = "binomial")

summary(trunc_prof_lang)
beep()

anova(trunc_prof, trunc_prof_lang)
# interarcton doesn't significantly improve, remove interaction but retain ME




### Model 4 : adding in random effects for participant

### Model 4a : adding VOT as random slope

trunc_prof_random1 <- glmer(as.factor(response) ~ VOT + language_c + onset_c + VOT*language_c + VOT*onset_c + proficiency_diff + (1 + VOT | jatos_id) + (1 |target_word), glmerControl(optimizer = "bobyqa"), data=exp_trunc_headphones, family = "binomial")

summary(trunc_prof_random1)
beep()
# fit is singular

anova(trunc_prof_random1, trunc_prof)
# sig improvement to model, but again, singular fit, so remove









##### Exp AoA Analysis #####

# create df
aoa <- lhq %>% 
  # filter long data for only age of acquisition (listening) for only English & French
  filter(age_ability == "age" & category == "listening" & (lang == "English" | lang == "French")) 

# calculate descriptives
aoa_desc <- aoa %>% 
  group_by(lang) %>% 
  dplyr::summarize(mean = mean(score_corrected), sd = sd(score_corrected), min = min(score_corrected), max = max(score_corrected))


# plot distribution
ggplot(aoa, aes(x = score_corrected)) +
  geom_histogram(bins = 18) +
  facet_grid(. ~ lang)

ggsave("figures/aoa_distribution.png", width = 10, height = 6.9)


# plot relationship w proficiency

prof <- lhq %>% 
  # filter long data for only age of acquisition (listening) for only English & French
  filter(age_ability == "ability" & (lang == "English" | lang == "French")) %>% 
  group_by(lhq_id, lang) %>% 
  dplyr::summarize(prof = mean(score_corrected))

aoa_prof <- aoa %>% 
  left_join(prof, by = c("lhq_id", "lang"))

ggplot(aoa_prof, aes(x = score_corrected, y = prof, color = lang)) +
  geom_jitter()

ggsave("figures/aoa_proficiency.png", width = 10, height = 6.9)


## not really a lot of variability, probs don't include in model


##--------models with standardized variables to calculate standardized coefficients------------##
library(QuantPsyc)

##------full words headphones model with standardized variables---------------##
exp_full_hp_stndrd<- exp_full_headphones

VOT_z <- as.data.frame(Make.Z(exp_full_headphones$VOT))
language_c_z <- as.data.frame(Make.Z(exp_full_headphones$language_c))
onset_c_z<- as.data.frame(Make.Z(exp_full_headphones$onset_c))
overall_ability_English_z<-as.data.frame(Make.Z(exp_full_headphones$overall_ability_English))
overall_ability_French_z<-as.data.frame(Make.Z(exp_full_headphones$overall_ability_French))
proficiency_diff_z <- as.data.frame(Make.Z(exp_full_headphones$proficiency_diff))

exp_full_hp_stndrd<- cbind(exp_full_hp_stndrd, VOT_z)
exp_full_hp_stndrd<- exp_full_hp_stndrd%>% rename(VOT_z = V1)
exp_full_hp_stndrd<- cbind(exp_full_hp_stndrd, language_c_z)
exp_full_hp_stndrd<- exp_full_hp_stndrd%>% rename(language_c_z = V1)
exp_full_hp_stndrd<- cbind(exp_full_hp_stndrd, onset_c_z)
exp_full_hp_stndrd<- exp_full_hp_stndrd%>% rename(onset_c_z = V1)
exp_full_hp_stndrd<- cbind(exp_full_hp_stndrd, overall_ability_English_z)
exp_full_hp_stndrd<- exp_full_hp_stndrd%>% rename(overall_ability_English_z = V1)
exp_full_hp_stndrd<- cbind(exp_full_hp_stndrd, overall_ability_French_z)
exp_full_hp_stndrd<- exp_full_hp_stndrd%>% rename(overall_ability_French_z= V1)
exp_full_hp_stndrd<- cbind(exp_full_hp_stndrd, proficiency_diff_z)
exp_full_hp_stndrd<- exp_full_hp_stndrd%>% rename (proficiency_diff_z= V1)




##---------------truncated words headphones model with standardized variables-----------------##
exp_trunc_hp_stndrd <- exp_trunc_headphones

trunc_VOT_z <- as.data.frame(Make.Z(exp_trunc_headphones$VOT))
trunc_language_c_z <- as.data.frame(Make.Z(exp_trunc_headphones$language_c))
trunc_overall_ability_English_z<-as.data.frame(Make.Z(exp_trunc_headphones$overall_ability_English))
trunc_overall_ability_French_z<-as.data.frame(Make.Z(exp_trunc_headphones$overall_ability_French))
trunc_onset_c_z<- as.data.frame(Make.Z(exp_trunc_headphones$onset_c))
trunc_proficiency_diff_z <- as.data.frame(Make.Z(exp_trunc_headphones$proficiency_diff))

#We want to add onset before language to be consistent with the other data frames 
exp_trunc_hp_stndrd<- cbind(exp_trunc_hp_stndrd, trunc_VOT_z)
exp_trunc_hp_stndrd<- exp_trunc_hp_stndrd%>% rename(VOT_z = V1)
exp_trunc_hp_stndrd<- cbind(exp_trunc_hp_stndrd, trunc_onset_c_z)
exp_trunc_hp_stndrd<- exp_trunc_hp_stndrd%>% rename(onset_c_z = V1)
exp_trunc_hp_stndrd<- cbind(exp_trunc_hp_stndrd, trunc_language_c_z)
exp_trunc_hp_stndrd<- exp_trunc_hp_stndrd%>% rename(language_c_z = V1)
exp_trunc_hp_stndrd<- cbind(exp_trunc_hp_stndrd, trunc_overall_ability_English_z)
exp_trunc_hp_stndrd<- exp_trunc_hp_stndrd%>% rename(overall_ability_English_z = V1)
exp_trunc_hp_stndrd<- cbind(exp_trunc_hp_stndrd, trunc_overall_ability_French_z)
exp_trunc_hp_stndrd<- exp_trunc_hp_stndrd%>% rename(overall_ability_French_z= V1)
exp_trunc_hp_stndrd<- cbind(exp_trunc_hp_stndrd, trunc_proficiency_diff_z)
exp_trunc_hp_stndrd<- exp_trunc_hp_stndrd%>% rename (proficiency_diff_z= V1)

##--------------full participants full words-----------------------------##
exp_full_all_stndrd <- exp_full_all

full_full_VOT_z <- as.data.frame(Make.Z(exp_full_all$VOT))
full_full_language_c_z <- as.data.frame(Make.Z(exp_full_all$language_c))
full_full_onset_c_z<- as.data.frame(Make.Z(exp_full_all$onset_c))
full_full_overall_ability_English_z<-as.data.frame(Make.Z(exp_full_all$overall_ability_English))
full_full_overall_ability_French_z<-as.data.frame(Make.Z(exp_full_all$overall_ability_French))
full_full_proficiency_diff_z <- as.data.frame(Make.Z(exp_full_all$proficiency_diff))

exp_full_all_stndrd<- cbind(exp_full_all_stndrd, full_full_VOT_z )
exp_full_all_stndrd<- exp_full_all_stndrd%>% rename(VOT_z = V1)
exp_full_all_stndrd<- cbind(exp_full_all_stndrd, full_full_language_c_z)
exp_full_all_stndrd<- exp_full_all_stndrd%>% rename(language_c_z = V1)
exp_full_all_stndrd<- cbind(exp_full_all_stndrd, full_full_onset_c_z)
exp_full_all_stndrd<- exp_full_all_stndrd%>% rename(onset_c_z = V1)
exp_full_all_stndrd<- cbind(exp_full_all_stndrd, full_full_overall_ability_English_z)
exp_full_all_stndrd<- exp_full_all_stndrd%>% rename(overall_ability_English_z = V1)
exp_full_all_stndrd<- cbind(exp_full_all_stndrd, full_full_overall_ability_French_z)
exp_full_all_stndrd<- exp_full_all_stndrd%>% rename(overall_ability_French_z= V1)
exp_full_all_stndrd<- cbind(exp_full_all_stndrd, full_full_proficiency_diff_z)
exp_full_all_stndrd<- exp_full_all_stndrd%>% rename (proficiency_diff_z= V1)

##-------------full participants truncated words-------------------------##
exp_trunc_all_stndrd <- exp_trunc_all

trunc_all_VOT_z <- as.data.frame(Make.Z(exp_trunc_all$VOT))
trunc_all_language_c_z <- as.data.frame(Make.Z(exp_trunc_all$language_c))
trunc_all_onset_c_z<- as.data.frame(Make.Z(exp_trunc_all$onset_c))
trunc_all_overall_ability_English_z<-as.data.frame(Make.Z(exp_trunc_all$overall_ability_English))
trunc_all_overall_ability_French_z<-as.data.frame(Make.Z(exp_trunc_all$overall_ability_French))
trunc_all_proficiency_diff_z <- as.data.frame(Make.Z(exp_trunc_all$proficiency_diff))


exp_trunc_all_stndrd<- cbind(exp_trunc_all_stndrd, trunc_all_VOT_z  )
exp_trunc_all_stndrd<- exp_trunc_all_stndrd%>% rename(VOT_z = V1)
exp_trunc_all_stndrd<- cbind(exp_trunc_all_stndrd, trunc_all_language_c_z)
exp_trunc_all_stndrd<- exp_trunc_all_stndrd%>% rename(language_c_z = V1)
exp_trunc_all_stndrd<- cbind(exp_trunc_all_stndrd, trunc_all_onset_c_z)
exp_trunc_all_stndrd<- exp_trunc_all_stndrd%>% rename(onset_c_z = V1)
exp_trunc_all_stndrd<- cbind(exp_trunc_all_stndrd, trunc_all_overall_ability_English_z)
exp_trunc_all_stndrd<- exp_trunc_all_stndrd%>% rename(overall_ability_English_z = V1)
exp_trunc_all_stndrd<- cbind(exp_trunc_all_stndrd, trunc_all_overall_ability_English_z)
exp_trunc_all_stndrd<- exp_trunc_all_stndrd%>% rename(overall_ability_French_z= V1)
exp_trunc_all_stndrd<- cbind( exp_trunc_all_stndrd, trunc_all_proficiency_diff_z)
exp_trunc_all_stndrd<- exp_trunc_all_stndrd%>% rename (proficiency_diff_z= V1)


#----------------Constructing models with standardized variables to get standardized coefficients-------------------##
## Model 5a Full words headphones
standardized_full_hp <- glmer(as.factor(response) ~ VOT_z + language_c_z + onset_c_z + VOT_z*language_c_z + VOT_z*onset_c_z + proficiency_diff_z + proficiency_diff_z*onset_c_z + (1 + VOT_z + language_c_z || jatos_id) + (1 + VOT_z ||target_word), glmerControl(optimizer = "bobyqa"), data=exp_full_hp_stndrd, family = "binomial")
summary(standardized_full_hp)
saveRDS(standardized_full_hp , file = here("data_frames/standardized_full_hp.Rds"))

## Model 5a Full words full participants 
standardized_full_full <- glmer(as.factor(response) ~ VOT_z + language_c_z + onset_c_z + VOT_z*language_c_z + VOT_z*onset_c_z + proficiency_diff_z + proficiency_diff_z*onset_c_z + (1 + VOT_z + language_c_z || jatos_id) + (1 + VOT_z ||target_word), glmerControl(optimizer = "bobyqa"), data=exp_full_all_stndrd, family = "binomial")
summary(standardized_full_full)
saveRDS(standardized_full_full , file = here("data_frames/standardized_full_full.Rds"))


## Model 2  Trunc Full participants
standaridized_trunc_full <- glmer(as.factor(response) ~ VOT_z + language_c_z + onset_c_z + VOT_z*language_c_z + VOT_z*onset_c_z + proficiency_diff_z + (1 | jatos_id) + (1 |target_word), glmerControl(optimizer = "bobyqa"), data=exp_trunc_all_stndrd , family = "binomial")
summary(standaridized_trunc_full)
saveRDS(standaridized_trunc_full , file = here("data_frames/standaridized_trunc_full.Rds"))


## Model 2  Trunc  headphones
standardized_trunc_hp <- glmer(as.factor(response) ~ VOT_z + language_c_z + onset_c_z + VOT_z*language_c_z + VOT_z*onset_c_z + proficiency_diff_z + (1 | jatos_id) + (1 |target_word), glmerControl(optimizer = "bobyqa"), data=exp_trunc_hp_stndrd, family = "binomial")
summary(standardized_trunc_hp)
saveRDS(standardized_trunc_hp  , file = here("data_frames/standardized_trunc_hp.Rds"))


##### model predictions #####

### full model

# make new df with all predictor combos
newdata <- with(exp_full_headphones, expand.grid(VOT = unique(VOT), language_c = unique(language_c), onset_c = unique(onset_c), proficiency_diff = unique(proficiency_diff)))

# calculate predictions
newdata$predictions <- predict(full_prof_onset_random2_1, newdata = newdata, type = "response", re.form = NA)

VOT_predictions_full <- newdata %>% 
  mutate(language_factor = as.factor(case_when(
    language_c < 0 ~ "English",
    language_c > 0 ~ "French"
  ))) 

exp_full_headphones %>% 
  ggplot(aes(x = VOT, y = response, color = language)) +
  stat_summary(fun.data = "mean_cl_boot", size = 1) +
  stat_summary(fun.data = "mean_cl_boot", geom = 'line', size = 1) +
  geom_smooth(data = VOT_predictions_full, aes(x = VOT, y = predictions, color = language_factor)) +
  facet_grid(. ~ onset_c) 
  # geom_vline(xintercept = 4) +
  # geom_vline(xintercept = 6)

library(arm)
binnedplot(predict(full_prof_onset_random2_1), resid(full_prof_onset_random2_1))
binned.resids(predict(full_prof_onset_random2_1), resid(full_prof_onset_random2_1))

library(boot)
# VOT_predictions_full <- VOT_predictions_full %>% 
#   mutate(cooks_d = glm.diag(full_prof_onset_random2_1)$cook)
#This throws an error, try a different way

#############

library(languageR)
library(influence.ME)

pairscor.fnc(ranef(full_prof_onset_random2_1)$jatos_id)
#VOT and language look like they are normally distributed
#intercept slightly skewed
#Potential outlier in language

pairscor.fnc(ranef(full_prof_onset_random2_1)$target_word)
#Maybe outliers with the VOT?
#Intercept roughly normally distributed
#VOT is skewed

influence.partic <- influence(full_prof_onset_random2_1, group="jatos_id")

cd=cooks.distance(influence.partic)
save(cd, file = "cd")
cooksDistance=data.frame(participant=rownames(cd),CD=cd)
ggplot(data=cooksDistance,aes(x = reorder(participant,CD),y = CD)) + geom_point()+xlab("cook distance per participant")

4/19 #Cooks distance cut of = 0.21 

m3.influence.word <- influence(M3, group="target_word")
cd2=cooks.distance(m3.influence.word)
save(cd2, file = "cd2")
cooksDistance2=data.frame(word=rownames(cd2),CD=cd2)
ggplot(data=cooksDistance2,aes(x = reorder(word,CD),y = CD)) + geom_point()+xlab("cook distance per word")

4/12 #cook's distance cut off= 0.33 , the word candy is an outlier (bonbon)

############


### truncated model

# make new df with all predictor combos
newdata <- with(exp_trunc_headphones, expand.grid(VOT = unique(VOT), language_c = unique(language_c), onset_c = unique(onset_c), proficiency_diff = unique(proficiency_diff)))

# calculate predictions 
newdata$predictions <- predict(trunc_prof, newdata = newdata, type = "response", re.form = NA)

VOT_predictions_trunc <- newdata %>% 
  mutate(language_factor = as.factor(case_when(
    language_c < 0 ~ "English",
    language_c > 0 ~ "French"
  ))) 

ggplot(VOT_predictions_trunc, aes(x = VOT, y = predictions, color = language_factor)) +
  geom_smooth() +
  #facet_grid(. ~ language_factor) +
geom_vline(xintercept = -2) +
  geom_vline(xintercept = 2)

