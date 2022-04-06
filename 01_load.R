##### This script loads and cleans raw experiment data. Necessary to run subsequent scripts

library(tidyverse)
library(here)
library(Hmisc)
library(lme4)
library(lmerTest)
library(afex)
library(janitor)
library(data.table)

# create "not in" operator
`%notin%` <- Negate(`%in%`)

##### Load Data #####


### subject log

subject_log <- read_csv(here("data/subject-log.csv")) %>% 
  filter(study_completed == "yes")

subject_ids <- subject_log %>% 
  select(jatos_id, lhq_id)


### meta data

meta_eng_fr_br <- read_csv(here("data/full_meta_eng_fr_br_start.csv"))
meta_eng_fr_bl <- read_csv(here("data/full_meta_eng_fr_bl_start.csv"))
meta_fr_eng_br <- read_csv(here("data/full_meta_fr_eng_bl_start.csv"))
meta_fr_eng_bl <- read_csv(here("data/full_meta_fr_eng_br_start.csv"))

meta <- rbind(meta_eng_fr_bl, meta_eng_fr_br, meta_fr_eng_bl, meta_fr_eng_br) %>% 
  clean_names() %>% 
  rename(start = start_time, jatos_id = worker_id) %>% 
    #filter jatos ids to match those given to participants
  filter(jatos_id %in% subject_log$jatos_id) 

# just get jatos_id from meta
jatos_id <- meta %>% 
  select(exp_start, jatos_id)


### exp data

# add b_loc column to each dataset to note location of B

load(here("data/raw_data/eng_fr_br.Rda"))
eng_fr_br <- eng_fr_br %>% 
  mutate(b_loc = "right")

load(here("data/raw_data/eng_fr_bl.Rda"))
eng_fr_bl <- eng_fr_bl %>% 
  mutate(b_loc = "left") %>% 
  #remove extra columns that don't match other dfs
  select(-queryParams_batchId, -queryParams_personalSingleWorkerId, -queryParams_pre)

load(here("data/raw_data/fr_eng_bl.Rda"))
fr_eng_bl <- fr_eng_bl %>% 
  mutate(b_loc = "left") %>% 
  #remove extra columns that don't match other dfs
  select(-queryParams_batchId, -queryParams_personalSingleWorkerId, -queryParams_pre)

load(here("data/raw_data/fr_eng_br.Rda"))
fr_eng_br <- fr_eng_br %>% 
  mutate(b_loc = "right")


  

## combine all exp datasets

exp_df <- rbind(eng_fr_bl, eng_fr_br, fr_eng_bl, fr_eng_br) %>% 
  # select only necessary columns for analysis
  select(datetime, practice, block, target_word, target_onset, sound, VOT, response, b_loc) %>% 
  rename(exp_start = datetime) %>% 
  # join jatos_ids 
  left_join(jatos_id, by = "exp_start") %>% 
  # filter for runs that have jatos id
  filter(!is.na(jatos_id)) %>% 
  # change S's responses from s/l to b/p based on what version they saw
  mutate(response = case_when(
    response == "l" & b_loc == "right" ~ "b",
    response == "s" & b_loc == "right" ~ "p",
    response == "l" & b_loc == "left" ~ "p",
    response == "s" & b_loc == "left" ~ "b",
    TRUE ~ as.character(response)
  )) %>% 
  # separate block info into 2 columns
  separate(block, into = c("language", "block_number", "block_type"), sep = "_", remove = FALSE, extra = "merge")


# pull out headphones info (because I guess I programmed this wrong)
used_headphones <- exp_df %>% 
  filter(response == "y")

exp_df <- exp_df %>% 
  filter(response != "y" & response != "n")


## filter for only relevant responses

# isolate responses to test trials (not attention or practice)
exp_test <- exp_df %>% 
  filter(!str_detect(block_type, "attn_"), block_number != "prac") %>% 
  # create column combining block and jatos id to properly filter
  unite(attn_block, block, jatos_id, sep = "_", remove = FALSE) %>% 
  mutate(response = as.factor(response))


# isolate attention trials with response >= 3
exp_attn <- exp_df %>% 
  filter(str_detect(block_type, "attn_")) %>% 
  # separate attention to get in format of other blocks
  separate(block_type, into = c("attn", "block_type")) %>% 
  # unite block_number and block_type to match format of regular blocks
  unite(attn_block, language, block_number, block_type, jatos_id, sep = "_", remove = FALSE) %>% 
  filter(response >= 3)

# select only blocks participants had high enough attention for & join with LHQ IDs
exp_prefinal_all <- exp_test %>% 
  filter(attn_block %in% exp_attn$attn_block) %>% 
  left_join(subject_ids, by = "jatos_id")



# 
# exp_prefinal_headphones <- exp_test_attn %>% 
#   # select only participants who reported using headphones (not in pipe with previous in case we decide to include them or do a comparison)
#   filter(jatos_id %in% used_headphones$jatos_id) %>% 
#   # join with LHQ IDs
#   left_join(subject_ids, by = "jatos_id")
# 
# exp_prefinal_all <- exp_test_attn %>% 
#   # join with LHQ IDs
#   left_join(subject_ids, by = "jatos_id")





#### LHQ data 

### read in data ###

### LHQ follow up data
lhq_follow_up <- read_csv(here("data/LHQ_follow-up.csv"), col_types = cols(.default = "c")) %>% 
  # filter out duplicate responses - selecting most recent response
  filter(X1 != 1 & X1 != 2 & X1 != 3 & X1 != 6 & X1 != 15 & X1 != 24 & X1 != 27) %>% 
  # remove unnecessary columns
  select(- starts_with("X"), -L3, -L4)

# select name for follow up
lang_names_follow_up <- lhq_follow_up %>% 
  select(lhq_id, starts_with("name")) %>% 
  rename(L3 = name_L3, L4 = name_L4)

lhq_follow_up <- lhq_follow_up %>% 
  select(-name_L3, -name_L4) %>%
  # pivot wider to match other data
  pivot_longer(cols = -lhq_id, names_to = "question", values_to = "score_follow_up") %>% 
    # separate into multiple columns to match other data
    separate(question, into = c("category", "age_ability", "lang")) %>% 
    # remove L3, L4 rows if no data
    filter(!is.na(score_follow_up)) %>% 
    # get language names of L3 and L4
    left_join(lang_names_follow_up, by = "lhq_id") %>% 
    mutate(lang = case_when(
      lang == "L3" ~ L3,
      lang == "L4" ~ L4,
      # change language abbreviations while we're at it
      lang == "eng" ~ "English",
      lang == "fr" ~ "French"
    )) %>% 
  # remove L3, L4 columns
  select(-L3, -L4)
 
  


### original data ###

lhq_raw_1 <- read_csv(here("data/LHQ_1_error-code.csv"), col_types = cols(.default = "c")) %>% 
  #remove duplicate row for lhq_id jwii9
  slice(-37)

lhq_raw_2 <- read_csv(here("data/LHQ_2_error-code.csv"), col_types = cols(.default = "c"))

lhq_raw_3 <- read_csv(here("data/LHQ_3_error-code.csv"), col_types = cols(.default = "c"))

lhq_raw <- rbind(lhq_raw_1, lhq_raw_2, lhq_raw_3)


## get rid of unnecessary columns & language names
lhq_resp <- lhq_raw %>% 
  select(- starts_with("X"), - starts_with("name"), -error_comments) 

## make response data long
lhq_long <- lhq_resp %>% 
  pivot_longer(listening_age_1:writing_ability_4, 
               names_to = c("category", "age_ability", "language"), 
               names_sep = "_", 
               values_to = "score", 
               values_drop_na = TRUE)

## get language names
lang_names <- lhq_raw %>% 
  select(lhq_id, starts_with("name")) %>% 
  rename('1' = "name_1", '2' = "name_2", '3' = "name_3", '4' = "name_4") %>% 
  pivot_longer('1':'4', names_to = "language", values_to = "lang", values_drop_na = TRUE)

## join tables for final long data
lhq <- lhq_long %>% 
  left_join(lang_names, by = c("lhq_id", "language")) %>% 
  select(-language) %>% 
  # join follow up responses
  left_join(lhq_follow_up, by = c("lhq_id", "category", "age_ability", "lang")) %>% 
  # change error codes for those who completed follow up - group_by() and any() will change all error_code values, not just ones with value in score_follow_up
  group_by(lhq_id) %>% 
  mutate(error_code = case_when(
    any(!is.na(score_follow_up)) ~ "0",
    TRUE ~ as.character(error_code)
  ))

lhq$score <- as.numeric(lhq$score)

write.csv(lhq, "data_frames/lhq.csv")


# get mean ability for each language
lhq_ability <- lhq %>% 
  filter((lang == "English" | lang == "French") & age_ability == "ability") %>% 
  group_by(lhq_id, lang) %>% 
  mutate(score = as.numeric(score)) %>% 
  dplyr::summarize(score = mean(score)) %>% 
  pivot_wider(id_cols = lhq_id, names_from = lang, values_from = score) %>% 
  rename(overall_ability_English = English, overall_ability_French = French) %>% 
  #filter for those with a mean ability in each lang (Guess some did not fill it out)
  filter(!is.na(overall_ability_English) & !is.na(overall_ability_French)) %>% 
  # compute proficiency difference score
  mutate(proficiency_diff = overall_ability_English - overall_ability_French)


#### filter for participants who meet criteria



### recoding for those who did not complete LHQ follow-up

## Error Code 1: people who noted native language as learning from 1 or 2 
lhq_non_0_native <- lhq %>% 
  # filter for subjects, AoA, and listening/speaking categories
  filter(error_code == 1 & age_ability == "age" & (category == "listening" | category == "speaking")) %>% 
  # change 1s and 2s to 0s - Not simply subtracting, because assuming that reported ages above this are correct
  mutate(score_corrected = case_when(
    score == 1 | score == 2 ~ 0,
    TRUE ~ as.numeric(score)
  )) %>% 
  # select only relevant columns
  select(lhq_id, category, age_ability, lang, score_corrected)

## Error Code 2: Can't correct, because we cannot make assumption about how their proficiency response maps to AoA

## Error Code 3: reported years with ability instead of AoA

lhq_total_years <- lhq %>% 
  # filter for subjects, AoA, and listening/speaking categories
  filter(error_code == 3 & age_ability == "age" & category != "yearstotal") %>% 
  mutate(age = as.numeric(age),
         score_corrected = age - score) %>% 
  # select only relevant columns
  select(lhq_id, category, age_ability, lang, score_corrected)

## Error Code 4: Can't use, because we don't understand how they filled it out

## Error Code 5: Doesn't need to be corrected, but *DO NOT USE THESE PARTICIPANTS IN ANALYSES WITH READING/WRITING* 


lhq_corrections <- rbind(lhq_non_0_native, lhq_total_years)

## join corrected data
lhq_corrected <- lhq %>% 
  left_join(lhq_corrections, by = c("lhq_id", "category", "age_ability", "lang")) %>% 
  # replace NA values in score_corrected column (meaning S filled in LHQ correctly) with their original scores
  mutate(score = as.numeric(score),
         score_corrected = coalesce(score_corrected, score)) %>% 
  # cannot work with error code 2 or 4
  filter(error_code != 2 & error_code != 4) %>% 
  # filter for those with an ability score in each lang
  filter(lhq_id %in% lhq_ability$lhq_id)


# check to make sure Eng or Fr was learned from birth
lhq_fr_eng <- lhq_corrected %>% 
  filter((lang == "French" | lang == "English") & age_ability == "age" & category == "listening" & score_corrected == 0)

# check to make sure another language WASN'T learned from birth
# create df with those that did learn another language from birth
lhq_other <- lhq_corrected %>% 
  filter((lang != "French" & lang != "English") & age_ability == "age" & category == "listening" & score_corrected == 0)
  

# filter lhq data down to those that meet language exposure criteria
lhq_final_sample <- lhq_corrected %>% 
  filter(lhq_id %in% lhq_fr_eng$lhq_id & lhq_id %notin% lhq_other$lhq_id) %>% 
  ungroup()




### create final exp dataframe for analysis

# make LHQ data wide to be able to join with exp df
lhq_wide <- lhq_final_sample %>% 
  filter(lang == "French" | lang == "English") %>% 
  mutate(category = paste(category, age_ability, lang, sep = "_")) %>% 
  pivot_wider(id_cols = c("lhq_id", "age","gender"), names_from = category, values_from = score_corrected) %>% 
  # join overall ability
  left_join(lhq_ability, by = "lhq_id")

# final df
exp_final_all <- exp_prefinal_all %>% 
  # filter for participants who met the language criteria
  filter(lhq_id %in% lhq_final_sample$lhq_id) %>% 
  # join LHQ data
  left_join(lhq_wide, by = "lhq_id") %>% 
  # rescale variables to help fit in model
  mutate(
    language_c = case_when(
      language == "en" ~ 0,
      language == "fr" ~ 1), 
    language_c = arm::rescale(language_c),
    onset_c = case_when(
      target_onset == "b" ~ 0,
      target_onset == "p" ~ 1 ),
    onset_c = arm::rescale(onset_c),
    # create headphones column based on their reported used of headphones
    headphones = case_when(
      jatos_id %in% used_headphones$jatos_id ~ "Y",
      TRUE ~ "N"
    ))


# filter for those who used headphones
exp_final_headphones <- exp_final_all %>% 
  filter(headphones == "Y")




# separate by full and trunc files & write CSVs

exp_full_all <- exp_final_all %>% 
  filter(block_type == "full")

exp_trunc_all <- exp_final_all %>% 
  filter(block_type == "trunc")

write_csv(exp_final_all, here("data_frames/exp_final_all.csv"))
write_csv(exp_full_all, here("data_frames/exp_full_all.csv"))  
write_csv(exp_trunc_all, here("data_frames/exp_trunc_all.csv"))  
  


exp_full_headphones <- exp_final_headphones %>% 
  filter(block_type == "full")

exp_trunc_headphones <- exp_final_headphones %>% 
  filter(block_type == "trunc")

write_csv(exp_final_headphones, here("data_frames/exp_final_headphones.csv"))
write_csv(exp_full_headphones, here("data_frames/exp_full_headphones.csv"))  
write_csv(exp_trunc_headphones, here("data_frames/exp_trunc_headphones.csv"))  
  
# final LHQs

lhq_final_all <- lhq_final_sample %>% 
  filter(lhq_id %in% exp_final_all$lhq_id)

lhq_final_headphones <- lhq_final_sample %>% 
  filter(lhq_id %in% exp_final_headphones$lhq_id)

write_csv(lhq_final_all, here("data_frames/lhq_final_all.csv"))
write_csv(lhq_final_headphones, here("data_frames/lhq_final_headphones.csv"))

### create df with participants who do not meet inclusion criteria (non-keepers)

# start with not contributing any blocks due to low attention scores
non_keepers <- exp_df %>% 
  filter(jatos_id %notin% exp_prefinal_all$jatos_id) %>% 
  group_by(jatos_id) %>% 
  dplyr::summarize(n = n()) %>% 
  mutate(reason = "inattention") %>% 
  select(-n)

## update non-keepers info for those who did not meet language criteria

non_keepers_lang <- exp_df %>% 
  filter(jatos_id %notin% exp_final_all$jatos_id & jatos_id %notin% non_keepers$jatos_id) %>% 
  group_by(jatos_id) %>% 
  dplyr::summarize(n = n()) %>% 
  mutate(reason = "language") %>% 
  select(-n)

non_keepers <- rbind(non_keepers, non_keepers_lang)

## update non-keepers info for those who did not use headphones

non_keepers_headphones <- exp_final_all %>% 
  filter(jatos_id %notin% exp_final_headphones$jatos_id & jatos_id %notin% non_keepers$jatos_id) %>% 
  group_by(jatos_id) %>% 
  dplyr::summarize(n = n()) %>% 
  mutate(reason = "headphones") %>% 
  select(-n)

non_keepers <- rbind(non_keepers, non_keepers_headphones) %>% 
  group_by(reason) %>% 
  dplyr::summarize(reason_n = n())

# check to make sure all non-keepers were removed 
# original/raw data - final data == n non-keepers should == TRUE
length(unique(exp_df$jatos_id)) - length(unique(exp_final_headphones$jatos_id)) == sum(non_keepers$reason_n)

write_csv(non_keepers, here("data_frames/non_keepers.csv"))
