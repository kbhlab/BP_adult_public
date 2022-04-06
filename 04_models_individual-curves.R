library(here)
library(tidyverse)
library(data.table)



## load data

exp_full_headphones <- read_csv(here("data_frames/exp_full_headphones.csv")) %>% 
  mutate(resp = case_when(
    response == "b" ~ 0,
    response == "p" ~ 1 ))

exp_trunc_headphones <- read_csv(here("data_frames/exp_trunc_headphones.csv")) %>% 
  mutate(resp = case_when(
    response == "b" ~ 0,
    response == "p" ~ 1 ))

participants <- exp_full_headphones %>% 
  distinct(jatos_id)


##### Full words

# set contrast coding values
eng <- -0.503305
fr <- 0.496695
b <- -0.5002179
p <- 0.4997821


# create empty dfs
boundaries_lang <- data.frame(matrix(ncol = 4))
x <- c("jatos_id", "block", "lang", "pred_boundary")
colnames(boundaries_lang) <- x

boundaries_onset <- data.frame(matrix(ncol = 5))
x <- c("jatos_id", "block", "lang", "onset", "pred_boundary")
colnames(boundaries_onset) <- x



# function to predict a boundary for each participant based on language 
predict_boundary_lang <- function(model, id, block, lang, onset) {
  boundary <- (.5 - coef(model)["(Intercept)"] - coef(model)["language_c"]*lang) / (coef(model)["VOT"] + coef(model)["VOT:language_c"]*lang)
  
  boundaries_lang <<- boundaries_lang %>% 
    add_row(jatos_id = as.character(id), block = block, lang = lang, pred_boundary = boundary)
}


# function to predict a boundary for each participant based on language and target onset
predict_boundary_onset <- function(model, id, block, lang, onset) {
  boundary <- (.5 - coef(model)["(Intercept)"] - coef(model)["language_c"]*lang - coef(model)["onset_c"]*onset) / (coef(model)["VOT"] + coef(model)["VOT:language_c"]*lang + coef(model)["VOT:onset_c"]*onset)
  
  boundaries_onset <<- boundaries_onset %>% 
    add_row(jatos_id = as.character(id), block = block, lang = lang, onset = onset, pred_boundary = boundary)
}


# function to run predict_boundary over each language for both full and truncated blocks
compile_boundaries_lang <- function(id) {
  
  model_data_full <- exp_full_headphones %>% 
    filter(jatos_id == id)
  
  model_data_trunc <- exp_trunc_headphones %>% 
    filter(jatos_id == id)
  
  model_full <<- glm(as.factor(response) ~ VOT + language_c + VOT*language_c, data = model_data_full, family = "binomial")
  
  model_trunc <<- glm(as.factor(response) ~ VOT + language_c + VOT*language_c, data = model_data_trunc, family = "binomial")
  
  
  predict_boundary_lang(model_full, id, "full", eng)
  predict_boundary_lang(model_full, id, "full", fr)
  
  predict_boundary_lang(model_trunc, id, "trunc", eng)
  predict_boundary_lang(model_trunc, id, "trunc", fr)
  
  boundaries_lang <<- boundaries_lang %>%
    filter(!is.na(lang)) %>%
    mutate(language = case_when(lang == -0.503305 ~ "English",
                                TRUE ~ "French"))
}


# function to run predict_boundary over each language, target onset combo for both full and truncated blocks
compile_boundaries_onset <- function(id) {
  
  model_data_full <- exp_full_headphones %>% 
    filter(jatos_id == id)
  
  model_data_trunc <- exp_trunc_headphones %>% 
    filter(jatos_id == id)
  
  model_full <<- glm(as.factor(response) ~ VOT + language_c + onset_c + VOT*language_c + VOT*onset_c, data = model_data_full, family = "binomial")
  
  model_trunc <<- glm(as.factor(response) ~ VOT + language_c + onset_c + VOT*language_c + VOT*onset_c, data = model_data_trunc, family = "binomial")
  

  predict_boundary_onset(model_full, id, "full", eng, b)
  predict_boundary_onset(model_full, id, "full", eng, p)
  predict_boundary_onset(model_full, id, "full", fr, b)
  predict_boundary_onset(model_full, id, "full", fr, p)
  
  predict_boundary_onset(model_trunc, id, "trunc", eng, b)
  predict_boundary_onset(model_trunc, id, "trunc", eng, p)
  predict_boundary_onset(model_trunc, id, "trunc", fr, b)
  predict_boundary_onset(model_trunc, id, "trunc", fr, p)

  boundaries_onset <<- boundaries_onset %>%
    filter(!is.na(lang)) %>%
    mutate(language = case_when(lang == -0.503305 ~ "English",
                            TRUE ~ "French"),
           target_onset = case_when(onset == -0.5002179 ~ "b",
                             TRUE ~ "p"))
}





# calculate boundaries for each participant
map_dfr(participants$jatos_id, compile_boundaries_lang)
map_dfr(participants$jatos_id, compile_boundaries_onset)

write_csv(boundaries_lang, here("data_frames/individual-boundaries-lang.csv"))
write_csv(boundaries_onset, here("data_frames/individual-boundaries-onset.csv"))

boundaries_lang_summary <- boundaries_lang %>% 
  filter(!is.na(pred_boundary)) %>% 
  group_by(block, language) %>% 
  dplyr::summarize(n = n(), mean_boundary = mean(pred_boundary), median_boundary = median(pred_boundary), sem = sqrt(var(pred_boundary)/length(pred_boundary)))

boundaries_onset_summary <- boundaries_onset %>% 
  filter(!is.na(pred_boundary)) %>% 
  group_by(block, language, target_onset) %>% 
  dplyr::summarize(n = n(), mean_boundary = mean(pred_boundary), median_boundary = median(pred_boundary), sem = sqrt(var(pred_boundary)/length(pred_boundary)))




## boundary rank

# create boundary ranks
boundaries_lang <- boundaries_lang %>% 
  filter(!is.na(pred_boundary)) %>% 
  group_by(block) %>% 
  mutate(rank_block = order(order(pred_boundary, decreasing=TRUE))) %>% 
  group_by(block, language) %>% 
  mutate(rank_language = order(order(pred_boundary, decreasing=TRUE)))

boundaries_onset <- boundaries_onset %>% 
  filter(!is.na(pred_boundary)) %>% 
  group_by(block) %>% 
  mutate(rank_block = order(order(pred_boundary, decreasing=TRUE))) %>% 
  group_by(block, language) %>% 
  mutate(rank_language = order(order(pred_boundary, decreasing=TRUE))) %>% 
  group_by(block, language, target_onset) %>% 
  mutate(rank_onset = order(order(pred_boundary, decreasing=TRUE))) 



# calculate median and sem based on block + language
rank_lang <- boundaries_lang %>% 
  group_by(block, language) %>% 
  dplyr::summarize(median_rank_block = median(rank_block), sem = sqrt(var(rank_block)/length(rank_block))) %>% 
  mutate(sem_min = median_rank_block - sem, sem_max = median_rank_block + sem)

write_csv(rank_lang, here("data_frames/rank-language.csv"))

# calculate median and sem based on block + language + target onset
rank_onset <- boundaries_onset %>% 
  group_by(block, language, target_onset) %>% 
  # this compares median value for b's and p's within each language and block type
  dplyr::summarize(median_rank_onset = median(rank_language), 
                   sem_onset = sqrt(var(rank_language)/length(rank_language)))%>% 
  mutate(sem_min_onset = median_rank_onset - sem_onset, 
         sem_max_onset = median_rank_onset + sem_onset)

write_csv(rank_onset, here("data_frames/rank-onset.csv"))



##### statistical comparisons
### Paired samples Wilcoxon test

# arranging df's for paired comparison

boundaries_lang_wide <- boundaries_lang %>% 
  pivot_wider(id_cols = "jatos_id", names_from = c("block", "language"), values_from = "pred_boundary")

boundaries_onset_wide <- boundaries_onset %>% 
  pivot_wider(id_cols = "jatos_id", names_from = c("block", "language", "target_onset"), values_from = "pred_boundary")



# comparing language
full_language <- wilcox.test(boundaries_lang_wide$full_English, boundaries_lang_wide$full_French, paired = TRUE)

trunc_language <- wilcox.test(boundaries_lang_wide$trunc_English, boundaries_lang_wide$trunc_French, paired = TRUE)



# comparing onset in English
full_eng <- wilcox.test(boundaries_onset_wide$full_English_b, boundaries_onset_wide$full_English_p, paired = TRUE)
trunc_eng <- wilcox.test(boundaries_onset_wide$trunc_English_b, boundaries_onset_wide$trunc_English_p, paired = TRUE)

# comparing onset in French
full_fr <- wilcox.test(boundaries_onset_wide$full_French_b, boundaries_onset_wide$full_French_p, paired = TRUE)
trunc_fr <- wilcox.test(boundaries_onset_wide$trunc_French_b, boundaries_onset_wide$trunc_French_p, paired = TRUE)



##### Visualizations

pal1 <- c("#006d2c", "#810f7c")
pal1_rev <- c("#810f7c", "#006d2c")
pal2 <- c("#e41a1c", "#377eb8")


boundaries_lang$language <- ordered(boundaries_lang$language, levels = c("French", "English"))
boundaries_onset$target_onset <- ordered(boundaries_onset$target_onset, levels = c("p", "b"))

# predicted boundaries by language
ggplot(boundaries_lang, aes(x = pred_boundary, y = factor(language), color = factor(language))) +
  geom_violin(draw_quantiles = 0.5) +
  geom_jitter(height = 0.2, alpha = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  facet_grid(. ~ block, labeller = as_labeller(c("full" = "Full words", "trunc" = "Truncated words"))) +
  theme(legend.position = "none") +
  labs(x = "Predicted boundary (ms)", y = "Language") +
  scale_color_manual(values = pal2)

# predicted boundaries by language + target onset
ggplot(boundaries_onset, aes(x = pred_boundary, y = factor(target_onset), color = factor(target_onset))) +
  geom_violin(draw_quantiles = 0.5) +
  geom_jitter(height = 0.2, alpha = 0.1)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_discrete(labels = c("/p/", "/b/")) +
  facet_grid(language ~ block, labeller = as_labeller(c("full" = "Full words", "trunc" = "Truncated words", "English" = "English", "French" = "French"))) +
  theme(legend.position = "none") +
  labs(x = "Predicted boundary (ms)", y = "Target onset") +
  scale_color_manual(values = pal1_rev)



### boundary rank

# plot boundary ranks by language
ggplot(boundaries_lang, aes(x = rank_block, y = factor(language), color = factor(language))) +
  geom_jitter(height = 0.2, alpha = 0.2) +
  geom_point(data = rank_lang, aes(x = median_rank_block, y = factor(language)), size = 3, color = "black") +
  geom_segment(data = rank_lang, aes(x = sem_min, xend = sem_max, y = language, yend = language), color = "black") +
  facet_grid(. ~ block, labeller = as_labeller(c("full" = "Full words", "trunc" = "Truncated words"))) +
  theme(legend.position = "none") +
  labs(x = "Boundary rank", y = "Language") +
  scale_color_manual(values = pal2)

# plot boundary ranks by language and target onset
ggplot(boundaries_onset, aes(x = rank_language, y = factor(target_onset), color = factor(target_onset))) +
  geom_jitter(height = 0.2, alpha = 0.2) +
  geom_point(data = rank_onset, aes(x = median_rank_onset, y = factor(target_onset)), size = 3, color = "black") +
  geom_segment(data = rank_onset, aes(x = sem_min_onset, xend = sem_max_onset, y = target_onset, yend = target_onset), color = "black") +
  facet_grid(language ~ block, labeller = as_labeller(c("full" = "Full words", "trunc" = "Truncated words", "English" = "English", "French" = "French"))) +
  theme(legend.position = "none") +
  labs(x = "Boundary rank", y = "Target onset") +
  scale_color_manual(values = pal1_rev)
  
