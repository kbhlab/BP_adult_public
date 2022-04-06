library(here)
library(tidyverse)
library(ggtext)
library(patchwork)
library(extrafont)
font_import(paths = c("C:/Users/lenav/AppData/Local/Microsoft/Windows/Fonts/"), prompt= FALSE)
loadfonts(device = "win")

#need to separate by full and trunc files

##### Visualizations for BP #####

## load data

exp_full_headphones <- read_csv(here("data_frames/exp_full_headphones.csv")) %>% 
  mutate(resp = case_when(
    response == "b" ~ 0,
    response == "p" ~ 1 ))

exp_trunc_headphones <- read_csv(here("data_frames/exp_trunc_headphones.csv")) %>% 
  mutate(resp = case_when(
    response == "b" ~ 0,
    response == "p" ~ 1 ))

# create proficiency order
proficiency <- exp_full_headphones %>% group_by(jatos_id, proficiency_diff) %>% summarize(n = n()) %>% select(-n) %>% arrange(proficiency_diff) %>% rowid_to_column("proficiency_order")

exp_full_headphones <- exp_full_headphones %>% 
  left_join(proficiency, by = c("jatos_id", "proficiency_diff"))

exp_trunc_headphones <- exp_trunc_headphones %>% 
  left_join(proficiency, by = c("jatos_id", "proficiency_diff"))

## set color palette & text sizes

# green and purple
pal1 <- c("#006d2c", "#810f7c")

# red and blue
pal2 <- c("#e41a1c", "#377eb8")

# red and blue for CRBLM poster
pal_crblm <- c("#B7455B", "#607B9E")

# increase font size for posters
text_poster <-   theme(legend.text = element_text(size = 18),
                       legend.title = element_text(size = 20),
                       axis.text = element_text(size = 18),
                       axis.title = element_text(size = 20),
                       axis.title.y = element_text(margin = margin(r = 20)),
                       plot.title = element_text(size = 22, hjust = 0.5))

# increase font size, make font white, change font
text_poster_white <-   theme(legend.text = element_text(size = 18, color = "white", family = "Fauna One"),
                       legend.title = element_text(size = 20, color = "white", family = "Fauna One"),
                       axis.text = element_text(size = 18, color = "white", family = "Fauna One"),
                       axis.title = element_text(size = 20, color = "white", family = "Fauna One"),
                       axis.title.y = element_text(margin = margin(r = 20)),
                       plot.title = element_text(size = 22, hjust = 0.5, color = "white", family = "Fauna One"))

# make background transparent, must be accompanied by bg = "transparent" in ggsave
transparent_background <-   theme(plot.background = element_rect(fill = "transparent", color = NA),
                            legend.background = element_rect(fill = "transparent", color = NA))


##### FULL BLOCKS #####

### PROPORTION OF /P/ RESPONSES BY: 

# language
exp_full_headphones %>% 
  ggplot(aes(x = VOT, y = resp, color = language)) +
  stat_summary(fun.data = "mean_cl_boot", size = 1) +
  stat_summary(fun.data = "mean_cl_boot", geom = 'line', size = 1) +
  scale_color_manual(values = pal2, labels = c("English", "French"), name = "Language") +
  labs(y = "Proportion of /p/ responses") +
  ggtitle("Full Words")

ggsave(here("figures/full_language.png"), width = 8.4, height = 6.9)

# language by individual participant
exp_full_headphones %>% 
  ggplot(aes(x = VOT, y = resp, color = language)) +
  stat_summary(fun.data = "mean_cl_boot") +
  stat_summary(fun.data = "mean_cl_boot", geom = 'line') +
  scale_color_manual(values = pal1, labels = c("English", "French"), name = "Language") +
  labs(y = "Proportion of /p/ responses") +
  geom_hline(yintercept = 0.5) +
  facet_wrap(. ~ proficiency_order)

# language - CRBLM poster, red/blue palette, transparent background, white text
exp_full_headphones %>% 
  ggplot(aes(x = VOT, y = resp)) +
  stat_summary(fun.data = "mean_cl_boot", size = 1, aes(shape = language)) +
  stat_summary(fun.data = "mean_cl_boot", geom = 'line', size = 1, aes(linetype = language)) +
  scale_shape_manual(values = c(19, 15), labels = c("English", "French"), name = "Language") +
  scale_linetype_manual(values = c("solid", "longdash"), labels = c("English", "French"), name = "Language") +
  labs(y = "Proportion of /p/ responses") +
  text_poster_white +
  transparent_background

ggsave("figures/language_CRBLM.png", width = 8.4, height = 6.9, dpi = 500, bg = "transparent")

# language (color) and target onset (facet)
exp_full_headphones %>% 
  ggplot(aes(x = VOT, y = resp, color = language)) +
  stat_summary(fun.data = "mean_cl_boot", size = 1) +
  stat_summary(fun.data = "mean_cl_boot", geom = 'line', size = 1) +
  scale_color_manual(values = pal2, name = "Language", labels = c("English", "French")) +
  facet_grid(target_onset ~ .) +
  labs(y = "Proportion of /p/ responses") +
  ggtitle("Full Words")

# target onset (color) and language (facet)
exp_full_headphones %>% 
  ggplot(aes(x = VOT, y = resp, color = target_onset)) +
  stat_summary(fun.data = "mean_cl_boot", size = 1) +
  stat_summary(fun.data = "mean_cl_boot", geom = 'line', size = 1) +
  scale_color_manual(values = pal1, name = "Target Onset") +
  facet_grid(. ~ language, labeller = as_labeller(c("en" = "English", "fr" = "French"))) +
  labs(y = "Proportion of /p/ responses") +
  ggtitle("Full Words")

ggsave(here("figures/full_lanuage_onset.png"), width = 8.4, height = 6.9)

# target onset language - CRBLM poster, red/blue palette, transparent background, white text
exp_full_headphones %>% 
  ggplot(aes(x = VOT, y = resp, color = target_onset)) +
  stat_summary(fun.data = "mean_cl_boot", size = 1) +
  stat_summary(fun.data = "mean_cl_boot", geom = 'line', size = 1) +
  scale_color_manual(values = pal_crblm, labels = c("/b/", "/p/"), name = "Original Onset") +
  facet_grid(. ~ language, labeller = as_labeller(c("en" = "English", "fr" = "French"))) +
  labs(y = "Proportion of /p/ responses") +
  text_poster_white +
  transparent_background +
  theme(strip.text.x = element_text(size = 18, family = "Fauna One"))
  
ggsave("figures/language-onset_CRBLM.png", width = 8.4, height = 6.9, dpi = 500, bg = "transparent")


# English ability

exp_full_headphones_rounded <- exp_full_headphones %>% 
  mutate(rounded_ability_English = as.factor(round(overall_ability_English, digits = 0)),
         rounded_ability_French = as.factor(round(overall_ability_French, digits = 0)))

pal_en_ability <- c("#41ae76", "#238b45", "#006d2c", "#00441b")

ggplot(exp_full_headphones_rounded, aes(x = VOT, y = resp)) +
  stat_summary(fun = "mean", geom = 'point', aes(color = rounded_ability_English)) +
  stat_summary(fun.data = "mean_cl_boot", geom = 'line', aes(color = rounded_ability_English), linetype="dashed")+
  scale_color_manual(values = pal_en_ability) +
  stat_summary(fun = "mean", geom = 'point', size = 3) +
  stat_summary(fun = "mean", geom = 'line', size = 1.3) +
  labs(y = "Proportion of /p/ responses") +
  facet_grid(. ~ target_onset)


# French ability

pal_fr_ability <- c("#9ebcda", "#8c96c6", "#8c6bb1", "#88419d", "#810f7c", "#4d004b")

ggplot(exp_full_headphones_rounded, aes(x = VOT, y = resp)) +
  stat_summary(fun = "mean", geom = 'point', aes(color = rounded_ability_French)) +
  stat_summary(fun.data = "mean_cl_boot", geom = 'line', aes(color = rounded_ability_French), linetype="dashed")+
  scale_color_manual(values = pal_fr_ability) +
  stat_summary(fun = "mean", geom = 'point', size = 3) +
  stat_summary(fun = "mean", geom = 'line', size = 1.3)+
  labs(y = "Proportion of /p/ responses") +
  facet_grid(. ~ target_onset)



# by individual words

fr <- exp_full_headphones %>% 
  filter(language == "fr") %>% 
  mutate(word = case_when(
    target_word == "apple" ~ "pomme (apple)",
    target_word == "candy" ~ "bonbon (candy)",
    target_word == "cookie" ~ "biscuit (cookie)",
    target_word == "doll" ~ "poupée (doll)",
    target_word == "foot" ~ "pied (foot)",
    target_word == "mouth" ~ "bouche (mouth)"
  ))

en <- exp_full_headphones %>% 
  filter(language == "en") %>% 
  mutate(word = target_word)

pal <- c("#006d2c", "#2ca25f", "#66c2a4", "#99d8c9", "#810f7c", "#8856a7", "#8c96c6", "#b3cde3")

pal_crblm_words <- c("#B7455B", "#660619", "#C7304E", "#F28DA1", "#607B9E", "#1B3454", "#3D91FF", "#7AA5DE")

# French by word with average by target onset
ggplot(fr, aes(x = VOT, y = resp)) +
  stat_summary(fun = "mean", geom = 'point', aes(color = word)) +
  stat_summary(fun.data = "mean_cl_boot", geom = 'line', aes(color = word), linetype = "dashed") +
  scale_color_manual(values = pal, name = "Full Word") +
  stat_summary(fun = "mean", geom = 'point', aes(color = target_onset), size = 3) +
  stat_summary(fun = "mean", geom = 'line', aes(color = target_onset), size = 1.3) +
  labs(y = "Proportion of /p/ responses")

# English by word with average by target onset
ggplot(en, aes(x = VOT, y = resp)) +
  stat_summary(fun = "mean", geom = 'point', aes(color = word)) +
  stat_summary(fun.data = "mean_cl_boot", geom = 'line', aes(color = word), linetype = "dashed") +
  scale_color_manual(values = pal, name = "Full Word") +
  stat_summary(fun = "mean", geom = 'point', aes(color = target_onset), size = 3) +
  stat_summary(fun = "mean", geom = 'line', aes(color = target_onset), size = 1.3) +
  labs(y = "Proportion of /p/ responses")


# French by word with average by target onset - CRBLM, transparent, white text
ggplot(fr, aes(x = VOT, y = resp)) +
  stat_summary(fun = "mean", geom = 'point', aes(color = word, shape = word), size = 2, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = 'line', aes(color = word), linetype = "dashed", size = .75, alpha = 0.5) +
  stat_summary(fun = "mean", geom = 'point', aes(color = target_onset, shape = target_onset), size = 3.6) +
  stat_summary(fun = "mean", geom = 'line', aes(color = target_onset), size = 1.5) +
  scale_shape_manual(values = c(19,15,15,15,19,15,15,15),
                     name = "Original Onset \nand Individual Words", 
                     labels = c("**/b/**", "biscuit (cookie)", "bonbon (candy)", "bouche (mouth)", "**/p/**", "pied (foot)", "pomme (apple)", "poupée (doll)")) +
  scale_color_manual(values = pal_crblm_words, 
                     name = "Original Onset \nand Individual Words", 
                     labels = c("**/b/**", "biscuit (cookie)", "bonbon (candy)", "bouche (mouth)", "**/p/**", "pied (foot)", "pomme (apple)", "poupée (doll)")) +
  labs(y = "Proportion of /p/ responses") +
  text_poster_white +
  transparent_background +
  theme(legend.text = element_markdown()) +
  ggtitle("French")

ggsave("figures/French-onset_CRBLM2.png", width = 10, height = 6.9, dpi = 500, bg = "transparent")


# English by word with average by target onset
ggplot(en, aes(x = VOT, y = resp)) +
  stat_summary(fun = "mean", geom = 'point', aes(color = word, shape = word), size = 2, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = 'line', aes(color = word), linetype = "dashed", size = .75, alpha = 0.5) +
  stat_summary(fun = "mean", geom = 'point', aes(color = target_onset, shape = target_onset), size = 3.6) +
  stat_summary(fun = "mean", geom = 'line', aes(color = target_onset), size = 1.5) +
  scale_shape_manual(values = c(19,15,15,15,19,15,15,15),
                     name = "Original Onset \nand Individual Words", 
                     labels = c("**/b/**", "bird", "book", "bunny", "**/p/**", "pen", "pillow", "puppy")) +
  scale_color_manual(values = pal_crblm_words, 
                     name = "Original Onset \nand Individual Words", 
                     labels = c("**/b/**", "bird", "book", "bunny", "**/p/**", "pen", "pillow", "puppy")) +
  labs(y = "Proportion of /p/ responses") +
  text_poster_white +
  transparent_background +
  theme(legend.text = element_markdown()) +
  ggtitle("English")


ggsave("figures/English-onset_CRBLM2.png", width = 10, height = 6.9, dpi = 500, bg = "transparent")



##### TRUNCATED BLOCKS #####

### PROPORTION OF /P/ RESPONSES BY: 

# language
exp_trunc_headphones %>% 
  ggplot(aes(x = VOT, y = resp, color = language)) +
  stat_summary(fun.data = "mean_cl_boot", size = 1) +
  stat_summary(fun.data = "mean_cl_boot", geom = 'line', size = 1) +
  scale_color_manual(values = pal2, labels = c("English", "French"), name = "Language") +
  labs(y = "Proportion of /p/ responses") +
  ggtitle("Truncated Words")

ggsave(here("figures/trunc_language.png"), width = 8.4, height = 6.9)

# language by individual participant
exp_trunc_headphones %>% 
  ggplot(aes(x = VOT, y = resp, color = language)) +
  stat_summary(fun.data = "mean_cl_boot") +
  stat_summary(fun.data = "mean_cl_boot", geom = 'line') +
  scale_color_manual(values = pal1, labels = c("English", "French"), name = "Language") +
  labs(y = "Proportion of /p/ responses") +
  geom_hline(yintercept = 0.5) +
  facet_wrap(. ~ proficiency_order)

# language (color) and target onset (facet)
exp_trunc_headphones %>% 
  ggplot(aes(x = VOT, y = resp, color = language)) +
  stat_summary(fun.data = "mean_cl_boot", size = 1) +
  stat_summary(fun.data = "mean_cl_boot", geom = 'line', size = 1) +
  scale_color_manual(values = pal2, name = "Language", labels = c("English", "French")) +
  facet_grid(target_onset ~ .) +
  labs(y = "Proportion of /p/ responses") +
  ggtitle("Truncated Words")


# target onset (color) and language (facet)
exp_trunc_headphones %>% 
  ggplot(aes(x = VOT, y = resp, color = target_onset)) +
  stat_summary(fun.data = "mean_cl_boot", size = 1) +
  stat_summary(fun.data = "mean_cl_boot", geom = 'line', size = 1) +
  scale_color_manual(values = pal1, name = "Target Onset") +
  facet_grid(. ~ language, labeller = as_labeller(c("en" = "English", "fr" = "French"))) +
  labs(y = "Proportion of /p/ responses") +
  ggtitle("Truncated Words")

ggsave(here("figures/trunc_language_onset.png"), width = 8.4, height = 6.9)


# individual words

fr_trunc <- exp_trunc_headphones %>% 
  filter(language == "fr") %>% 
  mutate(word = case_when(
    target_word == "apple" ~ "pomme (apple)",
    target_word == "candy" ~ "bonbon (candy)",
    target_word == "cookie" ~ "biscuit (cookie)",
    target_word == "doll" ~ "poupée (doll)",
    target_word == "foot" ~ "pied (foot)",
    target_word == "mouth" ~ "bouche (mouth)"
  ))

en_trunc <- exp_trunc_headphones %>% 
  filter(language == "en") %>% 
  mutate(word = target_word)

# French by word with average by target onset
ggplot(fr_trunc, aes(x = VOT, y = resp)) +
  stat_summary(fun = "mean", geom = 'point', aes(color = word)) +
  stat_summary(fun.data = "mean_cl_boot", geom = 'line', aes(color = word), linetype = "dashed") +
  scale_color_manual(values = pal, name = "Truncated Word") +
  stat_summary(fun = "mean", geom = 'point', aes(color = target_onset), size = 3) +
  stat_summary(fun = "mean", geom = 'line', aes(color = target_onset), size = 1.3) +
  labs(y = "Proportion of /p/ responses")

# English by word with average by target onset
ggplot(en_trunc, aes(x = VOT, y = resp)) +
  stat_summary(fun = "mean", geom = 'point', aes(color = word)) +
  stat_summary(fun.data = "mean_cl_boot", geom = 'line', aes(color = word), linetype = "dashed") +
  scale_color_manual(values = pal, name = "Truncated Word") +
  stat_summary(fun = "mean", geom = 'point', aes(color = target_onset), size = 3) +
  stat_summary(fun = "mean", geom = 'line', aes(color = target_onset), size = 1.3) +
  labs(y = "Proportion of /p/ responses")




