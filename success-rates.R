################################################################################
# I calculate the success rates for each team here
# Author: Stephan Huber
################################################################################
rm(list = ls())
library("tidyverse")
setwd("/home/sthu/Dropbox/hsf/22-ws/dsb/projects/frese/DataScience/")

# import 
dta <- read_csv("WorldCupShootouts (1).csv")

# cleaning
dta <- dta %>% 
  rename_with(.,tolower) %>%  # I don't remember upper cases
  rename(i = game_id) %>%     # g stands for game
  rename(g = goal) %>%     # t stands for tor
  rename(c = team) %>%    # c stands for country
  relocate(i, g) %>% 
  select(i, g, c) %>%         # that are the variables of interest for me
  arrange(c, g) %>% 
  na.omit()

# I do everything within the tibble knowing that I create some redundancies
dta <- dta %>% 
  group_by(c) %>%
  drop_na(c, g) %>% # would not be neccessary as we already dropped all NAs
  add_count() %>% 
  rename(c_count = n) %>%   # count of countries 
  mutate(cg_count = sum(g, na.rm = TRUE)) %>% 
  mutate(success_rate = cg_count/c_count) %>% 
  ungroup()

# save the success_rate in an object
sr <- dta %>% 
  group_by(c) %>%
  summarise(mean(success_rate)) %>% 
  ungroup()

