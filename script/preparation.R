library(tidyverse)
library(cld2)

# Read & combine data ----
# read the data 
og_df_bumble <- read_csv("data/Bumble_submissions.csv")
og_df_hinge <- read_csv("data/hingeapp_submissions.csv")
og_df_tinder <- read_csv("data/Tinder_submissions.csv")
og_df_tinderstories <- read_csv("data/tinderstories_submissions.csv")
og_df_datingapps <- read_csv("data/DatingApps_submissions.csv")
og_df_onlinedating <- read_csv("data/OnlineDating_submissions.csv")

# add the subreddit name to the data
og_df_bumble$subreddit <- "Bumble"
og_df_hinge$subreddit <- "hingeapp"
og_df_tinder$subreddit <- "Tinder"
og_df_tinderstories$subreddit <- "tinderstories"
og_df_datingapps$subreddit <- "DatingApps"
og_df_onlinedating$subreddit <- "OnlineDating"

# combine the dataset
list <- list(og_df_bumble, og_df_datingapps, og_df_hinge, og_df_onlinedating, og_df_tinder, og_df_tinderstories)
og_df_combined <- bind_rows(list)

# Basic data preparation ----

# check duplicates in the text
length(og_df_combined$text)
length(unique(og_df_combined$text))

df_combined = 
  og_df_combined %>% 
  filter(year(created) > 2022) %>% # subset the df to posts after 2023 (less impact of COVID-19)
  filter(text != "[deleted]" & text != "[removed]") %>% # remove the removed/deleted posts
  select(title, score, created, text, subreddit) %>% # drop the irrelevant columns
  unique() # drop the duplicates

# check if there are posts that's not written in English
df_combined$language <- detect_language(df_combined$text)
table(df_combined$language)

# check if the language detection was correct by examining the non-English posts
df_combined %>%
  mutate(language = detect_language(text)) %>%
  filter(language != "en") %>%
  select(title, text) 

# remove the posts not written in English
df_combined = df_combined %>%
  mutate(language = detect_language(text)) %>%
  filter(language == "en") %>%
  select(title, score, created, text, subreddit)

# remove \n, \r in the texts
df_combined = df_combined %>%
  mutate(text = str_replace_all(text, "\\n", " ")) %>%
  mutate(text = str_replace_all(text, "\\r", " ")) %>%
  mutate(wordcount = str_count(text, '\\w+')) # add a raw word count

# check the distribution of word count
summary(df_combined$wordcount)

# remove the posts containing less than 10 words
df_combined = df_combined %>%
  filter(wordcount > 9)

summary(df_combined$wordcount)

# leads to 104702 obs., which will be the basis of the analysis
# save this as a RDS file
saveRDS(df_combined, "working_data/reddit_combined_v1.RDS")