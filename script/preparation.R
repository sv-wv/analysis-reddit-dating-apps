library(tidyverse)

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
