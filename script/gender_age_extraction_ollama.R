library(rollama)
library(tidyverse)

# Testing ollama (llama3.1) in extracting gender ----

# https://jbgruber.github.io/rollama/articles/annotation.html#the-make_query-helper-function

# start ollama
rollama::ping_ollama()
pull_model()
show_model()

# creating sample to test
ollama_sample = df_combined %>%
  slice_sample(n = 10) %>%
  select(title, text) %>%
  as.data.frame()
ollama_sample

# save the sample (because I forgot to set seed before)
# saveRDS(ollama_sample, "working_data/ollama_sample.rds") 

# add manually coded gender
sample_gender = list(NA, 0, 0, 1, 1, NA, 0, NA, NA, NA) # 0 for women, 1 for men
ollama_sample$gender = sample_gender

# ollama_sample = readRDS("working_data/ollama_sample.rds")

# Prepare classification task using make_query
sample_queries <- make_query(
  text = ollama_sample,
  prompt = "Categories: 0, 1, NA",
  template = "{prefix}{text}\n{prompt}",
  system = "Classify the gender of the writer of given reddit posts.
  In the post, M means male, W or F means female.
  Please only classify it if the writer explicitly state their gender.
  For the output, 0 is female, 1 is male. If nothing is given in the post, put NA.
  Answer with just the correct category.",
  prefix = "Text to classify: "
)

# when i give it ollama_sample, it just tries to predict every cell separately
# so i have to combine the title and the text to test again bcs it's sometimes in the title, sometimes in the text

# combining the title and main body
ollama_sample = ollama_sample %>%
  mutate(title_text = paste(title, text, sep = " "))

# Prepare classification task using make_query
sample_queries <- make_query(
  text = ollama_sample$title_text,
  prompt = "Categories: 0, 1, NA",
  template = "{prefix}{text}\n{prompt}",
  system = "Classify the gender of the writer of given reddit posts.
  In the post, M means male, W or F means female.
  Please only classify it if the writer explicitly state their gender.
  For the output, 0 is female, 1 is male. If nothing is given in the post, put NA.
  Answer with just the correct category.",
  prefix = "Text to classify: "
)

# Apply the classification
est_gender = rollama::query(sample_queries, screen = FALSE, output = "text")

ollama_sample$est_gender = est_gender
ollama_sample %>% select(est_gender, gender)

# 1st est_gender:  [1] "1"  "0"  "0"  "1"  "1"  "NA" "0"  "0"  "NA" "0"
# 2st est_gender after revising the prompt: 100% same'

# doing the exact same thing with a different sample
# creating sample to test
ollama_sample_2 = df_combined %>%
  slice_sample(n = 10) %>%
  mutate(title_text = paste(title, text, sep = " ")) %>%
  select(title_text) %>%
  as.data.frame()
ollama_sample_2

sample_gender_2 = list(0, NA, NA, 1, 1, 0, NA, 0, 0, NA) # 0 for women, 1 for men

ollama_sample_2$gender = sample_gender_2

# saveRDS(ollama_sample_2, "working_data/ollama_sample_2.rds")
# ollama_sample = readRDS("working_data/ollama_sample.rds")

# Prepare classification task using make_query
sample_queries_2 <- make_query(
  text = ollama_sample_2$title_text,
  prompt = "Categories: 0, 1, NA",
  template = "{prefix}{text}\n{prompt}",
  system = "Classify the gender of the writer of given reddit posts.
  In the post, M means male, W or F means female.
  Please only classify it if the writer explicitly state their gender.
  For the output, 0 is female, 1 is male. If nothing is given in the post, put NA.
  Answer with just the correct category.",
  prefix = "Text to classify: "
)

# Apply the classification
est_gender_2.2 = query(sample_queries_2, screen = FALSE, output = "text")
est_gender_2
sample_gender
ollama_sample_2$est_gender_2 = est_gender_2.2
ollama_sample_2 %>% select(gender, est_gender, est_gender_2)

# consistent with only one error (it's still 10%...)
# I forgot to save the LLM estimated version here (reproducibility issues...)


# Containing 'other' genders ----

ollama_sample_nb = df_combined %>%
  filter(grepl("NB", text)) %>% # I filtered the posts containing NB (could be a short form of non-binary, but could be also other things)
  slice_sample(n=20) %>%
  select(title, text) %>%
  mutate(title_text = paste(title, text, sep = " ")) %>%
  as.data.frame()

sample_gender_nb = list(2, 2, 0, NA, NA, NA, 0, NA, NA, 1, 0, NA, 0, NA, 1, NA, 1, 1, 0, NA) # 0 for women, 1 for men, 2 for non-binary or other genders
ollama_sample_nb$gender = sample_gender_nb

sample_queries_nb <- make_query(
  text = ollama_sample_nb$title_text,
  prompt = "Categories: 0, 1, 2, NA",
  template = "{prefix}{text}\n{prompt}",
  system = "Classify the gender of the writer of given reddit posts.
  In the post, M means male, W or F means female. NB would mean non-binary.
  It would be typically included in the beginning, also with the age of the person.
  Please only classify it if the writer explicitly state their gender.
  For the output, 0 is female, 1 is male, 2 is other. If nothing is given in the post, put NA.
  Answer with just the correct category.",
  prefix = "Text to classify: "
)

est_gender_nb = query(sample_queries_nb, screen = FALSE, output = "text")
est_gender_nb
# with this, the output wasn't clear and contained unnecessary characters other than 0, 1, 2, NA.
# recode some of the answers containing extra notes
est_gender_nb[14] = 2
est_gender_nb[1] = 2
est_gender_nb[17] = 1

sample_gender
ollama_sample_nb$est_gender = est_gender_nb
ollama_sample_nb %>% select(est_gender, gender)
# OK so clearly this model doesn't understand the concept of non-binary

saveRDS(ollama_sample_nb, "working_data/ollama_sample_nb.rds")
ollama_sample_nb %>%
  select(gender, est_gender) %>%
  saveRDS("share/ollama_sample_nb_estimated.rds")


# Testing qwen3:0.6b ----
# start ollama
rollama::ping_ollama()
pull_model("qwen3:0.6b")
show_model()

ollama_sample = ollama_sample %>%
  mutate(title_text = paste(title, text, sep = " "))

# Prepare classification task using make_query
sample_queries <- make_query(
  text = ollama_sample$title_text,
  prompt = "Categories: 0, 1, NA",
  template = "{prefix}{text}\n{prompt}",
  system = "Classify the gender of the writer of given reddit posts.
  In the post, M means male, W or F means female.
  The statement of gender could be at the very beginning, or after the first person referencing (e.g. I, me, my)
  Please only classify it if the writer explicitly state their gender.
  For the output, 0 is female, 1 is male. If nothing is given in the post, put NA.
  Answer with just the correct category.",
  prefix = "Text to classify: "
)

# Apply the classification
est_gender = rollama::query(sample_queries, model = "qwen3:0.6b", screen = FALSE, output = "text")
ollama_sample$est_gender = est_gender
ollama_sample %>% select(est_gender, gender)

est_gender2 = rollama::query(sample_queries, model = "qwen3:0.6b", screen = FALSE, output = "text")
ollama_sample$est_gender2 = est_gender2
ollama_sample %>% select(gender, est_gender, est_gender2)

est_gender3 = rollama::query(sample_queries, model = "qwen3:0.6b", screen = FALSE, output = "text")
ollama_sample$est_gender3 = est_gender3
ollama_sample %>% select(gender, est_gender, est_gender2, est_gender3)

est_gender4 = rollama::query(sample_queries, model = "qwen3:0.6b", stream = T, output = "text")
ollama_sample$est_gender4 = est_gender4

# could also add the accuracy score and write a loop to repeat this without having to write everything every time 
# also the conclusion... why I decided not to use LLMs

saveRDS(ollama_sample, "working_data/ollama_sample_estimated.rds")
ollama_sample %>% select(gender, est_gender, est_gender2, est_gender3, est_gender4) %>%
  saveRDS("share/ollama_sample_estimated.rds")

