# test samples
ex_titles <- c(" 22F ", "4.99", "$80", "23/F, ", "FALSE", "M 23", "60%", "50-60%", "[22F]", "(29M) 38", "22(F)", "[F] 22", "F ", "Female", "logic", "False", "date")
ex_titles_extracted = str_extract(ex_titles, "[MF][^A-z]")
str_extract(ex_titles_extracted, "[MF]")
str_extract(ex_titles, "[^$\\.][:digit:][:digit:][^-%]")

df_combined_gender_age = df_combined %>%
  mutate(title_text = paste(" ", title, text, sep = " "),
         gender1 = str_extract(title_text, "[MF][^A-z]"),
         gender = str_extract(gender1, "[MF]"),
         age1 = str_extract(title_text, "[^$\\.][:digit:][:digit:][^-%\\.+]"),
         age = str_extract(age1, "[:digit:][:digit:]")) %>%
  select(title, text, subreddit, wordcount, gender) # select more 

# keep only the posts that has a extracted gender
df = df_combined_gender_age %>%
  drop_na(gender)

table(df$gender)
table(df$subreddit)

# generate an id column for convenience 
id = c(1:19744)
df = cbind(id, df)

# trying to filter misgendered posts
this_girl = df %>%
  filter(str_detect(text, "this girl") & gender == "F") %>%
  select(id, title, text)
# some manual coding 
list_m = c(15, 123, 239, 299, 379, 439, 467,
           482, 500, 675, 923, 1779, 3317, 3769, 4799, 5762, 6019, 
           7362, 8081, 8201, 9024, 9209, 9447, 9461, 10295, 10410, 10432, 11824, 12538, 13566, 
           13687, 13728, 16342, 16362, 16554, 16694, 17041, 19637)
df$gender[df$id %in% list_m] = "M"

# check
df$gender[df$id == 9024]

this_guy = df %>%
  filter(str_detect(text, "this guy") & gender == "M") %>%
  select(id, title, text)

list_f = c(124, 152, 762, 949, 970, 1033, 1039, 1083, 1158, 1209, 1266, 1683, 2103, 2109, 2388, 2647, 2843, 2857,
           3114, 3428, 4024, 4156, 4254, 4269, 4286, 4436, 4443, 4517, 4564, 4633, 4675, 4700, 4890, 5113,
           5120, 5257, 5292, 5860, 5910, 6023, 6039, 6181, 6489, 7101, 7296, 7352, 7375, 7380, 7458, 7574, 7604,
           7731, 7815, 7883, 7991, 8134, 8487, 9754, 9783, 9945, 10017, 10027, 10129, 10545, 10606, 10755,
           10955, 11154, 11946, 12042, 12646, 12729, 12941, 13762, 14257, 14652, 14728, 15495, 15653, 15701,
           15839, 16085, 16159, 16242, 16481, 16627, 16630, 16647, 16737, 16859, 16983, 17186, 17228, 17323,
           17672, 17851, 18263, 18281, 18813, 19221, 19236, 19274, 19383, 19466, 19490, 19636)
df$gender[df$id %in% list_f] = "F"

# check
df$gender[df$id == 152]

