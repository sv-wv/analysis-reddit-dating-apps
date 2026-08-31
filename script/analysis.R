library(quanteda)
library(quanteda.textmodels)
library(quanteda.textstats)
library(quanteda.textplots)
library(tidytext)
library(tidyverse)
library(textstem)

# Top Features ----

topfeatures(dfm, 10, decreasing = TRUE) # showing the top features
# topfeatures(dfm, 100, decreasing = FALSE)

# bi- and trigrams
toks_bi = toks %>%
  tokens_ngrams(n = 2, concatenator = "_") # create bigram tokens
dfm_bi = toks_bi %>% dfm() # create the data frequency matrix with bigram tokens
topfeatures(dfm_bi, 10, decreasing = TRUE) # bigram top features

toks_tri = toks %>%
  tokens_ngrams(n = 3, concatenator = "_") # create trigram tokens
dfm_tri = toks_tri %>% dfm() # create the data frequency matrix with trigram tokens
topfeatures(dfm_tri, 10, decreasing = TRUE) # trigram top 10 features

# groups by gender
dfm = dfm %>%
  dfm_group(groups = gender)
dfm_bi = dfm_bi %>%
  dfm_group(groups = gender)

topfeatures(dfm, 10, groups = gender)
topfeatures(dfm_bi, 10, groups = gender) 

# Keyness analysis ----

# trim the dfm to only words that appear at least 10 times to make modeling more efficient
dfm_trimmed = dfm %>% 
  dfm_trim(min_termfreq = 10) 
# topfeatures(dfm_trimmed, 50, decreasing = F)

# keyness analysis (difference by gender)
keyness_uni = dfm_trimmed %>% 
  textstat_keyness() # calculate keyness statistics

# for the result table
keyness_uni_m = keyness_uni %>%
  tail(20) 

keyness_uni_f = keyness_uni %>%
  head(20)

keyness_uni_table = rbind(keyness_uni_f, keyness_uni_m)
keyness_uni_table$chi2 = round(keyness_uni_table$chi2, digits = 3) # round the chi2 and p values
keyness_uni_table$p = round(keyness_uni_table$p, digits = 3)
write.csv(keyness_uni_table, "table/keyness_uni.csv")

# plot the keyness statistics
keyness_uni_plot = keyness_uni %>% 
  textplot_keyness(
    n = 20,
    margin = 0.1,
    color = c("darkblue", "darkred"),
    labelsize = 4
  ) 

# save the graph file
ggsave(
  "plot/keyness_uni.png",
  plot = keyness_uni_plot,
  dpi = 300
)

# repeat with bigram

dfm_bi_trimmed = dfm_bi %>% 
  dfm_trim(min_termfreq = 10) 

keyness_bi = dfm_bi_trimmed %>% 
  textstat_keyness()

keyness_bi_m = keyness_bi %>%
  tail(20) 

keyness_bi_f = keyness_bi %>%
  head(20)

keyness_bi_table = rbind(keyness_bi_f, keyness_bi_m)
keyness_bi_table$chi2 = round(keyness_bi_table$chi2, digits = 3)
keyness_bi_table$p = round(keyness_bi_table$p, digits = 3)
write.csv(keyness_bi_table, "table/keyness_bi.csv")

keyness_bi_plot = keyness_bi %>% 
  textplot_keyness(
    n = 20,
    margin = 0.17,
    color = c("darkblue", "darkred"),
    labelsize = 4
  )

ggsave(
  "plot/keyness_bi.png",
  plot = keyness_bi_plot,
  dpi = 300
)

# KWIC ----

# tokenize the original text without lemmatization for better understanding of contexts
pattern2.1 = c("x200B", "https", "jpg", "width", "amp", "format", "webp", "pjpg", "png", "redd", "reddit", 
               "www", "com", "auto") # left only the keywords completely irrelevant to interpretation
toks_og = df %>%
  mutate(title_text = paste(title, text, sep = " ")) %>%
  corpus(docid_field = "id", text_field = "title_text") %>%
  quanteda::tokens(remove_url = T) %>%
  tokens_select(max_nchar = 20) %>% # to make single-character tokens remain
  tokens_select(pattern2.1, selection = "remove")

# subset the tokens 
toks_f <- toks_og[docvars(toks, "gender") == "F"]
toks_m <- toks_og[docvars(toks, "gender") == "M"]

# a function for sampling kwic
sample_kwic <- function(gender, keyword) {
  if (gender == "M") toks = toks_m
  else toks = toks_f
  
  kw = kwic(
    toks,
    pattern = keyword,
    window = 10
  )
  set.seed(927) # set seed for reproducibility
  kw %>%
    slice_sample(n = 10)
}

# calling the function and saving it to a variable (or .csv file)
# considering the effect of lemmatization, I tried to use every possible forms of verbs
kwic_f_bff = sample_kwic("F", "bff")
kwic_f_bff 
write.csv(kwic_f_bff, "table/kwic_f_bff.csv") 
kwic_f_say = sample_kwic("F", c("say", "says", "said", "told", "tell", "talk"))
kwic_f_say 
write.csv(kwic_f_say, "table/kwic_f_say.csv")
kwic_f_sex = sample_kwic("F", "sex")
kwic_f_sex
write.csv(kwic_f_sex, "table/kwic_f_sex.csv")

kwic_m_match = sample_kwic("M", c("match", "matches"))
kwic_m_match
write.csv(kwic_m_match, "table/kwic_m_match.csv")
kwic_m_prompt = sample_kwic("M", "prompt")
kwic_m_prompt
write.csv(kwic_m_prompt, "table/kwic_m_prompt.csv")
kwic_m_appreciate = sample_kwic("M", c("appreciate", "appreciated", "appreciates"))
kwic_m_appreciate
write.csv(kwic_m_appreciate, "table/kwic_m_appreciate.csv")


# compound the token patterns found in bigram analysis
toks_f = toks_f %>%
  tokens_compound(
    pattern = phrase(c("don't want", "doesn't want", "didn't want", "love bomb", 
                       "don't like", "doesn't like", "didn't like",
                       "wants relationship", "wants sex", "doesn't know",
                       "want relationship", "want sex", "don't know",
                       "wanted relationship", "wanted sex", "didn't know",
                       "wants a relationsip", "want a relationship", "wanted a relationship"))
  )

toks_m = toks_m %>%
  tokens_compound(
    pattern = phrase(c("profile review", "use hinge", "used hinge",
                       "per week", "get much", "got much"))
  )

# repeat with the bigram tokens
kwic_f_dont_want = sample_kwic("F", c("don't_want", "doesn't_want", "didn't_want"))
kwic_f_dont_want
kwic_f_love_bomb = sample_kwic("F", c("love_bomb", "lovebomb")) ##
kwic_f_love_bomb
write.csv(kwic_f_love_bomb, "table/kwic_f_love_bomb.csv")
kwic_f_want_relationship = sample_kwic("F", c( "wants_relationship", "want_relationship", "wanted_relationship", 
                                               "wants_a_relationsip", "want_a_relationship", "wanted_a_relationship"))
kwic_f_want_relationship 
write.csv(kwic_f_want_relationship, "table/kwic_f_want_relationship.csv")
kwic_f_want_sex = sample_kwic("F", c("wants_sex", "want_sex", "wanted_sex"))
kwic_f_want_sex 
write.csv(kwic_f_want_sex, "table/kwic_f_want_sex.csv")

kwic_m_profile_review = sample_kwic("M", "profile_review")
kwic_m_profile_review
write.csv(kwic_m_profile_review, "table/kwic_m_profile_review.csv")
kwic_m_use_hinge = sample_kwic("M", c("use_hinge", "used_hinge"))
kwic_m_use_hinge
kwic_m_per_week = sample_kwic("M", "per_week")
kwic_m_per_week
write.csv(kwic_m_per_week, "table/kwic_m_per_week.csv")
kwic_m_get_much = sample_kwic("M", c("get_much", "got_much"))
kwic_m_get_much
write.csv(kwic_m_get_much, "table/kwic_m_get_much.csv")