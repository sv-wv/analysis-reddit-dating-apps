library(quanteda)
library(textplots)
library(tidyverse)

# Data wrangling ----

## lemmatization
# with textstem package, tokenize, tokens, types are masked so don't forget to specify when calling respective functions!!!
# see how it works
# sample %>%
#   mutate(text_l = lemmatize_strings(text)) %>%
#   select(text_l)

df_l = df %>%
  mutate(title_text = paste(title, text, sep = " "), # combine title and text
         title_text = lemmatize_strings(title_text)) # lemmatization

# saveRDS(df_l, "save/df_lemmatized.rds")

## tokenize, remove stopwords 
toks = df_l %>%
  corpus(docid_field = "id", text_field = "title_text") # tokenize the lemmatized corpus
sum(ntoken(toks)) # the total number of tokens
mean(ntoken(toks)) # the mean number of tokens per document

toks = toks %>%
  quanteda::tokens(remove_punct = T,
                   remove_numbers = T,
                   remove_url = T) %>%
  tokens_select(min_nchar = 2, max_nchar = 20) %>% # set maximum length to 20 to filter parts of urls
  tokens_select(pattern = stopwords("en"), selection = "remove") # remove stopwords 

# see what kind of tokens remain that needs to be excluded
head(toks)

# exclude age, gender, also remaining parts of urls
# toks[["17"]]
# 
# toks %>%
#   tokens_select("jpg") %>%
#   head(50)

pattern1 = "[:digit:][:digit:][MF]" # to detect age + language combination
pattern2 = c("x200B", "https", "jpg", "width", "amp", "format", "webp", "pjpg", "png", "redd", "reddit", 
             "www", "com", "auto", "guy", "man", "woman", "girl") # to remove parts of url that is not detected
# I also removed guy, man, woman, girl because it is obviously gendered in heterosexual setting

toks = toks %>%
  tokens_select(pattern1, selection = "remove", valuetype = "regex") %>%
  tokens_select(pattern2, selection = "remove")

# saveRDS(toks, "save/toks.rds")
sum(ntoken(toks))
summary(ntoken(toks))

dfm = toks %>% dfm() # make the document feature matrix