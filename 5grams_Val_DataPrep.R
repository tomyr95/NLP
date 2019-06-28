library(quanteda)
library(data.table)
library(readtext)
library(dplyr)
library(tidyr)
library(beepr)

setwd("C:/Users/tomyr/Documents/GitHub/NLP")

#Paths
docs_path    <- "/Users/tomyr/Documents/GitHub/NLP/validation"
dt_out_5     <- "/Users/tomyr/Documents/GitHub/NLP/output/Val_5grams.txt"

#Create Profanity Filter
profanity_file <- "/Users/tomyr/Documents/GitHub/NLP/wordlists/profanity.txt"
profanity <- as.character(read.table(profanity_file, sep = ",", header = FALSE)[, 1])

#Functions
  doSentences <- function(docs) {unlist(tokens(docs, what = "sentence"))}

  doTokens <- function(txt){
    txt <- as.character(txt)
    toks <- tokens(txt, what = "word",
                   remove_numbers = TRUE,
                   remove_punct = TRUE,
                   remove_symbols = TRUE,
                   remove_separators = TRUE,
                   remove_twitter = FALSE,
                   remove_hyphens = FALSE,
                   remove_url = TRUE)
    
    toks <- tokens_tolower(toks) %>%
      tokens_remove(valuetype = "regex", pattern = "\\d"          , padding = TRUE) %>%
      tokens_remove(valuetype = "regex", pattern = "^(?![a-z]).+" , padding = TRUE) %>%
      tokens_remove(valuetype = "regex", pattern = "^.*www.*$"    , padding = TRUE) %>%
      tokens_remove(valuetype = "regex", pattern = "\\.\\w{2,3}"  , padding = TRUE) %>%
      tokens_remove(valuetype = "regex", pattern = "^.*@.*$"      , padding = TRUE) %>%
      tokens_remove(valuetype = "regex", pattern = "^.*#.*$"      , padding = TRUE) %>%
      tokens_remove(valuetype = "regex", pattern = "rt"           , padding = TRUE) %>%
      tokens_remove(valuetype = "regex", pattern = "lol"          , padding = TRUE) %>%
      tokens_remove(valuetype = "regex", pattern = "(.)\\1{2}"    , padding = TRUE)
    
    return(toks)
  }
  
  #Read files / Create Corpus
    files <- readtext(docs_path, encoding = "UTF-8")
    docs <- corpus(files)
    rm(files)
    
  #Prepare Tokens
    sentences <- doSentences(docs); rm(docs)
    toks <- doTokens(sentences)

  #Prepare Ngrams (n = 5)
    toks_5 <- tokens(toks, ngrams = 5, concatenator = " "); rm(toks)
    toks_5 <- tokens_remove(toks_5, valuetype = "regex", pattern = "\\b(\\w+)\\s+\\1\\b")
    features <- dfm(toks_5); rm(toks_5)
    dt_val <- as.data.table(textstat_frequency(features)) %>% select(feature, frequency)
    dt_val <- dt_val %>% separate(col = feature, into = c("token", "tmp1", "tmp2", "tmp3", "nword"), sep = " ")
    dt_val <- dt_val %>% mutate(token = paste(token, tmp1, tmp2, tmp3, sep = " "))%>% select(-c(tmp1, tmp2, tmp3))
    write.table(dt_val, file = dt_out_5, sep = "\n", row.names = FALSE, col.names = FALSE)
    save(dt_val, file = dt_out_5)
    rm(features)
