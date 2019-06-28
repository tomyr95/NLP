library(quanteda)
library(data.table)
library(tidyr)
library(dplyr)

Data <- character()

#Functions

   clean_txt <- function(txt){
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
   
   sBackoff <- function(igm){
      #Set Lambda
      Lambda <- c(0.4^0, 0.4^1, 0.4^2, 0.4^3)
      #Determine Input Ngram Counts & Multiplier
      Input_Counts <- integer()
      S <- numeric()
      Toks_Levels <- as.integer(max(igm$Toks))
      for(i in 1:max(Toks_Levels)){
         Input_Counts[i] <- sum(as.integer(igm$Freq[igm$Toks == i]))
      }
      for(i in 1:nrow(igm)){
         igm$InGramCount[i] <- Input_Counts[igm$Toks[i]]
         igm$Multiplier[i] <- Lambda[6 - igm$Toks[i]]
         igm$S[i] <- igm$Multiplier[i] * as.numeric(igm$Freq[i]) / igm$InGramCount[i]
      } 
      igm <- igm %>% arrange(desc(S)) 
      igm <- igm[match(unique(igm$NextW), igm$NextW),]
      return(igm)
   }
   
   GuessWord <- function(UserInput){
      cutoff = 10
      qty_results = 5
      UserInput <- as.character(UserInput)
      tokens <- clean_txt(UserInput)
      token_qty <- ntoken(tokens)
      igm <- data.table(Gram = character(), Toks = integer(), NextW = character(), Freq = numeric(), stringsAsFactors = FALSE)
      
      if(token_qty >= 4){
         toks = 5
         snippet <- tail(tokens[[1]], 4)
         snippet <- paste(snippet, collapse = " ")
         x <- head(as.character(dt5[dt5[, 1] == snippet, 2]), cutoff)
         y <- head(as.numeric(dt5[dt5[, 1] == snippet, 3]), cutoff)
         if(length(x)>0){igm <- rbind(igm, data.table( rep(snippet, length(x)), toks, x, y), use.names = FALSE)}
      }
      
      if(token_qty >= 3){
         toks = 4
         snippet <- tail(tokens[[1]], 3)
         snippet <- paste(snippet, collapse = " ")
         x <- head(as.character(dt4[dt4[, 1] == snippet, 2]), cutoff)
         y <- head(as.numeric(dt4[dt4[, 1] == snippet, 3]), cutoff)
         if(length(x)>0){igm <- rbind(igm, data.table( rep(snippet, length(x)), toks, x, y), use.names = FALSE)}
      }
      
      if(token_qty >= 2){
         toks = 3
         snippet <- tail(tokens[[1]], 2)
         snippet <- paste(snippet, collapse = " ")
         x <- head(as.character(dt3[dt3[, 1] == snippet, 2]), cutoff)
         y <- head(as.numeric(dt3[dt3[, 1] == snippet, 3]), cutoff)
         if(length(x)>0){igm <- rbind(igm, data.table( rep(snippet, length(x)), toks, x, y), use.names = FALSE)}
      }
      
      if(token_qty >= 1){
         toks = 2
         snippet <- tail(tokens[[1]], 1)
         snippet <- paste(snippet, collapse = " ")
         x <- head(as.character(dt2[dt2[, 1] == snippet, 2]), cutoff)
         y <- head(as.numeric(dt2[dt2[, 1] == snippet, 3]), cutoff)
         if(length(x)>0){igm <- rbind(igm, data.table( rep(snippet, length(x)), toks, x, y), use.names = FALSE)}
      }
      
      if(nrow(igm) > 0){
         igm <- sBackoff(igm)
         igm <- head(igm, qty_results)
         result <- data.frame(NextWord = igm[, 3], sScore = igm[, 7], nGram = as.integer(igm[, 2]))
         result <- result %>% mutate(sScore = format(sScore, digits = 3))
      }
      
      
      if(nrow(igm) == 0){
         result <- dt1[1:5, 1]
      }    
      
      return(result)
   }
   
   
#Load n-gram Data Tables
   
      file1 <- "df1.Rdata"
      file2 <- "df2.Rdata"
      file3 <- "df3.Rdata"
      file4 <- "df4.Rdata"
      file5 <- "df5.Rdata"
      
      load(file1); dt1 <- as.data.frame(dt)
      load(file2); dt2 <- as.data.frame(dt)
      load(file3); dt3 <- as.data.frame(dt)
      load(file4); dt4 <- as.data.frame(dt)
      load(file5); dt5 <- as.data.frame(dt)
      
      Base <- data.frame(NextWord = dt1[1:5, 1])
      
      rm(dt)
      