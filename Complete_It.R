library(quanteda)
library(data.table)
library(tidyr)
library(dplyr)


setwd("C:/Users/tomyr/Documents/GitHub/NLP")

#Functions

   clean_txt <- function(txt){
      txt <- as.character(txt)
      toks <- tokens(txt, remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, 
                     remove_separators = TRUE, remove_twitter = TRUE, remove_url = TRUE,
                     remove_hyphens = TRUE)
      toks <- tokens_tolower(toks)
      toks <- tokens_remove(toks, valuetype = "regex", pattern = "\\d")
      toks <- tokens_remove(toks, valuetype = "regex", pattern = "^(?![a-z]).+")
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
         result <- data.frame(Next_Word = igm[, 3], S_Score = igm[, 7])
      }
      
      
      if(nrow(igm) == 0){
         result <- dt1[1:5, 1]
      }   
      
      return(result)
   }
   
#Load n-gram Data Tables
      file1 <- "/Users/tomyr/Documents/GitHub/NLP/output/df1.Rdata"
      file2 <- "/Users/tomyr/Documents/GitHub/NLP/output/df2.Rdata"
      file3 <- "/Users/tomyr/Documents/GitHub/NLP/output/df3.Rdata"
      file4 <- "/Users/tomyr/Documents/GitHub/NLP/output/df4.Rdata"
      file5 <- "/Users/tomyr/Documents/GitHub/NLP/output/df5.Rdata"
      
      load(file1); dt1 <- as.data.frame(dt)
      load(file2); dt2 <- as.data.frame(dt)
      load(file3); dt3 <- as.data.frame(dt)
      load(file4); dt4 <- as.data.frame(dt)
      load(file5); dt5 <- as.data.frame(dt)
      rm(dt)
            
#Get/Clean User Input
      UserInput <- ""
      result <- GuessWord(UserInput)
      result
