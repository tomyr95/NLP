library(quanteda)
library(data.table)
library(readtext)
library(dplyr)
library(tidyr)
library(beepr)

setwd("C:/Users/tomyr/Documents/GitHub/NLP")

#Paths
      data_file <- "/Users/tomyr/Documents/GitHub/NLP/output/Val_5grams.txt"

#Functions
      
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
        cutoff = 5
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
          igm <- head(igm, 3)
          result <- igm[, 1]
        }
        
        
        if(nrow(igm) == 0){
          result <- "NO MATCH FOUND"
        }   
        
        return(result)
      }
      
#Load n-gram Data Tables
    file2 <- "/Users/tomyr/Documents/GitHub/NLP/output/df2.Rdata"
    file3 <- "/Users/tomyr/Documents/GitHub/NLP/output/df3.Rdata"
    file4 <- "/Users/tomyr/Documents/GitHub/NLP/output/df4.Rdata"
    file5 <- "/Users/tomyr/Documents/GitHub/NLP/output/df5.Rdata"
    
    load(file2); dt2 <- as.data.frame(dt)
    load(file3); dt3 <- as.data.frame(dt)
    load(file4); dt4 <- as.data.frame(dt)
    load(file5); dt5 <- as.data.frame(dt)
      
#Load/Sample 5grams
   load(data_file)
   qty = 10
   set.seed(123)
   sample <- sample.int(nrow(dt_val), qty, replace = FALSE)
   
#Run Validation
   t1 <- Sys.time()
   
      Grams <- as.list(dt_val[sample, 1])
      Words <- as.list(dt_val[sample, 2])
      GuessWords <- lapply(Grams, GuessWord)
      Matches <- integer() 

      for(i in 1:qty){
        Matches[i] <- match(Words[[i]], GuessWords[[i]], nomatch = 0)
      }  
        
      match_1 <- length(Matches[Matches == 1])
      match_2 <- length(Matches[Matches == 2])
      match_3 <- length(Matches[Matches == 3])
      match_0 <- length(Matches[Matches == 0])
      percent <- sum(match_1, match_2, match_3) / qty * 100
      
      Unmatched <- sample[which(Matches == 0)]
      Unmatched <- dt_val[Unmatched, ]

      print(match_1)
      print(match_2)
      print(match_3)
      print(match_0)
      print(percent)
      
   t2 <- Sys.time()
   print(t2-t1)
   