library(quanteda)
library(newsmap)
library(data.table)
library(readtext)
library(RColorBrewer)
library(wordcloud)
library(ggplot2)
library(dplyr)
library(tidyr)
library(beepr)

strt <- Sys.time()

setwd("C:/Users/tomyr/Documents/GitHub/NLP")

Visualizations = TRUE
Cover = FALSE

Cut_1 = 3
Cut_2 = 3
Cut_3 = 3
Cut_4 = 3
Cut_5 = 3

#Paths
      docs_path    <- "/Users/tomyr/Documents/GitHub/NLP/training"
      en_dict_path <- "/Users/tomyr/Documents/GitHub/NLP/dictionary/words_alpha.txt"
      df_out_1     <- "/Users/tomyr/Documents/GitHub/NLP/output/df1.Rdata"
      df_out_2     <- "/Users/tomyr/Documents/GitHub/NLP/output/df2.Rdata"
      df_out_3     <- "/Users/tomyr/Documents/GitHub/NLP/output/df3.Rdata"
      df_out_4     <- "/Users/tomyr/Documents/GitHub/NLP/output/df4.Rdata"
      df_out_5     <- "/Users/tomyr/Documents/GitHub/NLP/output/df5.Rdata"
      
      if(Visualizations){
          wc_file_1    <- "/Users/tomyr/Documents/GitHub/NLP/png/cloud1.png"
          wc_file_2    <- "/Users/tomyr/Documents/GitHub/NLP/png/cloud2.png"
          wc_file_3    <- "/Users/tomyr/Documents/GitHub/NLP/png/cloud3.png"
          wc_file_4    <- "/Users/tomyr/Documents/GitHub/NLP/png/cloud4.png"
          wc_file_5    <- "/Users/tomyr/Documents/GitHub/NLP/png/cloud5.png"
          freq_file_1  <- "/Users/tomyr/Documents/GitHub/NLP/png/chart1.png"
          freq_file_2  <- "/Users/tomyr/Documents/GitHub/NLP/png/chart2.png"
          freq_file_3  <- "/Users/tomyr/Documents/GitHub/NLP/png/chart3.png"
          freq_file_4  <- "/Users/tomyr/Documents/GitHub/NLP/png/chart4.png"
          freq_file_5  <- "/Users/tomyr/Documents/GitHub/NLP/png/chart5.png"
      }
      
      if(Cover) Cover_File <- "/Users/tomyr/Documents/GitHub/NLP/output/Coverage.Rdata"
      
start_time <- Sys.time()
      
#Create Profanity Filter
    profanity_file <- "/Users/tomyr/Documents/GitHub/NLP/wordlists/profanity.txt"
    profanity <- as.character(read.table(profanity_file, sep = ",", header = FALSE)[, 1])
    
#Functions
      doSentences <- function(docs){unlist(tokens(docs, what = "sentence"))}
      
      doTokens <- function(docs){
            toks <- tokens(docs, what = "word",
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
            
            toks <- tokens_remove(toks, pattern = profanity, padding = TRUE)
      }

      if(Visualizations){
        wcloud_plot <- function(features, qty){
            suppressWarnings(
                  textplot_wordcloud(features, min_count = 10, random_order = FALSE, rotation = .25,
                        max_words = qty, color = RColorBrewer::brewer.pal(8,"Dark2"))
            )
        }
      
        freq_plot <- function(features, qty){
            x <- featnames(features)[1:qty]
            y <- colSums(features)[1:qty]
            data <- data.frame(x = x, y = y)
            data %>% arrange(y) %>% mutate(x=factor(x,x)) %>%
            ggplot(aes(x=x, y=y)) + geom_segment(aes(x=x, xend=x, y=0, yend=y), color = "light blue", size=1) +
            geom_point( color="magenta", size=2, alpha=0.6) + theme_light() + coord_flip() +
            theme(panel.grid.major.y = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
                  axis.ticks.y = element_blank()) + xlab("") + ylab("freq")
        }
      }

#Read files / Create Corpus
   files <- readtext(docs_path, encoding = "UTF-8")
   docs <- corpus(files)
   rm(files)
   
#Prepare Tokens
   sentences <- doSentences(docs)
   toks <- doTokens(sentences)
   rm(profanity)
   rm(sentences)
   rm(docs)
   
#ngrams = 1
      features <- dfm(toks)
      features <- dfm_sort(features, decreasing = TRUE)
      features <- dfm_trim(features, min_termfreq = Cut_1)

      if(Cover){
           Freq            <- as.integer(colSums(features))
           Freq_Unique     <- as.integer(unique(Freq))
           Freq_Unique_Qty <- length(Freq_Unique)
           FofF <- integer()
           for(i in 1:Freq_Unique_Qty) FofF[i] = Freq_Unique[i] * length(which(Freq == Freq_Unique[i]))
           FofF_Cum   <- cumsum(FofF)
           FofF_Total <- sum(colSums(features))
           F_Percent  <- FofF_Cum / FofF_Total * 100
           FREQ <- data.table(Freq_Unique, FofF, FofF_Cum, F_Percent)
           cutoff <- min(which(F_Percent > Coverage))
           Cut_1 <- as.integer(FREQ[cutoff, 1])
           save(FREQ, file = Cover_File)
           rm(FREQ); rm(Freq); rm(F_Percent, FofF, FofF_Cum, Freq_Unique)
      }
           
           if(Visualizations){
              png(wc_file_1, units="in", width=5, height=5, res=300)
              wcloud_plot(features, 350)
              dev.off()
              
              png(freq_file_1, units="in", width=5, height=5, res=300)
              freq_plot(features, 30)
              dev.off()
           }

      #Save Frequency Table
            dt <- as.data.table(textstat_frequency(features))[,.(feature, frequency)]
            dt <- dt[head(which(dt[, 1]!=""), 50),]
            save(dt, file = df_out_1)
            
#ngrams = 2            
      toks_2 <- tokens(toks, ngrams = 2, concatenator = " ")
      toks_2 <- tokens_remove(toks_2, valuetype = "regex", pattern = "\\b(\\w+)\\s+\\1\\b")
      features <- dfm(toks_2); rm(toks_2)
      features <- dfm_sort(features, decreasing = TRUE)
      features <- dfm_trim(features, min_termfreq = Cut_2)

      if(Visualizations){
            png(wc_file_2, units="in", width=5, height=5, res=300)
            wcloud_plot(features, 250)
            dev.off()
            png(freq_file_2, units="in", width=5, height=5, res=300)
            freq_plot(features, 30)
            dev.off()
      }
            
      #Save Frequency Table
            dt <- as.data.table(textstat_frequency(features))[,.(feature, frequency)]
            dt <- dt[, c("token", "nword") := tstrsplit(feature, " ", fixed = TRUE)]; dt[, feature:=NULL]
            dt <- dt %>% select(-frequency, frequency)
            save(dt, file = df_out_2)    

#ngrams = 3       
      toks_3 <- tokens(toks, ngrams = 3, concatenator = " ")
      toks_3 <- tokens_remove(toks_3, valuetype = "regex", pattern = "\\b(\\w+)\\s+\\1\\b")
      features <- dfm(toks_3); rm(toks_3)
      features <- dfm_sort(features, decreasing = TRUE)
      features <- dfm_trim(features, min_termfreq = Cut_3)
    
      if(Visualizations){
            png(wc_file_3, units="in", width=5, height=5, res=300)
            wcloud_plot(features, 200)
            dev.off()
            png(freq_file_3, units="in", width=5, height=5, res=300)
            freq_plot(features, 30)
            dev.off()
      }
            
      #Save Frequency Table
            dt <- as.data.table(textstat_frequency(features))[,.(feature, frequency)]
            dt <- dt[, c("token", "tmp1", "nword") := tstrsplit(feature, " ", fixed = TRUE)]
            dt <- dt %>% mutate(token = paste(token, tmp1, sep = " ")) %>% select(-tmp1)
            dt <- dt %>% select(-feature, -frequency, frequency)
            save(dt, file = df_out_3)
            
#ngrams = 4
      toks_4 <- tokens(toks, ngrams = 4, concatenator = " ")
      toks_4 <- tokens_remove(toks_4, valuetype = "regex", pattern = "\\b(\\w+)\\s+\\1\\b")
      features <- dfm(toks_4); rm(toks_4)
      features <- dfm_sort(features, decreasing = TRUE)
      features <- dfm_trim(features, min_termfreq = Cut_4)
     
      if(Visualizations){
            png(wc_file_4, units="in", width=5, height=5, res=300)
            wcloud_plot(features, 200)
            dev.off()
            png(freq_file_4, units="in", width=5, height=5, res=300)
            freq_plot(features, 30)
            dev.off()
      }
            
      #Save Frequency Table
            dt <- as.data.table(textstat_frequency(features))[,.(feature, frequency)]
            dt <- dt[, c("token", "tmp1", "tmp2", "nword") := tstrsplit(feature, " ", fixed = TRUE)]
            dt <- dt %>% mutate(token = paste(token, tmp1, tmp2, sep = " ")) %>% select(-c(tmp1, tmp2))
            dt <- dt %>% select(-feature, -frequency, frequency)
            save(dt, file = df_out_4) 
            
#ngrams = 5
      toks_5 <- tokens(toks, ngrams = 5, concatenator = " ")
      toks_5 <- tokens_remove(toks_5, valuetype = "regex", pattern = "\\b(\\w+)\\s+\\1\\b")
      features <- dfm(toks_5); rm(toks_5)
      features <- dfm_sort(features, decreasing = TRUE)
      features <- dfm_trim(features, min_termfreq = Cut_5)
      
      if(Visualizations){
            png(wc_file_5, units="in", width=5, height=5, res=300)
            wcloud_plot(features, 150)
            dev.off()
            png(freq_file_5, units="in", width=5, height=5, res=300)
            freq_plot(features, 30)
            dev.off()
      }
      
      #Save Frequency Table
            dt <- as.data.table(textstat_frequency(features))[,.(feature, frequency)]
            dt <- dt[, c("token", "tmp1", "tmp2", "tmp3", "nword") := tstrsplit(feature, " ", fixed = TRUE)]
            dt <- dt %>% mutate(token = paste(token, tmp1, tmp2, tmp3, sep = " "))%>% select(-c(tmp1, tmp2, tmp3))
            dt <- dt %>% select(-feature, -frequency, frequency)
            save(dt, file = df_out_5)
            
rm(features)
rm(toks)
rm(dt)


stp <- Sys.time()
stp - strt

beep(8)
