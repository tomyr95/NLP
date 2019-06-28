library(LaF)                  # fast access to large ASCII files 


file1 <- "/Users/tomyr/Documents/GitHub/NLP/dataset/en_US.blogs.txt"
file2 <- "/Users/tomyr/Documents/GitHub/NLP/dataset/en_US.news.txt"
file3 <- "/Users/tomyr/Documents/GitHub/NLP/dataset/en_US.twitter.txt"

file.info(c(file1, file2, file3))$size

len1 <- length(readLines(file1))
len2 <- length(readLines(file2))
len3 <- length(readLines(file3))

size1 <- file.info(file1)$size
size2 <- file.info(file2)$size
size3 <- file.info(file3)$size

set.seed <- 100
sample_percent <- .10
file1_sub <- sample_lines(file1, len1 * sample_percent, nlines = NULL)
file2_sub <- sample_lines(file2, len2 * sample_percent, nlines = NULL)
file3_sub <- sample_lines(file3, len2 * sample_percent, nlines = NULL)

fileConn<-file("subs/en_US.blogs_sub.txt")
      writeLines(file1_sub, fileConn)
close(fileConn)

fileConn<-file("subs/en_US.news_sub.txt")
      writeLines(file2_sub, fileConn)
close(fileConn)

fileConn<-file("subs/en_US.twitter_sub.txt")
      writeLines(file3_sub, fileConn)
close(fileConn)

rm(file1_sub, file2_sub, file3_sub)