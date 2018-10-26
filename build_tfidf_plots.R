# load pkgs
library(dplyr)
library(stringr)
library(igraph)
library(ggraph)
library(ggplot2)

# set working directory
getwd()
setwd("c:/users/conner/jhopkins_data_science/capstone/final/")

# paths to cleaned files
cleanedPaths <- c("english_twitter_squeaky_clean2.txt",
                  "english_news_squeaky_clean.txt", 
                  "english_blogs_squeaky_clean.txt")

# set sample size
prcntFile <- 0.5  # set portion of file to keep

# get n-gram counts for all files in single dataframe
source('getNgram.R')

# get full dataframe of bigram counts for all three files
bigramAllFiles <- lapply(cleanedPaths, 
                         FUN=getBiGrams, tgthr = TRUE, prcntFile = prcntFile)
bigramAllFiles <- data.frame(Reduce(rbind, bigramAllFiles))


# get tf_idf stats for bigrams
tf_idf <- bigramAllFiles %>%
        count(filename, bigram) %>%
        bind_tf_idf(bigram, filename, n) %>%
        arrange(filename, desc(tf_idf)) 

save(tf_idf, file = "tf_idf.RData")

ggplot(tf_idf[1:10,]) +
        geom_bar(aes(x=bigram, y=tf_idf), stat = "identity") +
        coord_flip() +
        facet_wrap( ~ filename, ncol=1) 

load("tf_idf.RData")
tweets10 <- tf_idf[tf_idf$filename == 'twitter',][1:25,]
tweets10 <- tweets10 %>% arrange(desc(tf_idf))
tweets10$bigram <- factor(tweets10$bigram, levels = tweets10$bigram)

ggplot(tweets10) + 
        geom_bar(aes(x=bigram, y = tf_idf), stat = "identity") + 
        coord_flip()