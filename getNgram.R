


library(dplyr)
library(stringr)
library(igraph)
library(ggraph)
library(ggplot2)

getBiGrams <- function(filePath, tgthr = FALSE, prcntFile){
        print(filePath)
        # read file
        lines2table <- read.table(filePath, quote = "",
                                  sep = "\n", col.names = c("lines")) 
        
        # reduce file size
        tbl <- lines2table %>%
                sample_n(size = round(nrow(lines2table) * prcntFile))
        
        # calculate bigrams
        bigramCount <- tbl %>%
                unnest_tokens(bigram, lines, token = "ngrams", n = 2) %>%
                separate(bigram, c("word1", "word2"), sep = " ") %>%
                filter(!word1 %in% stop_words$word,
                       !word2 %in% stop_words$word) %>%
                unite(bigram, word1, word2, sep = " ")

        
        # add file name to dataframe
        fName <- gsub("^[^_]*_([^_]*)_.*", "\\1", filePath)
        bigramCount <- cbind(fName, bigramCount)
        
        # rename cols
        names(bigramCount) <- c("filename", "bigram")
        
        return(bigramCount)
}


getTriGrams <- function(tbl, tgthr = FALSE){
        # read file
        lines2table <- read.table(filePath, quote = "",
                                  sep = "\n", col.names = c("lines")) 
        
        # reduce file size
        tbl <- lines2table %>%
                sample_n(size = round(nrow(lines2table) * prcntFile))
        
        # calculate trigrams
        trigramCount <- tbl %>%
                unnest_tokens(trigram, lines, token = "ngrams", n = 3) %>%
                separate(bigram, c("word1", "word2", "word3"), sep = " ") %>%
                filter(!word1 %in% stop_words$word,
                       !word2 %in% stop_words$word,
                       !word3 %in% stop_words$word) %>%
                count(word1, word2, word3, sort = TRUE)
        
        # tie words in trigram into single string
        if (tgthr == TRUE){
                trigramCount <- trigramCount %>%
                        unite(trigram, word1, word2, word3, sep = " ")
        }
        
        return(trigramCount)
}