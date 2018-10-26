library(dplyr)


tblLoader <- function(p){
        print(p)
        tbl <- read.table(p, sep = "\n", quote = "", 
                          stringsAsFactors = F)        
        tbl[2] <- sub("^[^_]*_([^_]*)_.*", "\\1", p, perl = T)
        names(tbl) <- c("text", "file")
        tblCounts <- tbl %>%
                unnest_tokens(word, text) %>%
                filter(!word %in% stop_words$word) %>%
                count(file, word, sort = TRUE) %>%
                ungroup()
        
        return(tblCounts[1:20,])
}

wordFreqs <- lapply(cleanedPaths[1:3], tblLoader)
freqTbl <- as.data.frame(wordFreqs)[c(2,3, 5,6, 8,9)]
names(freqTbl) <- rep(c("word", "n ="), 3)
write.csv(freqTbl, file = "file_word_freqs.csv")
save(freqTbl, file = "fileWordFreqs.R")

# paths to cleaned files
cleanedPaths <- c("english_news_squeaky_clean.txt", 
                  "english_twitter_squeaky_clean.txt",
                  "english_blogs_squeaky_clean.txt")

