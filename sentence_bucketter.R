## Script for sorting sentences into buckets by length, i.e. number of tokens

# packages
library(tidytext)
library(tokenizers)
library(readr)
library(quanteda)

# load sample data
setwd("c:/users/conner/jhopkins_data_science/capstone/final/english_us/")


# function for bucketting sentences by number of tokens
sentenceBucketter <- function(path){
        
        # read file
        text_vec <- read_file(file = path)        
        
        # split text file into sentences
        sent_vec <- tokens(text_vec, what = "sentence", remove_numbers = TRUE,
                           remove_punct = TRUE, remove_symbols = TRUE, remove_separators = TRUE,
                           remove_twitter = TRUE, remove_url = TRUE, verbose = TRUE)
        
        # clean up and make space in memory
        rm(text_vec)
        gc()
        
        # tokenize words by sentence, result is vector of vectors
        token_vec <- tokens(sent_vec[[1]], what = "word", verbose = TRUE,
                            remove_punct = TRUE, remove_numbers = TRUE,
                            remove_twitter = TRUE, remove_url = TRUE,
                            remove_symbols = TRUE, remove_separators = TRUE)
        
        # clean up and make space in memory
        rm(sent_vec)
        gc()
        
        # create dictionary of unique tokens
        dict <- c()
        
        # create hash table and and buckets named 1 to 100
        buckets <- new.env()
        
        for(bkt in as.character(seq_len(100))){
                assign(bkt, vector("list", length = 1000), envir = buckets)
        } 
        
        nxt_slot <- rep(1, 100)
        
        # sort sentences (tokenized by word) to buckets 
        # based on number of words, i.e. sentence length
        system.time(
        for(i in 1:100){
                if (i %% 10 == 0){
                        msg <- paste0(i, "% of sentences complete.")
                        print(msg)
                }
                #change class to character from token
                sentence <- token_list[[i]]
                
                # check spelling and remove tokens not in dictionary
                spell_check <- hunspell_check(sentence)
                sentence <- tolower(sentence[spell_check])
                
                sent_len <- length(sentence)
                
                if(sent_len < 4 | sent_len > 100) next
                
                # throw sentence in bucket
                rw <- nxt_slot[sent_len] # sent_len defines/indexes bucket
                nxt_slot[sent_len] <- (rw + 1)
                bkt <- as.character(bkt) # assign bucket based on n tokens
                buckets[[bkt]][[rw]] <- sentence
        })
        
        # name and save environment for use in 
        hashtable_name <- paste0(sub("en_US_([a-z0-9]+)\\.txt", "\\1", path), "_hashtable.RData")
        save(buckets, file = hashtable_name)
}

paths <- c("en_US_blogs.txt", "en_US_news2.txt", "en_US_twitter.txt")