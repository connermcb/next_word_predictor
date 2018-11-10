# This script creates of dictionary of all unique tokens expected 
# from the cleaning process that uses the `quanteda` package. It's difficult
# to load the text data using the bucketter script in batches unless there 
# is a existant unique token dictionary. 

library(readr)
library(quanteda)
library(hunspell)
library(tcltk)

# load sample data
paths <- c("en_US_blogs.txt", "en_US_news2.txt", "en_US_twitter.txt")
setwd("c:/users/conner/jhopkins_data_science/capstone/final/english_us/")
text_vec <- read_file(file = path)

# instantiate unique tokens dictionary
dict -> c()

for( path in paths){
        
        # load file
        text_vec <- read_file(file = path) 
        
        # tokenize entire document by words, complete all cleaning (see args) in this step
        tokens_vec <- tokens(text_vec, what = "word", remove_numbers = TRUE,
                           remove_punct = TRUE, remove_symbols = TRUE, remove_separators = TRUE,
                           remove_twitter = TRUE, remove_url = TRUE, verbose = TRUE)

        # flatten tokenized text and change vector class to character from token
        tokens_vec <- as.character(unlist(tokens))

        # clean up and make space in memory
        rm(text_vec)
        gc()
        
        # check spelling and remove tokens not in hunspell dictionary
        spell_check <- hunspell_check(tokens_vec)
        tokens_vec <- tokens_vec[spell_check]

        tokens_vec <- unique(tokens_vec)
        
        dict <-  union(dict, tokens_vec)        
}