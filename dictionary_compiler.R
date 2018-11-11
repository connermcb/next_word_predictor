# This script creates of dictionary of all unique tokens expected 
# from the cleaning process that uses the `quanteda` package. It's difficult
# to load the text data using the bucketter script in batches unless there 
# is a existent unique token dictionary. 

library(readr)
library(quanteda)
library(hunspell)
library(tcltk)

# load sample data
paths <- c("en_US_blogs.txt", "en_US_news2.txt", "en_US_twitter.txt")
setwd("c:/users/conner/jhopkins_data_science/capstone/final/english_us/")

# instantiate unique tokens dictionary
dict <- c()

for( path in paths ){
        
        print(paste("Now processing", path))
        
        # load file
        print("Reading file")
        text_vec <- read_file(file = path) 
        print("Read file complete")
        
        # tokenize entire document by words, complete all cleaning (see args) in this step
        print("Tokenizing")
        tokens_vec <- tokens(text_vec, what = "word", remove_numbers = TRUE,
                           remove_punct = TRUE, remove_symbols = TRUE, remove_separators = TRUE,
                           remove_twitter = TRUE, remove_url = TRUE, verbose = TRUE)
        print("Tokenization complete")
        
        print("Cleaning and getting unique tokens")
        
        # flatten tokenized text and change vector class to character from token
        tokens_vec <- as.character(tokens_vec[[1]])
        
        # clean up and make space in memory
        rm(text_vec)
        gc()
        
        # set case to lower and reduce vector to unique tokens for efficiency in later steps
        tokens_vec <- tolower(tokens_vec)
        tokens_vec <- unique(tokens_vec)
        
        # check spelling and remove tokens not in hunspell dictionary
        spell_check <- hunspell_check(tokens_vec)
        tokens_vec <- tokens_vec[spell_check]
        
        # clean up and free memory
        rm(spell_check)
        gc()
        
        # filter out one-letter 'words' except for I and a
        one_word_test <- sapply(tokens_vec, function(tkn){nchar(tkn) > 1 | tkn %in% c('i', 'a')})
        tokens_vec <- tokens_vec[one_word_test]
        
        print("Cleaning complete")
        
        # add tokens from present file to dictionary
        print("Compiling dictionary")
        dict <-  union(dict, tokens_vec)
        print("Dictionary compilation complete")
        print("")
        
        # clean up and open memory
        rm(tokens_vec)
        gc()
}

save(dict, file = "unique_tokens_dict.RData")