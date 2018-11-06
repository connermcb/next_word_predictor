### Function for preparing cleaned data for RNN model

# packages
library("readr")
library("stringr")
library("stringi")
library("mxnet")

# set working directory
setwd("c:/users/conner/jhopkins_data_science/capstone/final/")

prepData <- function(path, seq_len=5, dict=NULL){
        
        # load data
        text_vec <- read_file(file = path)
        text_vec <- gsub('[\\r\\n]+', ' ', text_vec, perl = T)
        text_vec <- strsplit(text_vec, ' ') %>% unlist()
        
        # create dictionary of unique tokens
        dict <- unique(text_vec)
        
        # ennumerate tokens
        names(dict) <- 1:length(dict)
        
        # reverse dictionary
        rev_dict <- names(dict)
        names(rev_dict) <- dict
        
        num_seq <- (length(text_vec) - 1) %/% seq_len
        
        features <- dict[text_vec[1:(seq_len * num_seq)]]
        labels <- dict[text_vec[1:(seq_len * num_seq) + 1]]
        
        features_array <- array(features, dim = c(seq_len, num_seq))
        labels_array <- array(labels, dim = c(seq_len, num_seq))
        
        return (list(features_array = features_array,
                     labels_array = labels_array,
                     dict = dict, rev_dict = rev_dict))
        
        
}