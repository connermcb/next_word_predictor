## Small-scale test of neural network approach

# packages
library("readr")
library("stringr")
library("stringi")
library("mxnet")

# set working directory
setwd("c:/users/conner/jhopkins_data_science/capstone/final/")

# load data
text_vec <- read_file(file="blog_sample.txt")
text_vec <- gsub('[\\r\\n]+', ' ', text_vec, perl = T)

# create dictionary of unique tokens
dict <- unlist(strsplit(text_vec, ' '))
dict <- unique(dict)

# ennumerate tokens
names(dict) <- 1:length(dict)

# reverse dictionary
rev_dict <- names(dict)
names(rev_dict) <- dict
