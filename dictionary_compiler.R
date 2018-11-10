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

