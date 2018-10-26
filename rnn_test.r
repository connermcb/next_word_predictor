library(keras)
library(readr)
library(stringr)
library(purrr)
library(tokenizers)

# Parameters --------------------------------------------------------------

maxlen <- 40

# Data Preparation --------------------------------------------------------

# Retrieve text
path <- get_file(
        'nietzsche.txt', 
        origin=
)

# Load, collapse, and tokenize text
text <- read_lines('https://s3.amazonaws.com/text-datasets/nietzsche.txt', n_max = 1000) %>%
        str_to_lower() %>%
        str_c(collapse = "\n") %>%
        tokenize_characters(strip_non_alphanum = FALSE, simplify = TRUE)

print(sprintf("corpus length: %d", length(text)))

chars <- text %>%
        unique() %>%
        sort()

print(sprintf("total chars: %d", length(chars)))  

# Cut the text in semi-redundant sequences of maxlen characters
dataset <- map(
        seq(1, length(text) - maxlen - 1, by = 3), 
        ~list(sentece = text[.x:(.x + maxlen - 1)], next_char = text[.x + maxlen])
)

dataset <- transpose(dataset)

# Vectorization
x <- array(0, dim = c(length(dataset$sentece), maxlen, length(chars)))
y <- array(0, dim = c(length(dataset$sentece), length(chars)))

for(i in 1:length(dataset$sentece)){
        
        x[i,,] <- sapply(chars, function(x){
                as.integer(x == dataset$sentece[[i]])})
        
        y[i,] <- as.integer(chars == dataset$next_char[[i]])
        
}



model <- keras_model_sequential()

model %>%
        layer_lstm(128, input_shape = c(maxlen, length(chars))) %>%
        layer_dense(length(chars)) %>%
        layer_activation("softmax")

optimizer <- optimizer_rmsprop(lr = 0.01)

model %>% compile(
        loss = "categorical_crossentropy", 
        optimizer = optimizer
)


