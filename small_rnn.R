## Small-scale test of neural network approach

# packages and source files
library("readr")
library("stringr")
library("stringi")
library("mxnet")
source("rnn_data_prep.R")  # source file for prepData()

# set working directory
setwd("c:/users/conner/jhopkins_data_science/capstone/final/")

# assign args for prepData()
path <- "blog_sample.txt"
seq_len <- 10

# get prepared data
prepdData <- prepData(path = path, seq_len = seq_len, dict = NULL)

X <- prepdData$features_array
Y <- prepdData$labels_array

dict <- prepdData$dict
rev_dict <- prepdData$rev_dict

vocab <- length(dict)

samples <- tail(dim(X), 1)

train_val_fraction <- 0.9

X_train_data <- X[, 1:as.integer(samples * train_val_fraction)]
X_val_data <- X[, -(1:as.integer(samples * train_val_fraction))]

X_train_label <- Y[, 1:as.integer(samples * train_val_fraction)]
X_val_label <- Y[, -(1:as.integer(samples * train_val_fraction))]

train_buckets <- list("10" = list(data = X_train_data,
                                  label = X_train_label))

eval_buckets <- list("10" = list(data = X_val_data,
                                 label = X_val_label))

train_buckets <- list(buckets = train_buckets, 
                      dict = dict, rev_dict = rev_dict)

eval_buckets <- list(buckets = eval_buckets,
                     dict = dict, rev_dict = rev_dict)



# create iterators for training and evaluation datasets
vocab <- length(eval_buckets$dict)

batch_size <- 32

train_data <- mx.io.bucket.iter(buckets = train_buckets$buckets,
                                batch.size = batch_size, 
                                data.mask.element = 0, shuffle = TRUE)

eval_data <- mx.io.bucket.iter(buckets = eval_buckets$buckets,
                               batch.size = batch_size,
                               data.mask.element = 0, shuffle = TRUE)


# create model architecture
rnn_graph_one2one <- rnn.graph(num_rnn_layer = 10,
                               num_hidden = 7,
                               input_size = vocab,
                               num_embed = 64,
                               num_decode = vocab,
                               ignore_label = 0,
                               cell_type = "lstm",
                               masking = F,
                               output_last_state = T,
                               loss_output = "softmax",
                               config = "one-to-one")

# visualization of unrolled RNN architecture
graph.viz(rnn_graph_one2one, type = "graph", direction = "LR",
          graph.height.px = 180, shape = c(100,64))

# initialize RNN network
devices <- mx.cpu()
initializer <- mx.init.Xavier(rnd_type = "gaussian",
                              factor_type = "avg",
                              magnitude = 3)

optimizer <- mx.opt.create("adadelta", rho = 0.9,
                           eps = 1e-5, wd = 1e-8,
                           clip_gradient = 5, 
                           rescale.grad = 1/batch_size)

logger <- mx.metric.logger()

epoch_callback <- mx.callback.log.train.metric(period = 1, logger = logger)
batch_callback <- mx.callback.log.train.metric(period = 50)

# prepare state callback
mx.metric.custom_nd <- function(name, feval){
        init <- function(){
                c(0, 0)
        }
        update <- function(label, pred, state) {
                m <- feval(label, pred)
                state <- c(state[[1]] + 1, state[[2]] + m)
                return(state)
        }
        get <- function(state){
                list(name = name, 
                     value = (state[[2]] / state[[1]]))
        }
        ret <- (list(init = init, update = update, get = get))
        class(ret) <- "mx.metric"
        return(ret)
}

mx.metric.Perplexity <- mx.metric.custom_nd("Perplexity", function(label, pred){
        label <- mx.nd.reshape(label, shape = -1)
        label_probs <- as.array(mx.nd.choose.element.0index(pred, label))
        batch <- length(label_probs)
        NLL <- -sum(log(pmax(13-15, as.array(label_probs)))) / batch
        Perplexity <- exp(NLL)
        return(Perplexity)
})

# train model
model <- mx.model.buckets(symbol = rnn_graph_one2one,
                          train.data = train_data, 
                          eval.data = eval_data,
                          num.round = 20, ctx = devices,
                          verbose = T, metric = mx.metric.Perplexity,
                          initializer = initializer, 
                          optimizer = optimizer,
                          batch.end.callback = NULL,
                          epoch.end.callback = epoch_callback)


mx.model.save(model, prefix = "one2one_seq_model", iteration = 20)

