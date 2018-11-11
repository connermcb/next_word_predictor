# This script is to prepare data that has already been organized into buckets by number of
# tokens into a format that can be used by the bucketter function from the mxnet package and 
# fed into the mxnet rnn model.

# load dictionaries
load("unique_tokens_dict.RData")
load("reverse_token_dict.RData")

# function for reading data from environment hashtable and making it digestable by RNN batcher
prepareBatches <- function( hashtable, dict=dict, rev_dict=rev_dict, train_val_fraction=0.9 ){
        # get prepared hashtable data
        load( hashtable )
        
        train_buckets <- list()
        eval_buckets <- list()

        for( bkt in names(hashtable)){
                df <- as.data.frame(do.call(rbind, buckets[[bkt]]))
                X <- df[1:(nrow(df) - 1)]
                Y <- df[2:nrow(df)]
                
                samples <- nrow(df)
                
                X_train_data <- X[1:as.integer(samples * train_val_fraction), ]
                X_val_data <- X[-(1:as.integer(samples * train_val_fraction)), ]
                
                X_train_label <- Y[1:as.integer(samples * train_val_fraction), ]
                X_val_label <- Y[-(1:as.integer(samples * train_val_fraction)), ]
                
                train_buckets <- append(train_buckets, 
                                        bkt = list(data = X_train_data,
                                                    label = X_train_label))
                
                eval_buckets <- append(eval_buckets,
                                       bkt = list(data = X_val_data,
                                                 label = X_val_label))
                
                
        }
        return( list(train=train_buckets,
                     eval=eval_buckets
                     ) 
                )
}

vocab <- length(dict)