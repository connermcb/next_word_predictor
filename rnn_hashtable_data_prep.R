# This script is to prepare data that has already been organized into buckets by number of
# tokens into a format that can be used by the bucketter function from the mxnet package and 
# fed into the mxnet rnn model.


# function for reading data from environment hashtable and making it digestable by RNN batcher
prepareBatches <- function( hashtable, train_val_fraction=0.9 ){
        # get prepared hashtable data
        load( hashtable_name ) # loads as 'buckets'
        
        train_buckets <- list()
        eval_buckets <- list()

        for( bkt in names(buckets)){
                df <- as.data.frame(do.call(rbind, buckets[[bkt]]))
                
                # test that bucket actually has sentences
                if( nrow(df) == 0 ) next
                
                X <- df[, 1:(as.integer(bkt) - 1)]
                Y <- df[, 2:as.integer(bkt)]
                
                samples <- nrow(df)
                
                X_train_data <- X[1:as.integer(samples * train_val_fraction), ]
                X_val_data <- X[-(1:as.integer(samples * train_val_fraction)), ]
                
                X_train_label <- Y[1:as.integer(samples * train_val_fraction), ]
                X_val_label <- Y[-(1:as.integer(samples * train_val_fraction)), ]
                
                train_buckets[bkt] <- list(data = X_train_data,
                                           label = X_train_label)
                
                eval_buckets[bkt] <- list(data = X_val_data,
                                          label = X_val_label)
                
                print(length(train_buckets))
                
        }
        return( list(train=train_buckets,
                     eval=eval_buckets
                     ) 
                )
}

# prepareBatches("twitter_hashtable.RData")