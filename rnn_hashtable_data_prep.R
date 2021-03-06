# This script is to prepare data that has already been organized into buckets by number of
# tokens into a format that can be used by the bucketter function from the mxnet package and 
# fed into the mxnet rnn model.


# function for reading data from environment hashtable and making it digestable by RNN batcher
prepareBatches <- function( hashtable, train_val_fraction=0.9 ){
        # get prepared hashtable data
        load( hashtable ) # loads as 'buckets'
        
        train_buckets <- list()
        eval_buckets <- list()

        for( bkt in names(buckets)){

                # created dataframe from list of vectors under bkt in env hashtable
                mtrx <- do.call(rbind, buckets[[bkt]])

                # test that bucket actually has sentences
                if( is.null(mtrx) ) next
                if( nrow(mtrx) < 2 ) next

                X <- mtrx[, 1:(as.integer(bkt) - 1)]
                Y <- mtrx[, 2:as.integer(bkt)]

                samples <- nrow(mtrx)
                
                X_train_data <- X[1:as.integer(samples * train_val_fraction), , drop = F]
                X_val_data <- X[-(1:as.integer(samples * train_val_fraction)), , drop = F]

                X_train_label <- Y[1:as.integer(samples * train_val_fraction), , drop = F]
                X_val_label <- Y[-(1:as.integer(samples * train_val_fraction)), , drop = F]

                # cope with single row matrices (temporary solution, information loss!)
                # TODO: find better solution for dealing with single line buckets.
                if( head(dim(X_train_data), 1) < 2) next

                # transpose - for some reason it looks like mxnet reads sentences as columns
                # instead of rows. Untransposed matrix causing error when mxnet tries to batch
                # by column instead of by row.
                X_train_data <- t(X_train_data)
                X_val_data <- t(X_val_data)
                X_train_label <- t(X_train_label)
                X_val_label <- t(X_val_label)
                
                # gather buckets in to train and eval lists
                train_buckets[[bkt]] <- list(data = X_train_data,
                                           label = X_train_label)

                eval_buckets[[bkt]] <- list(data = X_val_data,
                                          label = X_val_label)
                
        }
        return( list(train=train_buckets,
                     eval=eval_buckets
                     )
                )
}

# test <- prepareBatches("twitter_hashtable.RData")

