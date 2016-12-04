lcy.classifier <- function(data, label, k=0, classifier='naiveBayes'){
    uni.label <- unique(label)
    num <- c()
    x <- matrix(numeric(0), nr=nrow(data))
    y   <- c()
    for(i in 1:length(uni.label)){
        num    <- c(num,sum(label == uni.label[i]))
        x    <- cbind(x, data[,label == uni.label[i],drop=FALSE])
        y   <- c(y,rep(as.character(uni.label[i]),num[i]))
    }
    return.value <- list()
    if (k <= 1){ # independent validation
        ncol            <- ncol(x)
        split           <- 1:ncol
        train           <- x[,split %in% seq(1,ncol,2)]
        train.label     <- y[split %in% seq(1,ncol,2)]
        testing         <- x[,split %in% seq(2,ncol,2)]
        testing.label   <- y[split %in% seq(2,ncol,2)]
        switch(classifier,
            naiveBayes={
                cf              <-naiveBayes(t(train), train.label)
                ypredscore      <- predict(cf, t(testing), type='raw')
            },
            svm={
                cf              <- ksvm(t(train), train.label, type="C-svc", kernel='rbf')
                ypredscore      <- predict(cf, t(testing), type="decision")

            }
        )
        ## if we want to support multi-label classification, I need to think of how to ROC plot multi-label. 
        ## Example in http://stackoverflow.com/questions/11467855/roc-curve-in-r-using-rocr-package
        return.value$prediction <- ypredscore
        return.value$label      <- testing.label
    }else{
        # k-fold cross validation
        split           <- 1:ncol
        sapply(1:k, function(i){
                                    test.indx       <- splits %in% seq(i,ncol,k)
                                    train.indx      <- !test.indx
                                    testing         <- x[,test.indx,drop=FALSE]
                                    testing.label   <- y[test.indx]
                                    train           <- x[,train.indx,drop=FALSE]
                                    train.label     <- y[train.indx]
                                    switch(classifier,
                                        naiveBayes={
                                            cf              <-naiveBayes(t(train), train.label)
                                            ypredscore      <- predict(cf, t(testing), type='raw')
                                        },
                                        svm={
                                            cf              <- ksvm(t(train), train.label, type="C-svc", kernel='rbf')
                                            ypredscore      <- predict(cf, t(testing), type="decision")

                                        }
                                    )
                                    return.value$prediction[[i]]    <<- ypredscore
                                    return.value$label[[i]]         <<- testing.label
                                })
    }
    return(return.value)
}

