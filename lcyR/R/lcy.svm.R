lcy.svm <- function(data, label, k=0){
    require(kernlab)
    num0    <- sum(label == 0)
    num1    <- sum(label == 1) 
    data0   <- data[,label == 0,drop=FALSE]
    data1   <- data[,label == 1,drop=FALSE]
    return.value <- list()
    if (k <= 1){ # independent validation
        split0      <- 1:num0
        split1      <- 1:num1
        train       <- cbind(data0[,split0 %in% seq(1,num0,2)], data1[,split1 %in% seq(1,num1,2)])
        train.label <- c(rep(0,length(seq(1,num0,2))), rep(1,length(seq(1,num1,2))))
        testing     <- cbind(data0[,split0 %in% seq(2,num0,2)], data1[,split1 %in% seq(2,num1,2)])
        testing.label <- c(rep(0,length(seq(2,num0,2))), rep(1,length(seq(2,num1,2))))

        svp             <- ksvm(t(train), train.label, type="C-svc", kernel='polydot')
        ypredscore      <- predict(svp, t(testing), type="decision")
        return.value$prediction <- as.vector(ypredscore)
        return.value$label      <- testing.label
    }else{
        # k-fold cross validation
        data    <- cbind(data0,data1)
        label   <- c(rep(0,num0),rep(1,num1))
        splits  <- 1:ncol(data)
        sapply(1:k, function(i){
                                    test.indx       <- splits %in% seq(i,ncol(data),k)
                                    train.indx      <- !test.indx
                                    testing         <- data[,test.indx,drop=FALSE]
                                    testing.label   <- label[test.indx]
                                    train           <- data[,train.indx,drop=FALSE]
                                    train.label     <- label[train.indx]

                                    svp             <- ksvm(t(train), train.label, type="C-svc", kernel='rbf')
                                    ypredscore      <- predict(svp, t(testing), type="decision")
                                    return.value$prediction[[i]]    <<- as.vector(ypredscore)
                                    return.value$label[[i]]         <<- testing.label
                                })
    }
    return(return.value)
}

