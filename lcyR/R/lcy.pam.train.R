lcy.pam.train <- function(data, label, nfold = 10, nboot = 100, err.cutoff=0.02, n.threshold = 30, err.pdf=TRUE, thresh, seed=123456){
    # n.threshold = 30 for pamr.train : Number of threshold values desired (default 30)
    # if there is any threshold which smaller than err.cutoff, then all genes are considered as signature.
    getCentroids <- function (fit, data, threshold) {
        genenames <- data$genenames[fit$gene.subset]
        x <- data$x[fit$gene.subset, fit$sample.subset]
        clabs <- colnames(fit$centroids)
        scen <- pamr.predict(fit, data$x, threshold = threshold, 
            type = "cent")
        dif <- (scen - fit$centroid.overall)/fit$sd
        if (!is.null(fit$y)) {
            nc <- length(unique(fit$y))
        }
        if (is.null(fit$y)) {
            nc <- ncol(fit$proby)
        }
        o <- drop(abs(dif) %*% rep(1, nc)) > 0
        d <- dif[o, ]
        nd <- sum(o)
        genenames <- genenames[o]
        xx <- x[o, ]
        oo <- order(apply(abs(d), 1, max))
        d <- d[oo, ]
        genenames <- genenames[oo]
        return(d)
    }
    require(pamr)
    set.seed(seed)
    dat <- list()
    if(is.null(names(label))){
        names <- 1: length(label)
    }else{
        names <- names(label)
    }
    if(is.null(rownames(data))){
        rownames(data) <- paste("row",1:nrow(data),sep='_')
    }
    dat$x <- data[, names]
    dat$y <- label
    dat$y <- factor(dat$y)
    dat$geneid      <- rownames(dat$x)
    dat$genenames   <- rownames(dat$x)
    pam.rslt        <- pamr.train(data = dat,n.threshold=n.threshold)
    pam.cv.rslt.l   <- list()
    for (i in 1:nboot) {
        pam.cv.rslt         <- pamr.cv(fit = pam.rslt, data = dat, nfold = nfold)
        pam.cv.rslt.l[[i]]  <- pam.cv.rslt
    }
    print('#####################################################################################################here0')
    err             <- t(sapply(1:length(pam.cv.rslt.l), function(x) pam.cv.rslt.l[[x]]$error, simplify = TRUE))
    ngenes          <- c(sapply(1:(length(pam.rslt$threshold) - 1), function(x) nrow(pamr.listgenes(pam.rslt, dat, pam.rslt$threshold[x]))), 0)
    colnames(err)   <- ngenes
    # choose threshold cutoff. select the first index where error rate is smaller than err.cutoff.
    if(err.pdf){
        pdf("Pamr_error_rate.pdf")
        boxplot(err, xlab = "No. of genes", ylab = "error rate")
        dev.off()
    }
    print('#####################################################################################################here1')
    index           <- which(colMeans(err) <= err.cutoff)
    if(length(index)<=0){
        index <- 1
    }else{
        index <- index[length(index)]
    }
    print('here1')
    print(pam.rslt$threshold)
    if(missing(thresh)){
        thresh      <- pam.rslt$threshold[index]
    }else{
        thresh      <- pam.rslt$threshold[n.threshold - thresh + 1]
    }
    print('here2')
    signature       <- (pamr.listgenes(pam.rslt, dat, thresh))[, "id"]
    print('here3')
    print(length(signature))
    cents           <- getCentroids(pam.rslt, dat, thresh)
    cents           <- cents[signature, ]
    print("pam is done")
    return(list(signature = signature, pam.rslt = pam.rslt, thresh = thresh, err = err, cents = cents))
}
