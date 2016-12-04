lcy.pam.predict <- function (data, fit, postRth = 1){
    require(pamr)
    signature       <- fit$signature
    pam.rslt        <- fit$pam.rslt
    thresh          <- fit$thresh

    if(is.null(colnames(data))){
        colnames(data) <- paste('col',1:ncol(data),sep='_')
    }
    pred            <- pamr.predict(pam.rslt, data, thresh, type = "posterior")
    maxr            <- apply(pred, 1, max)
    postR           <- maxr/(1 - maxr)
    sel.samples.f   <- names(postR)[which(postR >= postRth)]
    clu.pred        <- apply(pred, 1, which.max)
    #clu.pred        <- sort(clu.pred)
    #clu.pred.reord  <- NULL
    #for (cl in 1:ncol(pred)) {
    #    temp            <- names(sort(postR[names(clu.pred[clu.pred == cl])]))
    #    clu.pred.reord  <- c(clu.pred.reord, temp)
    #}
    #clu.pred    <- clu.pred[clu.pred.reord]
    #nam.ord     <- names(clu.pred)
    #sdat.sig    <- data[signature, nam.ord]
    #gclu.f      <- hclust(as.dist(1 - cor(t(sdat.sig))), method = "complete") 
    #return(list(sdat.sig=sdat.sig, pred=pred, clu.pred=clu.pred, nam.ord=nam.ord, gclu.f=gclu.f))
    attr(pred, "scaled:scale") <- NULL
    return(list(class=clu.pred, prob=pred))
}
