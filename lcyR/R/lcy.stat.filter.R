lcy.stat.filter <- function(data, byrow=TRUE,method=c('sd','var','mad'),decreasing=TRUE, top.N=nrow(data), cutoff){
    if(byrow){
        DIM <- 1;
    }else{
        DIM <- 2
    }
    score   <- apply(data,DIM,as.name(as.character(method)),na.rm=T)
    order   <- order(score,decreasing=decreasing)
    score   <- score[order]
    data    <- data[order,]
    if(!missing(cutoff)){
        index <- score >= cutoff
        if(sum(index) >= top.N){
            index <- which(index)[1:top.N]
        }
        ret <- data[index,,drop=FALSE]
    }else{
        if(top.N <= nrow(data)){
            ret <- data[1:top.N,,drop=FALSE]
        }else{
            ret <- NULL
        }
    }
    return(ret)
}
