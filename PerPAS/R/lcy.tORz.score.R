lcy.tORz.score <- function(data, group=list(ctrl,treat), type='zscore', method='median',pvalue=FALSE, scale=TRUE, byrow=FALSE){
    if(missing(data))
        stop("data is missing")
    if(is.null(group)|missing(group)){# if group is missing, apply mean centering to all data
        if(!byrow){
            if(method == 'median'){
                y       <- apply(data,2,function(x){ x - median(x,na.rm=T) })
                score   <- scale(y,center=FALSE,scale=scale)
            }else{
                score   <- scale(data,scale=scale)
            }
        }else{
            if(method == 'median'){
                y       <- apply(data,1,function(x){ x - median(x,na.rm=T) })
                score   <- t(scale(y,center=FALSE))
            }else{
                data    <- t(data)
                score   <- scale(data,scale=scale)
                score   <- t(score)
            }
        }
    }else{
        if(is.null(group$ctrl) & !is.null(group$treat)){# if ctrl is null mean centering to treat only 
            treat <- intersect(colnames(data), group$treat)
            data <- data[,treat]
            if(!byrow){
                if(method == 'median'){
                    y       <- apply(data,2,function(x){ x - median(x,na.rm=T) })
                    score   <- scale(y,center=FALSE,scale=scale)
                }else{
                    score   <- scale(data,scale=scale)
                }
            }else{
                if(method == 'median'){
                    y       <- apply(data,1,function(x){ x - median(x,na.rm=T) })
                    score   <- t(scale(y,center=FALSE),scale=scale)
                }else{
                    data    <- t(data)
                    score   <- scale(data,scale=scale)
                    score   <- t(score)
                }
            }
        }else if(!is.null(group$ctrl) & is.null(group$treat)){
            stop("group$treat could not be NULL")
        }else{# both are not NULL
            if(length(group$ctrl) <=1)
                stop("the number of ctrl samples should be larger than 1. The more the better")
            colnames    <- colnames(data)
            if(!is.null(colnames)){
                group$ctrl  <- intersect(colnames, group$ctrl)
                group$treat <- intersect(colnames, group$treat)
            }
            len.ctrl    <- length(group$ctrl)
            len.treat   <- length(group$treat)
            data        <- as.matrix(data[,c(group$treat, group$ctrl)])
            if(byrow){
                DIM1 <- 1
                DIM2 <- 2
            }else{
                DIM1 <- 2
                DIM2 <- 1
            }
            mean.ctrl   <- apply(data[,group$ctrl], DIM1, as.name(as.character(method)),na.rm=TRUE)
            sd.ctrl     <- apply(data[,group$ctrl], DIM1, sd,na.rm=TRUE)
            sd.ctrl[sd.ctrl == 0] <- NA
            score       <- apply(data[,c(group$treat, group$ctrl)], DIM2 , function(x) {
                                    (x-mean.ctrl)
                                })
            if(scale){
                score   <- score / sd.ctrl
            }
            if(!byrow){
                score   <- t(score)
            }
            score <- score[!colSums(apply(score, 1,  is.na)) == ncol(score),]
            if(type!='zscore'){
                score   <- score * sqrt(len.ctrl)
            }
            if(pvalue){
                if(type=='zscore'){
                    score <- 2*pnorm(-abs(score))
                }else{
                    score <- 2*pt(-abs(score),df=len.ctrl - 1)
                }
            }
        }
    }
    attr(score, "scaled:scale") <- NULL
    attr(score, "scaled:center") <- NULL
    return(score)
}
