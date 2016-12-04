lcy.row.join <- function(data, id.column=1, method=c('mean','median','none'),join.column=NULL,include=NULL){
    if(missing(data)){
        stop('data is missing')
    }
    # which column should be merged
    if(is.null(join.column)){
        join.column <- 1:ncol(data)
        join.column <- join.column[-id.column]
    }
    # which columns should be included in the end.
    if(is.null(include)){
        include <- 1:ncol(data)
        include <- include[c(-id.column,-join.column)]
    }

    split.list <- split(data, data[,id.column])
    if(missing(method)){
        method <- 'none'
    }
    if(method == 'none'){
        tmp <- lapply(split.list,function(x){x[1,join.column,drop=FALSE]})
        return(do.call(rbind,tmp))
    }else{
        if(method=='mean'){
            tmp <- lapply(split.list,function(x){
                        y <- colMeans(x[,join.column,drop=FALSE],na.rm=TRUE)
                        y <- c(x[1,c(id.column,include)],y)
                        return(y)
                    })
        }else{
            tmp <- lapply(split.list,function(x){
                        y <- apply(x[,join.column,drop=FALSE],2,median,na.rm=T)
                        y <- c(x[1,c(id.column,include)],y)
                        return(y)
                    })
        }
        ret <- do.call(rbind,tmp)
        colnames(ret)[1:length(c(id.column,include))] <- colnames(data)[c(id.column,include)]
        return(ret)
    }
}
