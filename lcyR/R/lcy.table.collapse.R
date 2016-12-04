lcy.table.collapse <- function(table, column.id=1, collapse=',', first.occur.pick=TRUE, dup.rm=FALSE){
    # Given a column (column.id), if there are duplicated elements, make them into one row where each column (except for column.id) are collapsed into one element separated by collapse.
    # if there are duplicated rows for other columns, keep them there when collapsing if dup.rm=FALSE (default). they are not removed. 
    if(is.character(column.id) & !is.null(colnames(table))){
        column.id <- which(colnames(table) %in% column.id)
    }
    table <- data.frame(table, stringsAsFactors=FALSE)
    split <- split(table, f=table[,column.id], drop=FALSE)
    tmp <- lapply(split, function(x){
        if(nrow(x)==1){
           return(x)
        }else{
            if(first.occur.pick){
                ret <- t(apply(x, 2, function(y) { y[which(!is.na(y))[1]] }))
            }else{   
                if(dup.rm){
                    ret <- t(apply(x,2, function(y) {paste(unique(y), collapse=collapse)}))
                    ret[,column.id] <- x[1,column.id]
                }else{
                    ret <- t(apply(x,2, paste, collapse=collapse))
                    ret[,column.id] <- x[1,column.id]
                }
            }
            return(ret)
        }
    })
    ret <- do.call(rbind, tmp)
    if(!is.null(colnames(table))){
        colnames(ret) <- colnames(table)
    }
    return(ret)
}

