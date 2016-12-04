lcy.table.split <- function(table, column.id=1, split=','){
    # split "split" separated column and duplicate the rows which have multiple elements after splitting.
    if(is.character(column.id) & !is.null(colnames(table))){
        column.id <- which(colnames(table) %in% column.id)
    }
    tmp <- lapply(1:nrow(table), function(x) {
        elm <- strsplit(table[x,column.id], split=split)[[1]]
        len <- length(elm)
        ret <- matrix(rep(table[x,], len), nrow=len,byrow=T)
        ret[,column.id] <- elm
        return (ret)
    })
    ret <- do.call(rbind, tmp)
    if(!is.null(colnames(table))){
        colnames(ret) <- colnames(table)
    }
    return(ret)
}

