lcy.is.matrix.in <- function(m1, m2, order=TRUE,byrow=TRUE){
# this function is used to check whether rows or columns exist in the other matrix.

    dim.m1 <- dim(m1)
    dim.m2 <- dim(m2)
    if(byrow){
        num <- dim.m1[1]
        margin <- 1
    }else{
        num <- dim.m1[2]
        margin <- 2
    }
    if(order){
        m1      <- apply(m1,margin,paste, collapse='->')
        m2      <- apply(m2,margin,paste, collapse='->')
        index   <- match(m1,m2)
        index   <- !is.na(index)
    }else{
        x <- lapply(seq_len(num), function(i) if(byrow) m1[i,] else m1[,i])
        z <- lapply(x, function(y){tf <- apply(m2,margin, function(z){tf <- setequal(y,z)})})
        z <- do.call(rbind,z)
        index <- rowSums(z) != 0
    }
    return(index)
}


