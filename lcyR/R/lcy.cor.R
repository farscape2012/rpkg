lcy.cor <- function(x,y,...){
    index1  <- is.na(x)
    index2  <- is.na(y)
    index   <- index1 | index2
    x <- x[!index]
    y <- y[!index]
    return(cor.test(x,y,...))
}

