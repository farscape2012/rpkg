lcy.in <- 
function(...,list){
    x <- unlist(list(...))
    tf.matrix <- matrix(rep(0,2*length(x)),nc=2)
    rownames(tf.matrix) <- as.character(x)
    colnames(tf.matrix) <- c('tf','index')
    if(nrow(tf.matrix) == 0)
        return(tf.matrix)
#    tf.matrix[,2] <- as.numeric(sapply(x,grep,x=list))
#    tf.matrix[!is.na(tf.matrix[,2]),1] <- 1
    for(i in 1:length(x)){
        index <- which(list %in% x[i])
        if(length(index)==0)
            tf.matrix[i,] <- c(0, NA)
        else
            tf.matrix[i,] <- c(1,index)
    }
    
    return(tf.matrix)
}

