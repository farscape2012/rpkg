lcy.freq.summ <- 
function(vector,ignore.case=FALSE){
# used to count frequency of each element in the vector. Return named matrix with frequency and proportion 
#
#   if data is numeric and ignore.case is set to be TRUE, then data is treated as string.
#   
#   Author : Chengyu Liu <chengyu.liu@helsinki.fi>
#   2012-11-30
#   
    if(length(vector) == 0){
       return(NULL)
    }
    if(!is.vector(vector)){
        stop('input should be a vector')
    }
    vector <- vector[!is.na(vector)]
    if(ignore.case == TRUE){
        if(is.numeric(vector))
            vector <- toupper(vector)
    }
    uni.value <- unique(vector)
    frequency <- c()
    for(i in uni.value){
        frequency <- c(frequency, sum(vector == i))
    }
    frequency <- cbind(frequency, proportion=frequency/length(vector))
    rownames(frequency) <- uni.value
    if(nrow(frequency) >=3){
        order <- order(frequency[,1],decreasing=T)
        frequency <- frequency[order,]
    }
    return(frequency)
}
