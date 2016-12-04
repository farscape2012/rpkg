lcy.set.operation <- function(..., func='intersect'){
    list <- list(...)
    if(!exists(func))
        stop('function does not exists')
    if (length(list) <=1)
        stop('at least two variables shoule be given.')
    x <- list[[1]]
    for(i in 2:length(list)){
        x <- do.call(func,args=list(x=x,y=list[[i]]))
    }
    x
}
