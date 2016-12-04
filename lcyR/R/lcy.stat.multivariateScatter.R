lcy.stat.multivariateScatter <- function(data, byrow=TRUE, output=NULL,...){
    require(car)
    if(byrow){
        data <- data
    }
    if(!is.null(output)){
        pdf(output);
    }
    scatterplotMatrix(data, ...)
    if(!is.null(output)){
        dev.off();
    }
}
