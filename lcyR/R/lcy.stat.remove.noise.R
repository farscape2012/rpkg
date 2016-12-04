lcy.stat.remove.outliers <- function(d, quantile=0.001){
    # replace values smaller or larger than given quantile and 1-quantile values.
    min.exp             <- quantile(d,quantile,na.rm=TRUE)
    d[d <= min.exp]     <- min.exp
    min.exp             <- quantile(d,1-quantile,na.rm=TRUE)
    d[d >= min.exp]     <- min.exp
    return(d)
}
