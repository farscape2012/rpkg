lcy.stat.mds <- function(d, label, k=2, dist.method="euclidean", eig=FALSE,output='mds.pdf'){
    dist <- dist(t(d),method=dist.method)
    fit <- cmdscale(dist, eig=eig, k=k) # k is the number of dim

    if(eig){
        fit <- fit$points
    }
    # plot
    #if(ncol(fit)) {
    #    stop("All samples are equal; MDS can not be computed.")
    #}
    if(ncol(fit) < k) {
        print("Warning: plotting of MDS is not possible due to similaty of samples.")
        plot.MDS <- FALSE
    }else {
        plot.MDS <- TRUE
    }
    len.label   <- length(unique(label))
    uni.label   <- unique(label)
    uni.color   <- rainbow(len.label)
    color       <- rep(uni.color[1],length(label))
    for(i in 2:length(uni.color)){
        color[label == uni.label[i]] <- uni.color[i]
    }
    if(!missing(output)){
        pdf(output)
    }
    pd <- data.frame(fit,label=label,stringsAsFactors=FALSE)
    plot(fit[,1:2], xlab="X", ylab="Y", main="Metric  MDS for zscore", type="n")
    text(fit[,1:2], labels = colnames(d),cex=.7,col=color)
    legend("topright", legend=as.character(uni.label), lwd=3, cex=0.6, y.intersp=1.4, col=uni.color, lty=rep(1,2))
    if(!missing(output)){
        dev.off()
    }
    if (k >2){
        require(rgl)
        x <- fit[,1]
        y <- fit[,2]
        z <- fit[,3]
        plot3d(x, y,z, xlab="X", ylab="Y",zlab='Z', main="Metric  MDS for zscore",    type="n")
        text3d(x, y,z, texts = colnames(d), col=color)
    }
    return(fit)
}

