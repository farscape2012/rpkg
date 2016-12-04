lcy.stat.mds <- function(d, label, k=2, dist.method="euclidean", eig=FALSE,output='mds.pdf'){
    dist <- dist(t(d),method=dist.method)
    fit <- cmdscale(dist, eig=eig, k=k) # k is the number of dim

    if(eig){
        fit <- fit$points
    }
    if(missing(label)){
        label   <- rep("x", ncol(d))
    }
    len.label   <- length(unique(label))
    uni.label   <- unique(label)
    uni.color   <- rainbow(len.label)
    color       <- rep(uni.color[1],length(label))
    for(i in 2:length(uni.color)){
        color[label == uni.label[i]] <- uni.color[i]
    }
    if(!is.null(output)){
        if(!grepl(pattern="\\.pdf$",x=output)){
            output = paste(output, '.pdf', sep='')
        }
        pdf(output)
    }
    pd <- data.frame(fit,label=label,stringsAsFactors=FALSE)
    plot(pd[,1:2], xlab="X", ylab="Y", main="Metric  MDS for zscore", type="n")
    text(pd[,1:2], labels = label, cex=.7,col=color)
    legend("topright", legend=as.character(uni.label), lwd=3, cex=0.6, y.intersp=1.4, col=uni.color, lty=rep(1,2))
    if(!is.null(output)){
        dev.off()
    }
    if (k >2){
        require(rgl)
        x <- pd[,1]
        y <- pd[,2]
        z <- pd[,3]
        plot3d(x, y, z, xlab="X", ylab="Y",zlab='Z', main="Metric  MDS for zscore",    type="n")
        text3d(x, y, z, texts=label, col=color)
    }
    return(fit)
}

