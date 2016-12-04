lcy.misc.cutdendrogram <- function(dendrogram, h) {
    # simple function that simply cut the dendrogram and return the cluster members.
    levels  <- cut(dendrogram, h=h)$lower
    group   <-lapply(levels, labels)
    names(group) <- paste('cluster', 1:length(levels),sep='_')
    return(group)
}


lcy.misc.filter.marker <- function (data, label, aucth = 0.9, unique=FALSE){
    ## Used to identify markers which are predictive or specific for a subtype. For each gene, the predictive power is evaluated for each subtype. When unique is FALSE, the retured genes are predictive for at least a subtype (maybe more). NOTE when unique is set to TRUE, it will return genes which are specific for a cluster which means if a gene is significant in two clusters compared with the others, then it will be returned. Since it is NOT specific for a cluster. If label does not have names, then the label order should be the same as the data.
    require(ROCR)
    require(Vennerable)
    sigdat <- data
    if(is.null(rownames(sigdat))){
        rownames(sigdat) <- as.character(1:ncol(sigdat))
    }
    sigGeneIds <- rownames(sigdat)
    compareTwoClus <- function(sigdat, label, clu = 1) {
        temp                <- label
        temp[label == clu] <- 0
        temp[label != clu] <- 1
        if(is.null(names(temp))){
            names <- 1: length(temp)
        }else{
            names <- names(temp)
        }
        sigdat  <- sigdat[, names]
        preds   <- apply(sigdat, 1, prediction, labels=temp)
        aucs    <- unlist(lapply(preds, function(x) {
                        (performance(x, measure = "auc"))@y.values[[1]]
                    }))  
        aucs    <- rowMax(cbind(aucs, 1-aucs))
        return(rownames(sigdat)[which(aucs >= aucth)])
    }
    sigs <- lapply(unique(label), function(x) {
        sigs <- compareTwoClus(sigdat, label, x)
    })
    if(unique) {
        K       <- length(sigs)
        if(length(sigs) <= 3){
            type = 'circles'
        }else{
            type = 'ellipses'
        }
        venn    <- lcy.venn.diagram(Sets=sigs, type=type)
        index   <- unlist(lapply(rownames(venn), function(x) {sum(strsplit(x,split='_')[[1]] == 'NOT')})) == (K-1)
        sigs  <- lapply(venn[index],function(x){tmp <-strsplit(x,split=',')[[1]]})
    }
    return(unique(unlist(sigs)))
}
lcy.plot.distribution <- function(data, group=list()){
    col     <- c(rgb(1,0,0,1/4),rgb(0,0,1,1/4),rgb(0,1,0,1/4)) 
    #col     <- c('red','blue','green','pink','yellow','black','purple') 
    group   <- lapply(group, function(x) {intersect(x, names(data))})
    if(is.null(names(group))){
        names(group) <- as.character(1:length(group))
    }
    names   <- names(group)
    # plot distribution (Figure 4)
    hist.obj <- lapply(group,function(x) {
                    h <- hist(data[x],plot=FALSE, breaks=50)
                    })
    if(length(hist.obj) >= 1){
        xlim <- c(min(data,na.rm=TRUE) - sd(data,na.rm=TRUE), max(data,na.rm=TRUE) + sd(data,na.rm=TRUE))
        ylim <- lapply(hist.obj,function(x ){
                            x$counts
                        })
        ylim <- c(0,max(unlist(ylim)))
        ret <- lapply(1:length(hist.obj), function(x) {
                        if(x == 1){
                            plot(hist.obj[[x]], col=col[x], xlim=xlim,ylim=ylim)
                        }else{
                            plot(hist.obj[[x]], col=col[x], xlim=xlim,ylim=ylim, add=T)  # first histogram
                        }
                        return(NULL)
                    })
        legend('topright',legend=names(group), col = col[1:length(hist.obj)], lty = 1, lwd=3)
    }
}
