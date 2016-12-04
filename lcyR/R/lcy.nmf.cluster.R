lcy.nmf.survey <- function(d, num.genes=seq(2000, nrow(d), 2001),method='brunet', rank=2:6,seed, .options='vp6'){
    # d should be non negative matrix and sorted based on var or mad or sd. High variance genes are on the top
    old.path <- getwd()
    require(NMF)
    lcy.initiation(sprintf("consensusmap_%s_%d_%d", method, rank[1], rank[length(rank)]))
    if(missing(seed)){
        seed <- 'random'
    }
    results <- lapply(num.genes, function(x){
        pdf(sprintf('consensusmap_%d.pdf',x));
        dt          <- d[1:x,1:200]
        results     <- nmf(dt, rank=rank, method=method,seed=seed,.options=.options)
        consensusmap(results, labCol = NA, labRow = NA)
        plot(results)
        dev.off()
        return(results)
    })
    # survey with different ranks and different genes
    pdf(sprintf('NMF_rank_survey_%s.pdf',method))
    len     <- length(results)
    tmp     <- lapply(1:len, function(k){
                    c(min(results[[k]][[1]][,"cophenetic"],na.rm=T),max(results[[k]][[1]][,"cophenetic"],na.rm=T))
                })
    tmp     <- do.call(rbind, tmp)
    ylim1   <- c(min(tmp[,1], na.rm=T)-3*sd(tmp[,1], na.rm=T), max(tmp[,2], na.rm=T)+ 2*sd(tmp[,2], na.rm=T))
    tmp     <- lapply(1:len, function(k){
                    c(min(results[[k]][[1]][,"rss"],na.rm=T),max(results[[k]][[1]][,'rss'],na.rm=T))
                })
    tmp     <- do.call(rbind, tmp)
    ylim2   <- c(min(tmp[,1], na.rm=T)-3*sd(tmp[,1], na.rm=T), max(tmp[,2], na.rm=T)+ 2*sd(tmp[,2], na.rm=T))
    par(mfrow=c(1,2))
    plot(results[[1]][[1]][,1], results[[1]][[1]][,13],type='b',ylim=ylim1,pch=1,xlab='Number of clusters', main='Cophenetic',col=1)
    legend('bottomleft',legend=num.genes,pch=25+(1:len),title='Number of genes',col=1:len)
    for(k in 2:length(results)){
        lines(results[[k]][[1]][,1], results[[k]][[1]][,13], type='b', pch=k, col=k)
    }
    plot(results[[1]][[1]][,1], (results[[1]][[1]][,4]),type='b',ylim=ylim2,pch=1,xlab='Number of clusters', main='RRS',col=1)
    legend('bottomleft',legend=num.genes,pch=20+(1:len),title='Number of genes',col=1:len)
    for(k in 2:length(results)){
        lines(results[[k]][[1]][,1],(results[[k]][[1]][,4]), type='b', pch=k,col=k)
    }
    dev.off()
    setwd(old.path)
    return(results)
}
lcy.nmf.cluster <- function(d, clinic, rank=3, method='brunet', marker.call='kim', marker.unique=FALSE, log2=FALSE, qvalue=5, aucth=0.7, err.cutoff=0.2, thresh, silhouette=FALSE, silhouette.method='correlation', seed=123456,min.foldchange=0){
    # DeSousa2013 bioconductor
    # signature identification
    require(NMF)
    colors              <- rainbow(9)
    if(silhouette){
        silhouette  <- pam(t(d), rank)
        si          <- silhouette(silhouette,dist(t(d),method=silhouette.method))
        if(sum(si[,3] >0)> 0.5 * ncol(d)){
            d       <- d[, names(which(si[,3] >0))]
        }
        pdf('silhouette.pdf')
        plot(si, col=colors[1:rank])
        dev.off()
    }
    pdf(sprintf('clustering_%s_%s_%s.pdf', method, marker.call, rank))
    #set.seed(321321412)
    res         <- nmf(d, rank=rank, method=method,seed=seed)
    label.pred  <- as.numeric(predict(res, 'columns'))
    # transform data to logrithm
    if(!log2){
        d       <- log2(d)
    }
    # clinic data
    if(!missing(clinic)){
        samples         <- intersect(colnames(d), rownames(clinic))
        label           <- rep(NA, ncol(d))
        names(label)    <- colnames(d)
        tmp             <- apply(clinic, 2, function(x) {
                                label.loc   <- label
                                label.loc[samples] <- x[samples]
                                return(label.loc)
                            })
        annC            <- lapply(1:ncol(tmp), function(x) {tmp[,x]})
        names(annC)     <- colnames(clinic)
        annC$pred       <- as.character(label.pred)
    }else{
         annC           <- list(cluster=as.character(label.pred))
    }
    annColors           <- lapply(1:length(annC), function(x) {
                                if(length(unique(annC[[x]])) <= length(colors)){
                                    colors[1:length(table(annC[[x]]))]
                                }else{
                                    c('red','blue')
                                }
                            })
    genes               <- unique(unlist(lapply(unique(label.pred), function(z) {
                                                temp                    <- label.pred
                                                temp[label.pred == z]   <- 1
                                                temp[label.pred != z]   <- 2
                                                genes                   <- lcy.DEG.samr(x=d, y=temp, resp.type='Two class unpaired',qvalue=qvalue,delta=0.3,min.foldchange=min.foldchange)
                                                return(rownames(genes))
                                            })))
    print(length(genes))
    if(length(genes) <= 1){
        print('there are not any significant genes')
        dev.off()
        return(NULL)
    }
    # evaluate the predictive power of each gene
    marker              <- lcy.misc.filter.marker(d[genes,], label=label.pred,aucth=aucth,unique=marker.unique)
    marker              <- unlist(marker)
    print(length(marker))
    if(marker.call == 'pam'){
        d               <- d[marker,]
        if(missing(thresh)){
            rst         <- lcy.pam.train(d,label.pred, err.cutoff=err.cutoff)
        }else{
            rst         <- lcy.pam.train(d,label.pred, err.cutoff=err.cutoff,thresh=thresh)
        }
        print('pam train done')
        label.pred2     <- lcy.pam.predict(d, rst)
        #annC$pred2      <- as.character(label.pred2$class)
        marker          <- rst$signature
    }else if(marker.call == 'simple'){
        # do nothing
    }else{
        marker          <- extractFeatures(res,method=marker.call)
        marker          <- unlist(marker)
        marker          <- marker[!is.na(marker)]
        marker          <- rownames(d)[marker]
        print(marker)
    }
    if(length(marker)> 1){
        print(length(marker))
        aheatmap(d[marker,],col='RdlBu',scale='none',annCol=annC,distfun=function(x) as.dist((1-cor(t(x))/2)), Rowv = "correlation", Colv="man", 
                                    breaks=seq(quantile(d[marker,], 0.01), quantile(d[marker,], 0.99),length.out=12),cexRow=0.7, cexCol=0.5,
                                    hclustfun=function(y) hclust(y,method='ward.D'),main=marker.call, fontsize=5, annColors=annColors)
        tmp <- lapply(annC,function(x) {
            col <- rainbow(length(unique(x[!is.na(x)])))
            ColSideColors   <- as.character(factor(x,levels=unique(x[!is.na(x)]),labels=col))
            heatmap.2(d[marker,],col='bluered',trace='none', scale='row', distfun=function(x) as.dist((1-cor(t(x))/2)),hclustfun=function(y) hclust(y,method='ward.D'), ColSideColors=ColSideColors)
            legend("topright", fill=col, legend=unique(x[!is.na(x)]),cex=0.5,text.col='black',title='Row color')
        })
    }
    dev.off()
    return(marker)
}
