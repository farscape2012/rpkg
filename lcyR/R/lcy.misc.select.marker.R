lcy.misc.select.marker <- function(d, label, qvalue=20, pvalue=0.05,aucth= 0.9,err.cutoff=0.1,marker.unique=FALSE,marker.call='pam',output='select.marker'){
    old.path    <- getwd()
    lcy.initiation(output)
    genes       <- unique(unlist(lapply(unique(label), function(z) {
                        temp                <- label
                        temp[label == z]    <- 1
                        temp[label != z]    <- 2
                        genes               <- lcy.DEG.samr(x = d, y = temp, resp.type = "Two class unpaired", qvalue = qvalue, pvalue=pvalue, delta = 0.3, min.foldchange = 0)
                        return(rownames(genes))
                    })))
    if(length(genes) <= 1){
        print('there are not any significant genes')
        return(NULL)
    }
    # evaluate the predictive power of each gene
    marker              <- lcy.misc.filter.marker(d[genes,], label=label,aucth=aucth,unique=marker.unique)
    marker              <- unlist(marker)
    print(sprintf("Number of markers after filtering: %s",length(marker)))
    if(marker.call == 'pam'){
        d               <- d[marker,]
        rst             <- lcy.pam.train(d,label, err.cutoff=err.cutoff)
        print('pam train done')
        label.pred2     <- lcy.pam.predict(d, rst)
        marker          <- rst$signature
    }else if(marker.call == 'simple'){
        # do nothing
    }else{
        require(NMF)
        marker          <- extractFeatures(res,method=marker.call)
        marker          <- unlist(marker)
        marker          <- marker[!is.na(marker)]
        marker          <- rownames(d)[marker]
    }
    print(sprintf("Number of marker after %s :%s",marker.call, length(marker)))
    if(length(marker)> 1){
        pdf('aheatmap.pdf')
        aheatmap(d[marker,],col='RdYlBu',scale='non',annCol=label,distfun=function(x) as.dist((1-cor(t(x))/2)), Rowv = "correlation", Colv="man", 
                                    breaks=seq(quantile(d[marker,], 0.01), quantile(d[marker,], 0.99),length.out=12),cexRow=0.7, cexCol=0.5,
                                    hclustfun=function(y) hclust(y,method='ward.D2'),main=marker.call, fontsize=5)
        dev.off()
        mds <- lcy.stat.mds(d[marker,],label=label,k=2)
    }
    write.table(file='marker.csv',x=data.frame(marker=marker,stringsAsFactors=FALSE),row.names=FALSE)
    setwd(old.path)
    return(marker)
}
