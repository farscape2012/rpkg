lcy.aheatmap <- function(data, clinic, col='-RdBu:100', scale='none', annCol=NA, annRow=NA, distfun=function(x) as.dist((1-cor(t(x),use='complete.obs')/2)), Rowv = "correlation", Colv="man", hclustfun='ward.D', breaks=seq(quantile(data, 0.05), quantile(data, 0.95),length.out=100), cexRow=0.7, cexCol=0.5, main="heatmap", fontsize=7, pdf=NULL, height=10,width=14,labCol=NULL, labRow=NULL){
    require(NMF)
    if(!missing(clinic)){
        samples         <- intersect(colnames(data), rownames(clinic))
        label           <- rep(NA, ncol(data))
        names(label)    <- colnames(data)
        tmp             <- apply(clinic, 2, function(x) {
                                label.loc   <- label
                                label.loc[samples] <- x[samples]
                                return(label.loc)
                            })
        annCol          <- lapply(1:ncol(tmp), function(x) {tmp[,x]})
        names(annCol)   <- colnames(clinic)
    }
    

    if(length(annCol)>=0){
        max.color <- max(unlist(lapply(annCol,function(x) {length(unique(x))})))
        if(max.color <= 6){
            colors          <- c('#FF8080','#5F5FFF','#66FF66','#FF80FF','#CC99FF','green','#AA00FFFF')
        }else{
            colors          <- rainbow(max.color)
        }
        annColors       <- lapply(1:length(annCol), function(x) {
                                if(length(unique(annCol[[x]])) <= length(colors)){
                                    colors[1:length(table(annCol[[x]]))]
                                }else{
                                    #c('red','blue')
                                    NULL
                                }
                            })
    }
    if(ncol(data)> 2){
        if(!is.null(pdf)){
            pdf(pdf,width=width,height=height)
        }
        h <- aheatmap(data, color=col,scale=scale, annCol=annCol, distfun=distfun, Rowv=Rowv, Colv=Colv, 
                        #breaks=breaks,cexRow=cexRow, cexCol=cexCol, hclustfun=hclustfun, main=main, fontsize=fontsize, annColors=annColors)
                        breaks=breaks,cexRow=cexRow, cexCol=cexCol, hclustfun=function(y) hclust(y,method=hclustfun), main=main, fontsize=fontsize, annColors=annColors)
        if(!is.null(pdf)){
            if(is.null(labCol)){
                labCol <- colnames(data)
            }
            if(is.null(labRow)){
                labRow <- rownames(data)
            }
            tmp <- heatmap.2(x=data,Colv=h$Colv,Rowv=h$Rowv,trace='none', col='bluered',labRow=labRow, labCol=labCol, scale=scale,symkey=FALSE, breaks=breaks)
            dev.off()
        }
    }
    return(h)
}
