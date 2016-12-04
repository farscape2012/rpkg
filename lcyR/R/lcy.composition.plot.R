lcy.composition.plot <- 
function(data,sample.id,fc=c(-1,1),file='composition_plot.pdf',col=c('red','green'),legend=c('UP','DOWN')){
    if(missing(sample.id))
        sample.id <- colnames(data)
    num.up  <- colSums(data[,sample.id] >= fc[2],na.rm=T)
    num.down<- colSums(data[,sample.id] <= fc[1],na.rm=T)
    num.deg <- cbind(num.up,num.down,num.deg=num.up+num.down)
    if(!is.null(file)){
        pdf(file)
        barplot(t(num.deg[,1:2]),beside=F,col=col,
                    main="Comparison between samples",
                    ylab='The number of DEGs',
                    args.legend=list(x='topright',inset=0.02,cex=0.8),
                    legend=legend, names.arg=sample.id)
        dev.off()
    }
    num.deg
}    
