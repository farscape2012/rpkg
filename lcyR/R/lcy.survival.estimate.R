lcy.survival.estimate <- function(clinic, group, col.time=1, col.event=2,label.event='alive', time.out.limit=NULL,min.event=5,main='Survival Estimate',file=NULL,width=8,height=8){
    col     <- c('red','blue','green','pink','yellow','black','purple')
    if(class(group) == 'vector'){
        t       <- table(group)
        names   <- names(t)
        index   <- t <= min.event
        rm.index<- rep(FALSE, length(group))
        if(sum(index)>=1){
            rm      <- names[index]
            for(i in rm){
                rm.index <- rm.index | (group == i)
            }
            group   <- group[!rm.index]
            clinic  <- clinic[!rm.index,]
        }
    }else if(class(group) == 'list'){
        group   <- lapply(group, function(x) {intersect(x, rownames(clinic))})
        t       <- unlist(lapply(group, length))
        if(is.null(names(group))){
            names(group) <- as.character(1:length(group))
        }
        names   <- names(group)
        index   <- t <= min.event
        rm.index<- rep(FALSE, length(group))
        if(sum(index)>=1){
            rm      <- names[index]
            for(i in rm){
                group[i] <- NULL
            }
        }
        samples <- intersect(unlist(group), rownames(clinic))
        v   <- rep(0,length(samples))
        names(v) <- samples
        for(i in names(group)){
            v[samples %in% group[[i]]] <- i
        }
        group   <- v
        clinic  <- clinic[samples,]
    }
    require(survival)
    time    <- as.numeric(clinic[,col.time])
    event   <- as.numeric(clinic[,col.event] != label.event)
    if(!is.null(time.out.limit)){
        long.times                              <- (time> time.out.limit)
        long.times[which(is.na(long.times))]    <- FALSE
        time[long.times]                        <- time.out.limit
        event[long.times]                       <- 0
    }
    fit         <-  survfit(Surv(time, event) ~ group)
    diff        <- survdiff(Surv(time, event) ~ group)
    pvalue      <- 1-pchisq(diff$chisq, length(diff$n)-1)
    g.strata    <- fit$strata
    g.names     <- sapply(names(g.strata), FUN=function(x){ strsplit(x,'=',fixed=TRUE)[[1]][2] })
    g.legend    <- paste(g.names, paste('(',fit$n,')',sep=''))
    if(!is.null(file)){
        pdf(file=file,width=width,height=height)
    }
    plot(fit, conf.int='none', col=col[1:length(unique(group))], xlab='Time (days)', ylab='Survival Probability',main=main,cex.main=0.6)
    legend('bottomleft',legend=g.legend, col=col[1:length(unique(group))], lty = 1)
    text(max(time,na.rm=T)*2/8,0.1,sprintf("P=%.2e",pvalue), pos=4, col="blue")
    if(!is.null(file)){
        dev.off()
    }
    return(pvalue)
}
