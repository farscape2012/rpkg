lcy.ROCcurve <- 
function(score, label,multiline=TRUE,main=NULL, plot=TRUE,auc.threshold=0.6,seed=12345){
    require(ROCR)
    set.seed(seed)
    if(multiline == TRUE){
        pred    <- prediction(score, label)
        perf    <- performance(pred,"tpr","fpr")
        auc     <- performance(pred,"auc")
        # now converting S4 class to vector
        auc     <- unlist(slot(auc, "y.values"))
        # adding min and max ROC AUC to the center of the plot
        minauc  <-min(round(auc, digits = 2))
        maxauc  <-max(round(auc, digits = 2))
        meanauc <-mean(round(auc, digits = 2),na.rm=T)
        if(plot==TRUE){
            minauct <- paste(c("min(AUC) = "),minauc,sep="")
            maxauct <- paste(c("max(AUC) = "),maxauc,sep="")
            meanauct<- paste(c("mean(AUC) = "),meanauc,sep="")
            plot(perf, lty=3, col="black",main=main,cex.main=0.8)
            plot(perf, avg="vertical",lwd=3,col="red",spread.estimate="stderror",plotCI.lwd=2,add=TRUE)
            legend(0.7,0.25,c(minauct,maxauct,meanauct,"\n"),border="white",cex=1,box.col = "white")
        }
        return(c(minauc,meanauc,maxauc))
    }
    else{
        if(length(score) <=1 | length(label) <=1 ){
            stop('input is not correct. length of score(label) should be larger than 1 when multiline is set to be TRUE')
        }else if(length(score) != length(label))
            stop('input is not correct. length of score(label) should be larger than 1 when multiline is set to be TRUE')
        else{
            x <- c()
            y <- c()
            for(i in 1:length(score)){
                x <- c(x, score[[i]])
                y <- c(y, label[[i]])
            }
        }
        pred    <- prediction(x, y)
        perf    <- performance(pred,"tpr","fpr")
        auc     <- performance(pred,"auc")
        # now converting S4 class to vector
        auc     <- unlist(slot(auc, "y.values"))
        auc     <- round(auc, digits = 2)
        if(plot==TRUE & auc >= auc.threshold){
            auct    <- paste(c("AUC = "),auc,sep="")
            plot(perf, lty=3, col="black",main=main,cex.main=0.8)
            legend(0.8,0.1,auct,border="white",cex=1,box.col = "white")
        }
        return(auc)
   }
}
