lcy.diff.comparison <- 
function(data,group,pair.wise=FALSE,p.adjust.method='bonferroni',fc.label=c(t='treat',c='ctrl')){
    tmp.func1 <- function(x){
        xx      <- pairwise.t.test(x=data[x,],label,p.adjust.method=p.adjust.method)
        return(xx$p.value)
    }
    
    tmp.func2 <- function(x){
        z       <- data.frame(x=x,y=label)
        tryCatch({xx      <- aov(x~y,data=z)
                  xx      <- summary(xx)
                  p.value <- xx[[1]][1,'Pr(>F)']
                  }, error=function(ex){
                                print(ex)
                  }, finially={
                            p.value <- NA
                  })
        return(p.value)
    }
    rownames(group) <- group[,1]
    x           <- intersect(colnames(data),group[,1])
    group       <- group[x,]
    group       <- group[!is.na(group[,2]),]
    uni.value   <- unique(group[,2])
    data        <- data[,group[,1]]
    label       <- group[,2]
    if(pair.wise==TRUE){
        result  <- lapply(as.list(1:nrow(data)), tmp.func1)
    }else{
        p.value <- apply(data,MARGIN=1, FUN=tmp.func2)
        p.value <- p.adjust(p.value,method=p.adjust.method)
        if(length(uni.value)==2){
            fc  <- apply(data[,group[group[,2]==fc.label['t'],1]],1,median,na.rm=TRUE) - apply(data[,group[group[,2]==fc.label['c'],1]],1,median,na.rm=TRUE)
            result <- cbind(p.value,fc)
        }else{
            result <- p.value
        }
    }
    return(result)
}
