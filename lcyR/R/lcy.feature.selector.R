lcy.feature.selector <-
function(data,group=list(t,c),id, search.engine='bfs',CV=5,eval='svm'){


    require(FSelector)
    SVM.classifier <- function(subset){
        print(subset)
        index    <- colSums(is.na(data[subset,,drop=FALSE]))==0
        label    <- label[index]
        tmp.data <- data[subset,index,drop=FALSE]
        value    <- lcy.svm(tmp.data,label,k=CV)
        auc      <- lcy.ROCcurve(value$prediction,value$label,multiline=FALSE,plot=FALSE)
        perf    <<- rbind(perf, data.frame(subset=paste(subset,collapse=","),perf=auc,stringsAsFactors=FALSE))
        return(auc)
    }
    
   NB.classifier <- function(subset){
        tmp.data <- data[subset,,drop=FALSE]
        value    <- lcy.svm(tmp.data,label,k=CV)
        auc      <- lcy.ROCcurve(value$prediction,value$label,multiline=FALSE,plot=FALSE)
        perf    <<- rbind(perf, data.frame(subset=paste(subset,collapse=","),perf=auc,stringsAsFactors=FALSE))
        return(auc)
    }

    set.seed(123456)

    stopifnot(!missing(data) | !missing(group))
    if(missing(id))
        id  <- rownames(data)
    search.engine <- tolower(search.engine)
    stopifnot(search.engine %in% c('bfs','fws','bws','hcs','exh'))
    stopifnot(is.numeric(CV) | CV < 0)

    data    <- data[id,c(group$t,group$c)]
    label   <- c(rep(1,length(group$t)), c(rep(0,length(group$c))))
    feature <- id
    num.feature <- length(feature)
    if(eval == 'svm')
        evaluator <- as.name('SVM.classifier')
    else
        evaluator <- as.name('NB.classifier')
    ## performance 
    perf <- data.frame(subset=character(0), perf=numeric(0),stringsAsFactors=FALSE)

    if(search.engine == 'bfs'){
        subset <- best.first.search(attributes=feature,eval.fun=evaluator)
    }else if(search.engine == 'fws'){
        subset <- forward.search(attributes=feature,eval.fun=evaluator)
    }else if(search.engine == 'bws'){
        subset <- backward.search(attributes=feature,eval.fun=evaluator)
    }else if(search.engine == 'hcs'){
        subset <- hill.climbing.search(attributes=feature,eval.fun=evaluator)
    }else if(search.engine == 'exh'){
        subset <- exhaustive.search(attributes=feature,eval.fun=evaluator)
    }
    index <- perf[,'subset'] %in% paste(subset,collapse=',')
    return(perf[index,])
}
