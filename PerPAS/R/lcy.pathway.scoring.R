lcy.pathway.scoring <-
function(paths, g, data, score.type='APD', use='none', min.neighbor=3, database='both',min.overlap=0.3){
    if(score.type == 'APD'){
        type <- 'ASP'
        abs <- TRUE
    }else if(score.type == 'PDT'){
        type <- 'ASP'
        abs <- FALSE
    }else if(score.type == 'GS'){
        type <- 'GS'
        abs <- FALSE
    }else{
        stop('score.type is wrong')
    }
    data('PerPAS')
    scoring.a.pathway <- function(paths,g,data,type='GS'){
        if(missing(paths)){
            if(missing(g)){
                stop("Both path and graph are missing")
            }else{
                if(type == 'GS') {## gene set without topology
                    paths    <- V(g)$name
                }else if(type == 'ASP'){
                    paths    <- lcy.graph.allShortestPath(g)
                }
            }
        }else{
            if(type == 'GS') {## gene set without topology
                paths    <- unique(unlist(paths))
            }else if(type == 'ASP'){
                paths    <- paths
            }
        }
        paths       <- unlist(paths)
        rownames    <- rownames(data)
        if(use == 'neighbor'){ # use neighbors to score the genes
            if(missing(g)){
                stop("graph responding to path was missing")
            }else{
                data    <- scoring.with.neighbor(g=g,min.neighbor=min.neighbor)
            }
        }else if(use =='none'){
        
        }else{
            warnings('use paramter is not either "neighbors" or "none"')
        }
        paths.len   <- length(paths)
        paths       <- paths[paths %in% rownames]
        if(length(paths)/paths.len <= min.overlap){
            print(sprintf('the number of genes overlapped pathway is smaller than %s%%', min.overlap*100))
            return(NULL)
        }
        if(length(paths) == 0){
            y   <- rep(NA,ncol(data))
        }else{
            if(is.null(data)){
                return(NULL)
            }
            y   <- colMeans(data[paths,,drop=FALSE],na.rm=T)
        }
        #print('in')
        if(sum(is.na(y)) >= 0.8*ncol(data) | sum(is.nan(y)) >= 0.8 * ncol(data)){
            return(NULL)
        }else{
            return(y)
        }
    }
    scoring.with.neighbor <- function(g,min.neighbor=2){
        names           <- V(g)$name
        rownames        <- rownames(data)
        #names           <- names[names %in% rownames]
        tmp             <- lapply(1:length(names), function(x) {
                            ids <- neighbors(g, v=x, mode ='out')$name
                            ids <- intersect(ids, rownames)
                            if(length(ids) <= min.neighbor){
                                if(names[x] %in% rownames){
                                    return(data[names[x],, drop=FALSE])
                                }else{
                                    return(rep(NA,ncol(data)))
                                }
                            }else{
                                return(colMeans(data[ids,,drop=FALSE],na.rm=T))
                            }
        })
        data            <- do.call(rbind,tmp)
        rownames(data)  <- names
        index           <- colSums(apply(data,1,is.na)) == ncol(data)
        data            <- data[!index,,drop=FALSE]
        return(data)
    }
    if(abs){
        data    <- abs(data)
    }
    if(missing(paths) & missing(g)){
    #### Score all the pathways
        if(database == 'pid'){
            pathways    <- CANO.GRAPH.PID
            paths       <- ALL.PATH.PID
        }else if(database == 'wiki'){
            pathways    <- WIKIPATH
            paths       <- ALL.PATH.WIKI
        }else{
            pathways    <- c(CANO.GRAPH.PID, WIKIPATH)
            paths       <- c(ALL.PATH.PID, ALL.PATH.WIKI)
        }
        tmp <- lapply(1:length(pathways), function(x){
                    return(scoring.a.pathway(paths=paths[[x]], g=pathways[[x]], data=data, type=type))
        })
        index   <- unlist(lapply(tmp, is.null))
        score   <- do.call(rbind, tmp)
        names   <- lapply(1:length(pathways), function(x){
                        get.graph.attribute(pathways[[x]],name='pathwayName')
                    })
        rownames(score) <- unlist(names)[!index]
    }else{
        if(missing(paths) & !missing(g)){
            score   <- scoring.a.pathway(g=g, data=data, type=type)
        }else{
            score   <- scoring.a.pathway(paths=paths, g=g, data=data, type=type)
        }
    }
    return(score)
}
