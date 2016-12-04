lcy.graph.allShortestPath <- function(g, dup=TRUE){
    # start with ranked nodes based on out-degree (degree > 0).
    # graph should have name attribute for vertices which are ids and used to map expression.

    start   <- names(which(sort(igraph::degree(g=g, mode='out'),decreasing=T)>=1))
    nodes   <- V(g)$name
    ids     <- 1:vcount(g)
    start   <- which(nodes %in% start)

    path <- list()
    cnt <- 1
    tmp <- lapply(start, function(x) {
                tmp <- lapply(ids[!ids %in% x], function(y) {
                        a   <- get.all.shortest.paths(graph=g,from=x, to=y,mode='out')$res
                        len <- length(a)
                        tmp <- lapply(a, function(z){
                                z <- as.numeric(z)
                                len <- length(z)
                                if(len>2){
                                    z   <- c(z[1], rep(z[2:(len-1)], rep(2,len-2)), z[len])
                                }
                                if(!dup){
                                    z   <- unique(z)
                                }
                                path[[cnt]] <<- z
                                cnt <<- cnt + 1;
                        })
                })
            })
    name.path <- lapply(path, function(x) {
                nodes[x]
    })
    return(name.path)
} 
