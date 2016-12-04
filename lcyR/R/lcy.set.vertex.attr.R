lcy.set.vertex.attr <-
function(g,index=V(g)$name,attr){
    #-----
    # g should have a 'name' attribute which is unique
    # attr is a data frame or matrix, with colnames and rownames
    # index is the nodes which you want to add attributes
    #-----
    
    if (!is.igraph(g)) {
       stop("Not a graph object")
    }
    index       <- intersect(index, rownames(attr))
    i           <- match(V(g)$name, index)
    attr.names  <- colnames(attr)
    z           <- lapply(attr.names, function(x){
                            g <<- set.vertex.attribute(g, name=x, index=which(!is.na(i)), attr[index[i[!is.na(i)]],x])
                    })
    return(g)
}
