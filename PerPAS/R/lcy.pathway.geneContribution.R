lcy.pathway.geneContribution <- function(paths,plot=FALSE){
    paths   <- unlist(paths)
    table   <- table(unlist(paths))
    table   <- table/length(paths)
    if(plot){
        barplot(table,col='blue')
    }
    return(table)
}
