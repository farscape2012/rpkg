lcy.table.read <-
function(file, sep='\t',rownames=TRUE, header=TRUE,...){
    stopifnot(is.character(file))
    stopifnot(length(file) == 1)
    table  <- read.table(file, sep=sep,header=header,check.names=FALSE,stringsAsFactors=FALSE,row.names=NULL,quote = "\"",...)
    if(rownames){
        rownames(table) <- table[,1]
        table           <- table[,-1,drop=FALSE]
    }
    return(table)
}
