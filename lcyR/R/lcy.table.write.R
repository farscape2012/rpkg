lcy.table.write <-
function(file,table,sep='\t',append=FALSE,first.cell="RowName",rownames=FALSE,...){
    stopifnot(is.character(file))
    stopifnot(length(file) == 1)
    if(append)
        open.mode   <- "ab"
    else
        open.mode   <- "wb"
    output.file     <- file(path.expand(file), open=open.mode)
    if (ncol(table) > 0) {
        if (!is.null(rownames(table))) {
            if(rownames == T)
                table   <- data.frame(first.cell=rownames(table), table)
            else
                rownames(table) <- NULL
        }
        write.table(table, output.file, sep=sep,eol='\n',row.names=FALSE,col.names=TRUE,...)
    }
    close(output.file)
}
