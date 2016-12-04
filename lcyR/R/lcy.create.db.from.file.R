lcy.create.db.from.file <- function(file.path, db.path, dbname, table.name, sep="\t", header=TRUE, index.key, db.new=TRUE){
    # this uses SQLite database which should be installed.
    # index.key could not be something that are reserved in SQLite unless it is wrapped with ''.
    
    if(!file.exists(file.path)){
        stop("Input 'file.path' does not exist")
    }
    # sometiem "/" are missing causing errors
    if(substr(db.path, start=nchar(db.path), stop=nchar(db.path))!="/"){
        db.path <- paste(db.path,"/",sep="")
    }
    # database path
    db  <- paste(db.path, dbname, sep="")
    require(sqldf)
    if(db.new==TRUE){
        if(file.exists(db)){
            unlink(db)
        }
        stm <- paste("attach '",db,"' as new",sep="")
        sqldf(stm)
    }else{
        if(!file.exists(db)){
            stop(sprintf("file dbname %s in %s does not exist", dbname, db.path ))
        }
    }
    f       <- file(file.path)
    #print(sqldf('select * from f',file.format=list(sep=sep,header=header)))
    stm     <- paste("CREATE TABLE ", table.name," as select * from f",sep="")
    sqldf(stm,dbname=db,file.format=list(sep=sep,header=header))
    stm     <- paste("CREATE INDEX idx_",table.name," ON ",table.name," (",paste(index.key, collapse=","),")",sep="")
    sqldf(stm,dbname=db)
    close(f)
}

