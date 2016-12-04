lcy.create.db.from.memory <- function(table, db.path, dbname, table.name, sep="\t", header=TRUE, index.key, db.new=TRUE, row.names=TRUE, addColumnName=""){
    # this uses SQLite database which should be installed.
    # index.key could not be something that are reserved in SQLite unless it is wrapped with ''.
    # If rownames of table is needed, it is set to primary key.
    # If rownames is not needed, a primary key should be given.
    # db.new for create new database or use current database given by dbname
    # sometiem "/" are missing causing errors
    if(substr(db.path, start=nchar(db.path), stop=nchar(db.path))!="/"){
        db.path <- paste(db.path,"/",sep="")
    }
    db  <- paste(db.path,dbname,sep="")
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
    if(row.names==TRUE){
        table              <- cbind(rownames(table),table)
        rownames(table)    <- NULL
        colnames(table)[1] <- addColumnName
    }
    require(sqldf)
    stm     <- paste("CREATE TABLE ",table.name," as select * from table",sep="")
    sqldf(stm,dbname=db,file.format=list(sep=sep,header=header))
    stm     <- paste("CREATE INDEX idx_", table.name," ON ",table.name," (",paste(index.key,collapse=","),")",sep="")
    sqldf(stm,dbname=db)
}

