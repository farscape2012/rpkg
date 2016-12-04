lcy.initiation <- 
function(path='./'){
    if(file.exists(path)){
        unlink(path,recursive=TRUE, force=TRUE)
    }
    dir.create(path, recursive=TRUE,showWarnings=FALSE)
    setwd(path)
}

