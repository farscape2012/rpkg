\name{lcy.create.db.from.file}
\alias{lcy.create.db.from.file}
\title{
    create a table from file
}
\description{
    create a table which is saved in a file.
}
\usage{
lcy.create.db.from.file(file.path, db.path, dbname, table.name, sep = "\t", header = TRUE, index.key, db.new = TRUE)
}
\arguments{
  \item{file.path}{
    A character which specifies the file where table is saved which going to be created in the dbname
}
  \item{db.path}{
    A character where user specifies the location of database (SQLite database)
}
  \item{dbname}{
    A character of database name. When db.new is set to be TRUE, new dbname will be created and the old one will be deleted if it exists.
}
  \item{table.name}{
    A character of table name which is used in the database.
}
  \item{sep}{
    A character of separator used in the file
}
  \item{header}{
    A logic value showing whether header exists
}
  \item{index.key}{
    A character which filed is used for indexing. 
}
  \item{db.new}{
    A logic value showing whether new database (SQLite) will be created.
}
}
\details{
}
\value{
    no return
}
\author{
}
\keyword{ create }
\keyword{ database }
