\name{lcy.table.write}
\alias{lcy.table.write}
\title{
    write table
}
\description{
    write a table to a file
}
\usage{
    lcy.table.write(file, table, sep = "\t", append = FALSE, first.cell = "RowName",...)
}
\arguments{
  \item{file}{ file name to write table to
}
  \item{table}{ matrix or data frame
}
  \item{sep}{ delimeter to separate row elements
}
  \item{append}{ logic. if TRUE append at the end of file. if FALSE, overwrite the file.
}
  \item{first.cell}{ the first cell. the colname of the first row in the file. 
}
  \item{\dots}{ for parameter options for write.table.
}
}
\details{
    write a table to a file, it could append or overwrite the old file.
}
\author{
    Chengyu Liu \email{chengyu.liu@helsinki.fi}
}

\seealso{
    read.table, write.table, lcy.table.read
}
\keyword{ write }
\keyword{ table }
