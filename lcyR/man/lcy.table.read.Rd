\name{lcy.table.read}
\alias{lcy.table.read}
\title{
    read file
}
\description{
    read table from a file.
}
\usage{
    lcy.table.read(file, sep = "\t", rownames = TRUE, header=TRUE,...)
}
\arguments{
  \item{file}{ file name to be read
}
  \item{sep}{ delimeter to separate each element in the row
}
  \item{rownames}{ logic value. if TRUE the data in the file contains rownames. 
}
  \item{header}{ logic value. if TRUE(default) the data in the file contains header. 
}
  \item{\dots}{ for parameter options for read.table.
}
}
\details{
    the data in the file always are always considered to have a header row at the beginning.
}
\value{
    a data frame. each variable could NOT be factor.
}
\author{
    Chengyu Liu \email{chengyu.liu@helsinki.fi}
}
\seealso{
    read.table, write.table, lcy.table.write
}
\keyword{ read }
\keyword{ table }
