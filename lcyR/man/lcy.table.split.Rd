\name{lcy.table.split}
\alias{lcy.table.split}
\title{
    split each element of a given column and duplicate the rows.
}
\description{
    Split each row of string given the column, and duplicate the rows. splited values are filled in the new rows of the column
}
\usage{
lcy.table.split(table, column.id = 1, split = ",")
}
\arguments{
  \item{table}{
    data.frame or matrix. It should have column names if a character is provided for column.id instead of integer. If there are rownames the rownames will be gone.
}
  \item{column.id}{
    can be an integer or a character to specify which column is used to apply strsplit function. Default the first column is used. It should have column names if a character is provided for column.id instead of integer.
}
  \item{split}{
    delimiter to split character (default ','). 
}
}
\details{
}
\value{
    return a data frame or matrix
}
\author{
    Chengyu Liu <Chengyu.liu@helsinki.fi>
}

\keyword{ strsplit }
\keyword{ split }
\keyword{ table }
