\name{lcy.row.join}
\alias{lcy.row.join}
\title{
    Join/Combin/Merge duplicate id rows.
}
\description{
    Join/Combin/Merge duplicate id rows. if there are numeric columns which have duplicate id rows, such rows can be combind by mean or median value
}
\usage{
lcy.row.join(data, id.column = 1, method = c("mean", "median", "none"), join.column = NULL, include = NULL)
}
\arguments{
  \item{data}{
    data can be numerici matrix, or data frame which have numeric columns.
}
  \item{id.column}{
    the column id or index which is used to specify which column is the key to identify duplicated rows
}
  \item{method}{
    charater of "mean", "median", or "none" (default). mean or median will be applied to the columns specified in 'join.column'. When it is n
}
  \item{join.column}{
    column index of column names indicating which columns should be combined by duplicated row
}
  \item{include}{
    column index of column names indicating which columns should be outputed in the output table
}
}
\details{
    see,,
}
\value{
    return a data from or a matrix
}
\author{
    Chengyu Liu <chengyu.liu@helsinki.fi>
}

\keyword{ join }
\keyword{ row }
