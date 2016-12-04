\name{lcy.table.collapse}
\alias{lcy.table.collapse}
\title{
    collpase duplicated rows
}
\description{
    used to remove duplicated rows for a column and collpase the other columns which have duplicated rows.
}
\usage{
lcy.table.collapse(table, column.id = 1, collapse = ",", first.occur.pick = TRUE, dup.rm = FALSE)
}
\arguments{
  \item{table}{
    data frame or matrix. it will be converted into data.frame. It should have column names if a character is provided for column.id instead of integer. 
}
  \item{column.id}{
    can be an integer or a character to specify which column is used to apply paste collapse function. Default the first column is used. It should have column names if a character is provided for column.id instead of integer.
}
  \item{collapse}{
    delimiter to split character (default ','). it is used when first.occur.pick is FALSE. 
}
  \item{first.occur.pick}{
    logic value indicating whether first occurred non NA value is used for the other columns. Default is TRUE, other columns are not collapsed but return the first non na value.
}
  \item{dup.rm}{
    a logic value. it is used when first.occur.pick is FALSE. duplicated rows are removed before collapsing.
}
}
\details{
}
\value{
    return a data.frame
}
\author{
    Chengyu Liu <chengyu.liu@helsinki.fi>
}
\keyword{ collapse }
\keyword{ table }
