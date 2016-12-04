\name{lcy.stat.remove.outliers}
\alias{lcy.stat.remove.outliers}
\title{
    Remove outliers
}
\description{
    Replace outliers with give quantile value. Assume data points are normally distributed.
}
\usage{
lcy.stat.remove.outliers(d, quantile = 0.001)
}
\arguments{
  \item{d}{
    a numeric vector or data matrix or data frame.
}
  \item{quantile}{
    a quantile value smaller than 1.
}
}
\value{
    the same data type to the input data.
}
\author{
    Chengyu Liu <chengyu.liu.cs@gmail.com>
}
\keyword{ remove }
\keyword{ outliers }
