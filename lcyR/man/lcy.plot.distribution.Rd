\name{lcy.plot.distribution}
\alias{lcy.plot.distribution}
\title{
    Plot distribution
}
\description{
    Plot distribution of multiple categories. It supports maximum 3 categories. 
}
\usage{
lcy.plot.distribution(data, group = list())
}
\arguments{
  \item{data}{
    a numeric vector and it should be named. The name should be partially overlap with group.
}
  \item{group}{
    a list of sample categories. It supports maximum 3 categories. The reason for maximum 3 is that it would be messy if there are too many categories. It's difficult to see.
}
}
\details{
}
\value{
    a plot 
}
\author{
    Chengyu Liu <chengyu.liu.cs@gmail.com>
}
\keyword{ distribution }
\keyword{ plot }
