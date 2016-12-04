\name{lcy.set.operation}
\alias{lcy.set.operation}
\title{
    set operations
}
\description{
    set operations, including union, intersect, setdiff and setequal. these functions does not support more than two inputs.
}
\usage{
lcy.set.operation(..., func = "intersect")
}
\arguments{
  \item{\dots}{ two or more vectors.
}
  \item{func}{ function names, 'union', 'intersect', 'setdiff' and 'setequal'
}
}
\value{
    a vector.
}
\author{
    Chengyu Liu<chengyu.liu@helsinki.fi>
}
\keyword{ intersect }
\keyword{ union }
\keyword{ setdiff }
