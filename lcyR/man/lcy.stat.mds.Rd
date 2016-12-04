\name{lcy.stat.mds}
\alias{lcy.stat.mds}
\title{
    Classical multidimensional scaling
}
\description{
    Classical multidimensional scaling of a data matrix.  Also known as _principal coordinates analysis_ (Gower, 1966).
}
\usage{
lcy.stat.mds(d, label, k = 2, dist.method="euclidean", eig = FALSE, output = "mds.pdf")
}
\arguments{
  \item{d}{
    a numeric data matrix where columns are observation and rows are features.
}
  \item{label}{
    a vector of labels maching with the column names of data.
}
  \item{k}{
    the maximum dimension of the space which the data are to be represented in; must be in {1, 2, ..., n-1}.
}
  \item{dist.method}{
    a character specifying dist method. One of "euclidean","maximum", "manhattan", "canberra", "binary" or "minkowski".
}
  \item{eig}{
    logical value indicates whether eigenvalues should be returned.
}
  \item{output}{
    a file name (mds.pdf) where pdf file outputs. it always output a 2-D figure
}
}
\details{
    Multidimensional scaling takes a set of dissimilarities and returns a set of points such that the distances between the points are approximately equal to the dissimilarities.  (It is a major part of what ecologists call ‘ordination’.)
}
\value{
    a matrix with up to ‘k’ columns whose rows give the coordinates of the points chosen to represent the dissimilarities.
}
\author{
    Chengyu Liu <chengyu.liu.cs@helsinki.fi>
}
\keyword{ mds }
\keyword{ pca }
