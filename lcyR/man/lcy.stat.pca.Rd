\name{lcy.stat.pca}
\alias{lcy.stat.pca}
\title{
    Principal component analysis
}
\description{
    Principal component analysis.
}
\usage{
lcy.stat.pca(d, label, choices=1:2, output = "pca")
}
\arguments{
  \item{d}{
    a numeric data matrix where columns are observation and rows are features.
}
  \item{label}{
    a vector of labels corresponding to with the column names of data.
}
  \item{choices}{
    a vector of principal component to use to visualize. Length can be either 2 (default) or 3.
}
  \item{output}{
    a file name (pca) where pdf file will be generated if length of choices is 2 and output is not NULL. A animation (output.gif) will be generated if length of choices is 3 and output is not NULL.
    If output is NULL, figure is plotted in the screen.
}
}
\details{
    see princmp
}
\value{
    princmp object. A list with center, x, etc components.
}
\author{
    Chengyu Liu <chengyu.liu.cs@helsinki.fi>
}
\keyword{ pca }
