\name{lcy.misc.select.marker}
\alias{lcy.misc.select.marker}
\title{
    Marker selection
}
\description{
    Given a label, identify markers which classify obs. It is used different scheme compared with lcy.feature.selector.
}
\usage{
lcy.misc.select.marker(d, label, qvalue = 20, pvalue = 0.05, aucth = 0.9, err.cutoff = 0.1, marker.unique = FALSE, marker.call = "pam", output = "select.marker")
}
\arguments{
  \item{d}{
    a numeric data matrix or data frame. A lot of zero for rows are not allowed. It causes errors
}
  \item{label}{
    a vector of class label for each obs. the order should be match with colnames of "d"
}
  \item{qvalue}{
    a q value specify FDR which is used in samr method.
}
  \item{pvalue}{
    a p value specify permutation pvalue which is used in samr method.
}
  \item{aucth}{
    a AUC value which specify the minimal threshold to select predictive markers. 
}
  \item{err.cutoff}{
    a error rate used in pam method.
}
  \item{marker.unique}{
    logical value specify whether unique marker for each class is used. Default is FALSE.
}
  \item{marker.call}{
    a method name to specify to use.
}
  \item{output}{
    a folder where the results are output. it will create/overwrite new folder and two figures are plotted.
}
}
\details{
}
\value{
    return a a vector of marker names.
}
\author{
    Chengyu Liu <chengyu.liu.cs@gmail.com>
}

\keyword{marker }
\keyword{select }
