\name{lcy.tORz.score}
\alias{lcy.tORz.score}
\title{
    t- or z-score calculation
}
\description{
    used to compute t- or z-score.
}
\usage{
lcy.tORz.score(data, group = list(ctrl, treat), type = "zscore", method='median',pvalue = FALSE,byrow=FALSE)
}
\arguments{
  \item{data}{
    a numeric data matrix 
}
  \item{group}{
    a list with two components, ctrl and treat corresponding to two groups. When group is null, then standard zscore transformation is applied. centering is done by subtracting the column means (omitting ‘NA’s) of ‘x’ from their corresponding columns, scaling is done by dividing the (centered) columns of ‘x’ by their standard deviations.
}
  \item{type}{
    a string which can be 'zscore' or 'tscore'
}
  \item{method}{
    a string of "median"(default) or "mean", specifying median- or mean-centered.
}
  \item{pvalue}{
    logical value indicating whether pvalue are calculated given t- or z-score.
}
  \item{scale}{
    logical value indicating whether the column is scaled or not when group is NULL.
}
  \item{byrow}{
    logical value indicating whether data is scaled by row or by column (default FALSE). it is used only if group is NULL.
}
}
\details{

}
\value{
    a vector of double value which has the same size of column of the data.
}
\author{
    Chengyu Liu <Chengyu.liu@helsinki.fi>
}

\keyword{ z-score }
\keyword{ t-score }
