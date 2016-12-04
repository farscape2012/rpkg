\name{lcy.feature.selector}
\alias{lcy.feature.selector}
\title{
    feature selection
}
\description{
    uses different scheme to select feature set which gives local or global optimal AUC.
}
\usage{
    lcy.feature.selector(data, group = list(t, c), id, search.engine = "bfs", CV = 5)
}
\arguments{
  \item{data}{ A numeric matrix. Row is feature and column is sample. If rownames is NULL, id must provided.
}
  \item{group}{ A list with two attributes, t and c representing two different group. Currently only binary classification is allowed.
}
  \item{id}{ A vector of characters or index of features. It could be missing when rownames of data is NOT NULL.
}
  \item{search.engine}{ One of "bfs", "fws", "bws", "hcs", "exh". It could be extended. See available methods in Fselector package.
}
  \item{CV}{ An integer to specify validation scheme. If CV=0/1, independent validation, CV > 1, CV-cross-validation.
}
}
\value{
    a data.frame with two columns, subset and perf. subset is the selected feature (string) and perf (numeric) and the corresponding AUC.
}
\author{
    Chengyu Liu <chengyu.liu@helsinki.fi>
}


\seealso{
    See Also as \code{FSelector},\code{lcy.svm},\code{lcy.ROCcurve}
}
\keyword{ feature }
\keyword{ selector}
