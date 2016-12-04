\name{lcy.misc.filter.marker}
\alias{lcy.misc.filter.marker}
\title{
    fitler features
}
\description{
    fitler features which has less predictive power than given threshold.
}
\usage{
lcy.misc.filter.marker(data, label, aucth = 0.9, unique = FALSE)
}
\arguments{
  \item{data}{
    numeric matrix (rows genes, columns observation)
}
  \item{label}{
    a vector of classes.
}
  \item{aucth}{
    AUC threshold. it should be not larger than 1.
}
  \item{unique}{
    if unique is set to TRUE, uniqe markers for each cluster are returned. If some markers are significant for more than two groups, it will not included.
}
}
\details{
Used to identify markers which are predictive or specific for a subtype. For each gene, the predictive power is evaluated for each subtype. When unique is FALSE, the retured genes are predictive for at least a subtype (maybe more). NOTE when unique is set to TRUE, it will return genes which are specific for a cluster which means if a gene is significant in two clusters compared with the others, then it will be returned. Since it is NOT specific for a cluster. If label does not have names, then the label order should be the same as the data.

AUC is used to estimate the predictive performance of each gene.
}
\value{
    return a vector of features ( genes)
}
\author{
    Chengyu Liu <chengyu.liu.cs@gmail.com>
}
\keyword{ filter }
\keyword{ classifier }
