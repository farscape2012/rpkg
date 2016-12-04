\name{lcy.nmf.survey}
\alias{lcy.nmf.survey}
\title{
    Determine the number of clusters
}
\description{
    generic defines the main interface to run NMF algorithms within the framework defined in package ‘NMF’. It has many methods that facilitates applying, developing and testing NMF algorithms.

}
\usage{
lcy.NMF.survey(d, num.genes = seq(1, 10, 2) * 1000, method = "brunet", rank = 2:6, seed, .options='vp6')
}
\arguments{
  \item{d}{
    none negative matrix. normally the data can be transformed into 2^x.
}
  \item{rank}{
    a integer which is the number of cluster
}
  \item{method}{
    a character specifying the method used to cluster samples. Possible values are 'brunet' (default), 'lee', 'ns', 'nsNMF'.
}
  \item{num.genes}{
    a vector of numbers specifying how many genes is used to survy cluster consensus.
}
  \item{seed}{
    an integer or one of 'random'(default),'none','ica','nndsvd'
}
  \item{.options}{
    a character to specify paramter for nmf function. default is 'vp6' which means parallelization with 6 cores.  'vp' means as many cores as possible
}
}
\details{
    This function will create a folder and output results into that folder. 
}
\value{
    a list returned by nmf function. most of results are plotted into figures.
}
\author{
}

\keyword{ nmf }
\keyword{ survey }
