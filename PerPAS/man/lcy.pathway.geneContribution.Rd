\name{lcy.pathway.geneContribution}
\alias{lcy.pathway.geneContribution}
\title{
    Quantify gene contribution
}
\description{
    Quantify contribution of each gene to a given pathway. The contribution is calculated by the fraction of paths going through a genes over all the paths.
}
\usage{
lcy.pathway.geneContribution(paths, plot = FALSE)
}
\arguments{
  \item{paths}{
    a list of paths, each component is a path (i.e., a,b,c,d means a path from gene a to d). The paths object can be obtained by running lcy.graph.allShortestPath function.
}
  \item{plot}{
    logic value indicating whether a barplot is produced.
}
}
\details{
}
\value{
    return a table() object, each element is quantification of each gene.
}
\author{
    Chengyu Liu <chengyu.liu.cs@gmail.com>
}
\keyword{ gene }
\keyword{ contribution }
