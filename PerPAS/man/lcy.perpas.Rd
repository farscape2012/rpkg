\name{lcy.perpas}
\alias{lcy.perpas}
\title{
    Personalized Pathway Alteration analysiS pipeline
}
\description{
    A combination of perpas function to quantify pathway alteration at an individual patient resolution. It includes two steps; first standardization of data and quantification of pathway alteration. It supports customized pathway alteration in case users have their own pathway.
}
\usage{
lcy.perpas(paths, g, data, group = list(ctrl, treat), score.type = "APD", use = "none", min.neighbor = 3, database = "both", min.overlap = 0.3)
}
\arguments{
  \item{path}{
    a list of paths, each element is a path (i.e., a path = a,b,c,d). It can be missing. It is used to provide customized paths from a pathway. When both paths and g are missing, builtin pathway database will be used.
}
  \item{g}{
    an igraph object of pathway. It can be missing. It is used to provide customized paths from a pathway. When both paths and g are missing, builtin pathway database will be used.
}
  \item{data}{
    a data matrix which is standardized gene expression. It is standardized using lcy.tORz.score function.
}
  \item{score.type}{
    can be one of "APD" (default), "PDT" or "GS", representing different algorithms. See the reference article.
}
  \item{use}{
    can be "neighbor" or "none" (default). When it is "neighbor", it will use TIF (Topology impact factor) mechanism.  In details, it will use its downstream genes to represent its activity. 
}
  \item{min.neighbor}{
    it is valid when use="neighbor". Only genes which have larger than min.neighbor neighbors use TIF scheme and other gene use its own expression as its activity.
}
  \item{database}{
    can be one of "both" (default), 'pid' (Pathway Interaction Database) or 'wiki' (WikiPathways).
}
  \item{min.overlap}{
    a arbitary value between 0-1 indicating a minimum number of overlaps between genes from data  and genes from a pathway.
}
  \item{group}{
    a list with two components, ctrl and treat corresponding to two groups. The ctrl can be NULL. In that case the data is standardized to median of the data cohort. When both ctrl and treat are not NULL, the data is standardized to median and standard deviation of control samples.
}
}
\details{
    Currently, it is not possible to specify pathway in the builtin database. In canse users want to exact certain pathway from builtin pathway database, users can load the database builtin and select allShortestPaths from ALL.PATH.PID or ALL.PATH.WIKI where all paths have been generated. Or in case users want to get the pathway topology, users can exact igraph object of pathway from CANO.GRAPH.PID or WIKIPATH. If you got problem please contact the author (see below for the contact detail).

    The gene overlap between the pathway being quantified and the data set. If the ratio between the number of overlapped genes and the total number of genes in the pathway is smaller than given threshold (min.overlap), then NULL is return. 
}
\value{
    return either a vector or a data matrix depending on the inputs. If paths or g is provided, then a vector is returned otherwise a matrix.
}
\references{
}
\author{
    Chengyu Liu <chengyu.liu.cs@gmail.com>
}
\keyword{ perpas }
