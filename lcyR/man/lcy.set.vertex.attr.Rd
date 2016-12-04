\name{lcy.set.vertex.attr}
\alias{lcy.set.vertex.attr}
\title{
    set vertex attribute
}
\description{
    set vertex attribute
}
\usage{
lcy.set.vertex.attr(g, index = V(g)$name, attr)
}
\arguments{
  \item{g}{
    an igraph
}
  \item{index}{ a vector of characters which is match with name attribute of g. default values are all the nodes in the g.
}
  \item{attr}{ a matrix of data frame which have rownames and colnames. rownames should partially overlap with index. colnames will be used as new attributes of graph
}
}
\details{
    let see
}
\value{
    igraph object
}
\author{
    chengyu.liu@helsinki.fi
}
\keyword{ set }
\keyword{ attribute }
\keyword{ vertex }% __ONLY ONE__ keyword per line
