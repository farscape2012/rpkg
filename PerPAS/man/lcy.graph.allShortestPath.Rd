\name{lcy.graph.allShortestPath}
\alias{lcy.graph.allShortestPath}
\title{
    enumerate all shortest paths given a graph or a graph list
}
\description{
    used to enumerate all shortest paths. Currently it is used for directed graph. It can be used for undrected graph with simple modification.
}
\usage{
lcy.graph.allShortestPath(g)
}
\arguments{
  \item{g}{
    an igraph or a list of igraphs which must have an attribute 'name' for vertex. it will be used as id for the node which can be further used to map to other identifiers (for instance gene)
}
}
\value{
    a list of path for each igraph object. 
}
\author{
    Chengyu Liu <chengyu.liu@helsinki.fi>
}

\keyword{ lcy }
\keyword{ graph }
\keyword{ all }
\keyword{ shortest }
\keyword{ path }
