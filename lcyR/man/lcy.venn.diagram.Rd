\name{lcy.venn.diagram}
\alias{lcy.venn.diagram}
\title{
    venndiagram
}
\description{ Used to construct venn diagrams using Erkka's modified script, compute.Venn.fix.r, in anduril component. He used Vennerable package.
}
\usage{
lcy.venn.diagram(Sets, file.name = NULL, Weight = NULL, SetNames = NULL, type = "ellipses", doWeights = FALSE, doEuler = FALSE, colourAlgorithm = "sequential", width = 120, height = 120, res = 300)
}
\arguments{
  \item{Sets}{ List. sets to be used to construct Venndiagram, when list is NOT null, weight and SetNames are ignored. Sets should have.
}
  \item{file.name}{ It will plot the venn diagram to screen (NULL). if it is file name, pdf file is generated.
}
  \item{Weight}{Vector. when list is not given, weight are directly used to construc Venndiagram
}
  \item{SetNames}{Vector. when list is not given, weight are directly used to construc Venndiagram. Both weight and SetNames are required, when list is not given.
}
  \item{type}{ string. One of "circles","squares","triangles","ellipses","AWFE","ChowRuskey","AWFE"
}
  \item{doWeights}{ logic. show Venndiagram is weighted.
}
  \item{doEuler}{ logic.
}
  \item{colourAlgorithm}{one of c('signature','binary','sequential')
}
  \item{width}{
}
  \item{height}{
}
  \item{res}{
}
}
\value{
    data frame.
}
\author{
    Chengyu Liu <chengyu.liu@helsinki.fi>
}
\keyword{ venndiagram }
