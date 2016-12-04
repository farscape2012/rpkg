\name{lcy.composition.plot}
\alias{lcy.composition.plot}
\title{
    identify composition in terms of DEGs
}

\description{
    compute the number of up, down and differentially expressed genes in given data. 
}
\usage{
lcy.composition.plot(data, sample.id,fc=c(-1,1), file = "composition_plot.pdf", col = c("red", "green"), legend = c("UP", "DOWN"))
}
\arguments{
  \item{data}{ fold change matrix with colnames. sample.id should be included in colnames. 
}
  \item{sample.id}{ string vector specifying column which should be characteristized. 
}
  \item{fc}{ numberic vector of size two. values smaller than the first element in fc are considered are down, and values larger than the second element are up, between them are neutrally expressed.
}
  \item{file}{ plot barplot if file is not NULL.
}
  \item{col}{ color to fill in barplot
}
  \item{legend}{ legend for the colors
}
}
\value{
    return a matrix with three columns and length(sample.id) rows
}
\author{
    Chengyu Liu <chengyu.liu@helsinki.fi>
}
\keyword{ composition }
\keyword{ plot }
