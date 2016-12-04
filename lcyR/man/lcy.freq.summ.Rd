\name{lcy.freq.summ}
\alias{lcy.freq.summ}
\title{
    frequency summary
}
\description{
    It is used to count frequency of each element in the vector. Return named matrix with frequency and proportion 

}
\usage{
    lcy.freq.summ(vector, ignore.case = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vector}{ A vector or list of character or number. If a list is given, unlist the list and compute the frequency.
}
  \item{ignore.case}{ if vector is numeric and ignore.case is set to be TRUE, then vector is treated as string.
}
}
\value{
    A matrix  with frequency and proportion.
}
\author{
    Chengyu Liu <chengyu.liu@helsinki>
}
\keyword{ frequency }
