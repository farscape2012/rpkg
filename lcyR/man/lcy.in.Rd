\name{lcy.in}
\alias{lcy.in}
\title{
    wrap of in
}
    
\description{
    Check whether elements are in the vector.
}
\usage{
    lcy.in(..., list)
}
\arguments{
  \item{\dots}{
    a charactor/numeric vector or elements that are checked in the list.
}
  \item{list}{
    a vector of charactor where elelment are checked.
}
}
\details{
    charactor or numeric value can be checked. Any kind of elements that can be checked using 'in' function can be checked their existence.
}
\value{
    a matrix with two columns named tf and index. 'tf' column is binary values showing TRUE or FALSE. index column shows the index of each element in the list. index column can be NA showing they do not exist in the list.
}
\author{
    Chengyu Liu \email{chengyu.liu@helsinki.fi}
}
\seealso{
    \%in\% expression.
}
\keyword{ in }
\keyword{ exist }
