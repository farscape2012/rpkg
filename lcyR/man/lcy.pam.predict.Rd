\name{lcy.pam.predict}
\alias{lcy.pam.predict}
\title{
    predict using pam methods
}
\description{
    Predict new data given a fit object returned by lcy.pam.train
}
\usage{
lcy.pam.predict(data, fit, postRth = 1)
}
\arguments{
  \item{data}{
    a numeric matrix (rows =genes, columns = observation)
}
  \item{fit}{
    a list returned by lcy.pam.train
}
  \item{postRth}{
    a numeric value specifying the cutoff of posterior odds in favor of optimal subtype to the other two.

}
}
\details{
    scripts are patially from "DeSousa2013" packge in bioconductor.
}
\value{
    a list of two components, class and prob. class is discrete values of class and prob is probabity.
}
\author{
    Chengyu Liu <chengyu.liu.cs@gmail.com>
}
\keyword{ pam }
\keyword{ classifier }
