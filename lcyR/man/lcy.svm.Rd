\name{lcy.svm}
\alias{lcy.svm}
\title{
    SVM
}
\description{
    super vector machine (SVM)
}
\usage{
    lcy.svm(data, label, k = 0)
}
\arguments{
  \item{data}{ a numeric matrix with features in row and samples in column
}
  \item{label}{ a binary vector, 0 is negative and 1 is positve classes.
}
  \item{k}{ a number showing which kind of validation is applied. k=0 or 1: independent validation, other is k-cross-validation.
}
}
\details{
    used package kernlab
}
\value{
    a list which has two attributes, prediction and label. This list can be used in lcy.ROCcurve to evaluate performance.
}

\author{
    Chengyu Liu \email{chengyu.liu@helsinki.fi}
}

\seealso{
    See Also as \code{lcy.ROCcurve}
}
\keyword{ prediction }
\keyword{ svm }
