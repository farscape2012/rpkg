\name{lcy.ROCcurve}
\alias{lcy.ROCcurve}
\title{
    plot ROC curve and return auc.
}
\description{
    evaluate performance of a classifier.
}
\usage{
    lcy.ROCcurve(score, label, multiline = TRUE, main = NULL, plot = TRUE, auc.threshold = 0.6)
}
\arguments{
  \item{score}{ a score of classification, for instance x$prediction where x was returned from lcy.svm
}
  \item{label}{ a label of correct classification, for instance x$label where x was returned from lcy.svm
}
  \item{multiline}{ a logic value
}
  \item{main}{ a title of plot
}
  \item{plot}{ boolean value showing whether ROC curve is plotted or not.
} 
  \item{auc.threshold}{ ROC curve whose auc is larger than auc.threshold is plotted.
}
}
\author{
    Chengyu Liu \email{chengyu.liu@helsinki.fi}
}

\seealso{
    See Also as \code{lcy.svm}
}
\keyword{ roc }
\keyword{ curve }
