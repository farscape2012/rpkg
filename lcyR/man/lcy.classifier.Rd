\name{lcy.classifier}
\alias{lcy.classifier}
\title{
    naive bayesian and SVM classifiers
}
\description{
    used to train and classify samples and return an object which is the return of function "predict".
}
\usage{
lcy.classifier(data, label, k = 0, classifier = "naiveBayes")
}
\arguments{
  \item{data}{
    an numeric matrix used to train and test classification
}
  \item{label}{
    a vector of characters which corresponds to columns of data.
}
  \item{k}{
    cross-validation parameters. independent validation when k <= 1, otherwise k-cross-validation will be applied.
}
  \item{classifier}{
    used to specify the classifier. currently "naiveBayes" and "svm" can be used.
}
}
\details{
    for svm classifier, only binary classification can be used.
}
\value{
    return an object of function "predict". The returned value can be the input of lcy.ROC to produce ROC curve.
}
\author{
    Chengyu Liu <chengyu.liu@helsinki.fi>
}
\keyword{ classifier }
\keyword{ predict }
