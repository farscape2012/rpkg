\name{lcy.classifier.pamr}
\alias{lcy.classifier.pamr}
\title{
    pam classifier trainer
}
\description{
    build a classifier using pam method. it is used to train a pam classifier which is further used by lcy.pam.predict function. It supports multi-classes.
}
\usage{
lcy.pam.train(data, label, nfold = 10, nboot = 100, err.cutoff = 0.02, n.threshold = 30, err.pdf = TRUE, thresh)
}
\arguments{
  \item{data}{
    a data matrix where rows are genes and columns are observations. In case there are not rownames, rownames are assigned. The rownames are pasted using "Row_" and row indices.
}
  \item{label}{
    a vector of charactors or numbers or factors. It can be multi-classes.
}
  \item{nfold}{
    n fold cross validation is applied.
}
  \item{nboot}{
    an integer of number of boot
}
  \item{err.cutoff}{
    a numeric value, specifying which threshold should be selected. The first threshold where the mean error rate of cross-validation is smaller than err.cutoff.
}
  \item{n.threshold}{
     Number of threshold values desired (default 30). It is used in pamr.train function
}
  \item{err.pdf}{
    logic value indicating whether boxplot of error rate are plotted or not.
}
  \item{thresh}{
    if thresh is not missing, use this specific threshold index to select signature. Otherwise, automatically select one.
}
}
\details{
The expression data of the retained most predictive genes were trained by PAM to build a robust classifier. To select the optimal threshold for centroid shrinkage, we performed nfold cross-validation over a range of shirnkage thresholds for nboot iterations, and selected the one yielding a good performance (error rate < err.cutoff \%) with the least number of genes.
}
\value{
    return a list of signature,pam.rslt,thresh,err,cents.
    \describe{
        \item{signature}{ a vector of characters which are selected as predictive markers. These characters match with rownames of data}
        \item{pam.rslt}{an list of training results returned by pamr.train}
        \item{thresh}{the selected shrinkage threshold}
        \item{err}{a matrix of cross validation error rates for different shrinkage thresholds}
        \item{cents}{a numeric matrix of PAM centroids for three subtypes}
    }
}
\author{
    Chengyu Liu <chengyu.liu@helsinki.fi>
}
\keyword{ pam }
\keyword{ train }
