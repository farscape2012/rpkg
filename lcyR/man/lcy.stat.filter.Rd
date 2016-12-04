\name{lcy.stat.filter}
\alias{lcy.stat.filter}
\title{
    feature filtering.
}
\description{
    filter out features that are statistically weak or meanningless, using standard deviation, variance and median absolute deviation.
}
\usage{
lcy.stat.filter(data, byrow = TRUE, method = c("sd", "var", "mad",'mean','median'), top.N = 1000, cutoff)
}
\arguments{
  \item{data}{
    a numeric data frame or matrix. rows are features, columns are observations
}
  \item{byrow}{
    boolean value indicating whehther method is applied in row (TRUE default) or column.
}
  \item{method}{
    supported methods, 'sd', 'mad','var'
}
  \item{top.N}{
    select top.N number features from ranked features using method. When cutoff is provided and the number of features selected based on cutoff is larger than top.N, then the top.N features are selected.
}
  \item{cutoff}{
    a numeric values.
}
}
\details{
}
\value{
    return a numerici matrix or data.frame where only selected features are included.
}
\author{
    Chengyu Liu <chengyu.liu@helsinki.fi>
}
\keyword{ filter }
