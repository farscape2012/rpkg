\name{lcy.diff.comparison}
\alias{lcy.diff.comparison}
\title{
    differentiation comparison
}
\description{
    compare the differentiation among groups. t-test is applied to check significance. There could be more than 2 groups and it is possible to apply pairwise t-test. and p-value could be adjusted (default 'bonferroni').
}
\usage{
    lcy.diff.comparison(data, group, pair.wise = FALSE, p.adjust.method='bonferroni',fc.label = c(t = "treat", c = "ctrl"))
}
\arguments{
  \item{data}{ numeric matrix.
}
  \item{group}{ matrix with two columns,the first column is sample names, the same with colnames of data, the second column is label for samples.
}
  \item{pair.wise}{ logic, whether pair.wise test should be applied or not.
}
  \item{p.adjust.method}{ adjusting methods, "holm","hochberg","hommel","bonferroni","BH","BY","fdr","none". see p.adjust
}
  \item{fc.label}{ used to compute fold change in case of two labels. if pair.wise is TRUE, this is ignored.
}
}
\value{
    a list if pair.wise is TRUE. if pair.wise is false and there are only two labels, then return a matrix, otherwise return a vector.
}
\author{
    Chengyu Liu <chengyu.liu@helsinki.fi>
}
\keyword{ comparison }
\keyword{ differentiation }
