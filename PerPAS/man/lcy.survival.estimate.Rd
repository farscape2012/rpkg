\name{lcy.survival.estimate}
\alias{lcy.survival.estimate}
\title{
    Survival analysis
}
\description{
    Used to do survival estimate using R survival package. Survival analysis requires clinical data (especially survival times and events) and group of patients.
}
\usage{
lcy.survival.estimate(clinic, group, col.time = 1, col.event = 2, label.event = "alive", time.out.limit = NULL, min.event = 5, main = "Survival Estimate", file = NULL, width = 8, height = 8)
}
\arguments{
  \item{clinic}{
    a data matrix of data frame which should contains rownames and colnames. rows are samples and columns are features.
}
  \item{group}{
    either a list of a vector. When it is a list, each element is a category of patients. The list can be named or numerical order is used when it is unnamed. It will automatically check the overlap between clinical samples and categories of patients and only overlap patients are used.
    When it is a vetor the order of patient should be the same as the row of clinic. They should be exactly matched.
}
  \item{col.time}{
    a character to indicate the column of survival times (followup times)
}
  \item{col.event}{
    a character to indicate the column of survival event (vital status)
}
  \item{label.event}{
    a character to specify the censored event such as 'alive', 'dead', etc.
}
  \item{time.out.limit}{
    to specify the cut of the time of survival analysis, such as 5- or 10-year survival analysis.
}
  \item{min.event}{
    a integer to specify minimum number of patients in each category.
}
  \item{main}{
    title of plot
}
  \item{file}{
    output filename. Default is NULL and plot to the screen.
}
  \item{width}{
    width of output file.
}
  \item{height}{
    height of output file.
}
}
\details{
}
\value{
    return a p-value calculated using log-rank test.    
}
\author{
    Chengyu Liu <chengyu.liu.cs@gmail.com>
}

\keyword{ survival }
\keyword{ estimate }
