\name{lcy.aheatmap}
\alias{lcy.aheatmap}
\title{
    plot heatmap
}
\description{
    It is used to plot heatmap. it supports multiple color bars and two heatmaps in the same figure.
}
\usage{
lcy.aheatmap(data, clinic, col='-RdBu:100', scale='none', annCol=NA, annRow=NA, distfun=function(x) as.dist((1-cor(t(x))/2)), Rowv = "correlation", Colv="man", hclustfun='ward.D', breaks=seq(quantile(data, 0.05), quantile(data, 0.95),length.out=100), cexRow=0.7, cexCol=0.5, main="heatmap", fontsize=7, height=10, width=14,pdf=NULL,labCol=NULL, labRow=NULL)
}
\arguments{
  \item{data}{
    a n by m numeric matrix where n is gene and m is samples. it should have colnames and rownames.
}
  \item{clinic}{
    a data frame or matrix. it should have rownames which match with colnames of data. Only intersection are used. at most 9 different classes are allowed for each clinical feature. When there are more than 9, it does not considered.
}
  \item{col}{
    a string of colour specification for the heatmap.  Default to palette '-RdYlBu2:100', i.e. reversed palette 'RdYlBu2' (a slight modification of RColorBrewer's palette 'RdYlBu') with 100 colors. Possible values are:
    \describe{
        \item{one of RColorBrewer's palette name}{ 'RdYlBu2', 'rainbow', 'heat', 'topo', 'terrain', 'cm', 'gray', 'grey', 'BrBG', 'PiYG', 'PRGn', 'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent', 'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3', 'Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges', 'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds', 'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd'.}
    }
}
  \item{scale}{
    one of 'row' (default),'column' or 'none'.
}
  \item{annCol}{
    a list to define the annotation of each sample.
}
  \item{annRow}{
    a list to define the annotation of each feature.
}
  \item{distfun}{
}
  \item{Rowv}{
    default 'correlation', can be NULL, TRUE or NA
}
  \item{Colv}{
    default 'man', can be NULL, TRUE or NA
}
  \item{hclustfun}{
    a character (average,ward,single,median, complete,mcquitty,centroid,ward.D,ward.D2)
}
  \item{breaks}{
    breaks see aheatmap, the number of breaks should be matched with the number of colors default colors are 12 (RdBu). Example: col=RdBu:100, breaks=seq(quantile(data, 0.05), quantile(data, 0.95),length.out=100)
}
  \item{cexRow}{
    cex for row (0.7 default)
}
  \item{cexCol}{
    cex for col (0.5 default)
}
  \item{main}{
    title
}
  \item{fontsize}{
  font size positve value
}
  \item{pdf}{
    if it is not NULL write to the file pdf refers to. it will plot addition heatmap produced by heatmap.2 function
}
  \item{width}{
    width of pdf
}
  \item{height}{
    height of pdf
}
}
\details{
    see details in aheatmap in NMF package
}
\value{
    an object returned by aheatmap. Contains Colv, Rowv, Indr, and Indc.
}
\author{
    Chengyu Liu <chengyu.liu.cs@gmail.com>
}
\keyword{ aheatmap }
\keyword{ heatmap }
