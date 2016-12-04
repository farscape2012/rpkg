\name{lcy.nmf.cluster}
\alias{lcy.nmf.cluster}
\title{
    cluster samples
}
\description{
    Used to identify signature which best separates samples into given clusters. First nmf is used to decide cluster members and then sam, pam are used to identify optimal signature.
}
\usage{
lcy.NMF.cluster(d, clinic, rank = 3, method = "brunet", marker.call = "kim", marker.unique = FALSE, log2 = FALSE, qvalue = 5, aucth = 0.7, err.cutoff = 0.2, thresh, silhouette = FALSE, silhouette.method='correlation', seed = 123456,min.foldchange=0)
}
\arguments{
  \item{d}{
    none negative matrix. normally the data can be transformed into 2^x.
}
  \item{clinic}{
    data frame where rows are samples and columns are features. There can be NA.
}
  \item{rank}{
    a integer which is the number of cluster
}
  \item{method}{
    a character specifying the method used to cluster samples. Possible values are 'brunet' (default), 'lee', 'ns', 'nsNMF'.
}
  \item{marker.call}{
    method used to identify signature. Possible values are 'pam' (default), 'kim', 'max', 'simple'.
}
  \item{marker.unique}{
    logic value specifying whether unique markers for each subtype should be identified. default is FALSE. Unless there are strong possible markers for each subtype, it is not recommanded.
}
  \item{log2}{
    if you applied 2^x to transform data to positive values, set to FALSE (default). the data will be taken log2 for further downstream signature dientifcation method.
}
  \item{qvalue}{
    FDR rate, default 5. 
}
  \item{aucth}{
    threshold for AUC (default 0.7).
}
  \item{err.cutoff}{
    threshold to determine optimal signature. default error rate is 0.2. The first signature which performs bettern than this threshold is used as signature.
}
  \item{thresh}{
    an index value between 1 to 30. It is used to decide optimal threshold above.
}
  \item{silhouette}{
    apply silhouette to select core samples associated with clustering. Samples with negative values are removed. Default is FALSE. It is recommanded if the data size if big enough (i.e. > 100).
}
  \item{silhouette.method}{
    method used in silhouette. possible values are "correlation", "canberra", "maximum", "manhattan", "canberra","binary" or "minkowski"
}
  \item{seed}{
    an integer or one of 'random'(default),'none','ica','nndsvd'
}
  \item{min.foldchange}{
    a numeric value which indicates the fold change difference between two groups. it is used in sam function. Default is 0 which means fold change does count.
}
}
\details{
    
}
\value{
    gene list. it might be changed in future
}
\author{
    Chengyu Liu <chengyu.liu.cs@gmail.com>
}
\keyword{ nmf }
\keyword{ clustering }
