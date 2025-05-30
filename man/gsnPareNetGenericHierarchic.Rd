% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gsnPareNetGenericHierarchic.R
\name{gsnPareNetGenericHierarchic}
\alias{gsnPareNetGenericHierarchic}
\title{gsnPareNetGenericHierarchic}
\usage{
gsnPareNetGenericHierarchic(
  object,
  distance = NULL,
  extreme = NULL,
  cutoff = NULL,
  keepOrphans = TRUE,
  matrix_scaling_fun = NULL,
  lower_is_closer = NULL,
  k = NULL,
  h = NULL,
  method = "average"
)
}
\arguments{
\item{object}{An object of type \code{GSNData} containing a distance matrix.}

\item{distance}{(optional) character vector of length 1 indicating which pared distance matrix is to be used for assigning
subnets. This defaults to the 'default_distance'.}

\item{extreme}{(optional) Either \code{min} or \code{max} indicating whether low or high values are most significant,
i.e. to be interpreted as the shortest distance for nearest neighbor paring. This defaults to the value set for the
\code{optimal_extreme} field of the specified \code{distance} matrix.}

\item{cutoff}{(optional) This is the argument to specify the distance used for clustering, specified before any
distance scaling is performed. This is mapped to the corresponding scaled value which is then used as the \code{h}
argument for \code{stats::cutree()}. (see details)}

\item{keepOrphans}{A boolean indicating whether 'orphan' gene sets that have no nearest neighbors should be retained in
the final network. (default \code{TRUE} )}

\item{matrix_scaling_fun}{A function to perform transformation and scaling of the distance matrix. The default,
\code{distMat2UnitNormRank} converts the distance matrix to ranks and scales the resulting numbers to a range between 0 and 1.
If set to \code{NULL}, the distances are not scaled or transformed. (see details)}

\item{lower_is_closer}{Boolean indicating that lower values should be treated as closer for the sake of hierarchical
clustering.}

\item{k}{(optional) Parameter passed to cutree to determine the number of desired clusters. If \code{k}, \code{h},
and \code{cutoff} are \code{NULL}, a value for k will be chosen. (see details)}

\item{h}{(optional) Parameter passed to cutree to determine the cutting height for breaking the clusters into groups.
This is specified in terms of the scaled data as opposed to \code{cutoff}. For the default scaling method, this
corresponds roughly to a quantile of scaled distances. (see details)}

\item{method}{(optional) Parameter passed to \code{hclust()} to specify the hierarchical clustering method used.
(default "average")}
}
\value{
A \code{GSNData} copy of the original \code{object} argument containing a pared distance matrix for the
specified distance metric.
}
\description{
Method to perform hierarchical clustering and paring of gene set networks.
}
\details{
This method performs hierarchical clustering, then joins the members of each cluster. This joining occurs as
follows:
\enumerate{
\item First, only the edges between gene sets belonging to the same hierarchical cluster are considered, and the
edges within each cluster are ordered by distance.
\item The first edge is the edge defined by the shortest distance.
\item Subsequent edges are added to the subnet by selecting the shortest from the edges shared by one joined  and
one unjoined gene set.
\item This process is repeated until all gene sets in a cluster are joined as a subnet.
}

This joining method differs from nearest neighbor joining in that unjoined nodes are initially joined, not to their
nearest neighbor necessarily, but to their nearest neighbor from among the nodes already joined together in a subnet.
This method avoids bifurcation of subnets that could occur by regular nearest neighbor joining.

NOTE: The \code{matrix_scaling_fun} argument is a function that takes the distance matrix and transforms
it into scaled data appropriate for hierarchical clustering. (As such, it should return data with low values
indicating closer gene sets, as opposed to a Jaccard index where high values are closest.) Because this
function may transform the data from a scale where high values are close to one where low values are close,
such functions should return a matrix with a \code{lower_is_closer} attribute set as \code{TRUE} to indicate
that. If the \code{lower_is_closer} attribute is not set by \code{matrix_scaling_fun}, then it will be assumed
to be the same as the raw distance matrix, which may generate an error if the \code{optimal_extreme} of the
distance matrix is not \code{'min'}. This value will be used to set the corresponding
\code{$distances[[distance]]$pared_optimal_extreme} field in the GSNData object. In general, a scaling
transformation is necessary because some potential distance metrics are in log-space and have skewed
distributions and negative values (like log Fisher) or are actually similarity metrics, with higher values
being closer. In this way they differ from standard distances, and require transformation to be suitable for
hierarchical clustering. The default, \code{matrix_scaling_fun} argument, \code{\link{distMat2UnitNormRank}()}
scales the data to a range between 0 and 1, and converts it to a uniform distribution. This may be a bit
extreme for some purposes, but it allows the hierarchical clustering method to work simply with default values
for most users obviating the need to transform the data or adjust default parameters in many cases. Other
values for this argument are \code{\link[base]{identity}()} (which can be used when a transformation is not
desired) and \code{\link{complement}()} which for an input value \eqn{x} returns \eqn{1 - x}, useful for
transforming Jaccard indices and Szymkiewicz–Simpson overlap coefficients. To produce a plot of the relationship
between the raw and transformed/scaled pared distances, use \code{\link{gsnParedVsRawDistancePlot}()}.
}
\examples{

library(GSNA)

# In this example, we generate a gene set network from CERNO example
# data. We begin by subsetting the CERNO data for significant results:
sig_pathways.cerno <- subset( Bai_CiHep_DN.cerno, adj.P.Val <= 0.05 )

# Now create a gene set collection containing just the gene sets
# with significant CERNO results, by subsetting Bai_gsc.tmod using
# the gene set IDs as keys:
sig_pathways.tmod <- Bai_gsc.tmod[sig_pathways.cerno$ID]

# And obtain a background gene set from differential expression data:
background_genes <- toupper( rownames( Bai_CiHep_v_Fib2.de ) )

# Build a gene set network:
sig_pathways.GSN <-
   buildGeneSetNetworkJaccard(geneSetCollection = sig_pathways.tmod,
                              ref.background = background_genes )

# Now import the CERNO data:
sig_pathways.GSN <- gsnImportCERNO( sig_pathways.GSN,
                                    pathways_data = sig_pathways.cerno )

# Now we can pare the network. By default, the distances are complemented
# and converted into ranks for the sake of generating a network.
sig_pathways.GSN <- gsnPareNetGenericHierarchic( object = sig_pathways.GSN )

# However, for similarity metrics such as the Jaccard index or Simkiewicz-
# Simpson overlap coefficient, with a domain of 0 to 1, in which higher
# values are "closer", \code{\link{complement}()} might be a good
# transformation as well.
sig_pathways.GSN <- gsnPareNetGenericHierarchic( object = sig_pathways.GSN,
                                           matrix_scaling_fun = complement )


}
\seealso{
\code{\link{gsnPareNetGenericToNearestNNeighbors}}
\code{\link{distMat2UnitNormRank}}
\code{\link{gsnParedVsRawDistancePlot}}
}
