
invisible(utils::globalVariables( c("DIST", "M1", "M2", "Stat")))

#' gsnPareNetGenericHierarchic
#'
#' @description Method to perform hierarchical clustering and paring of gene set networks.
#'
#' @param object  An object of type \code{GSNData} containing a distance matrix.
#'
#' @param distance  (optional) character vector of length 1 indicating which pared distance matrix is to be used for assigning
#' subnets. This defaults to the 'default_distance'.
#'
#' @param extreme (optional) Either \code{min} or \code{max} indicating whether low or high values are most significant,
#' i.e. to be interpreted as the shortest distance for nearest neighbor paring. This defaults to the value set for the
#' \code{optimal_extreme} field of the specified \code{distance} matrix.
#'
#' @param cutoff (optional) A cutoff specifying a maximal of minimal value that will be retained, dependent on the distance
#' metric being used. This is not usually necessary to specify for hierarchical clustering. (see details)
#'
#' @param keepOrphans A boolean indicating whether 'orphan' gene sets that have no nearest neighbors should be retained in
#' the final network. (default \code{TRUE} )
#'
#' @param matrix_scaling_fun A function to perform transformation and scaling of the distance matrix. The default,
#' \code{distMat2UnitNormRank} converts the distance matrix to ranks and scales the resulting numbers to a range between 0 and 1.
#' If set to NULL, the distances are not scaled or transformed. (see details)
#'
#' @param lower_is_closer Boolean indicating that lower values should be treated as closer for the sake of hierarchical
#' clustering.
#'
#' @param k (optional) Parameter passed to cutree to determine the number of desired clusters. If both k and h are NULL,
#' a value for k will be chosen. (see details)
#'
#' @param h (optional) Parameter passed to cutree to determine the cutting height for breaking the clusters into groups.
#' (see details)
#'
#' @param method (optional) Parameter passed to \code{hclust()} to specify the hierarchical clustering method used.
#' (default "average")
#'
#' @return A \code{GSNData} copy of the original \code{object} argument containing a pared distance matrix for the
#' specified distance metric.
#'
#' @details This method performs hierarchical clustering, then joins the members of each cluster. This joining occurs as
#' follows:
#'
#' 1. First, only the edges between gene sets belonging to the same hierarchical cluster are considered, and the
#'   edges within each cluster are ordered by distance.
#' 2. The first edge is the edge defined by the shortest distance.
#' 3. Subsequent edges are added to the subnet by selecting the shortest from the edges shared by one joined  and
#'   one unjoined gene set.
#' 4. This process is repeated until all gene sets in a cluster are joined as a subnet.
#'
#' This joining method differs from nearest neighbor joining in that unjoined nodes are initially joined, not to their
#' nearest neighbor necessarily, but to their nearest neighbor from among the nodes already joined together in a subnet.
#' This method avoids bifurcation of subnets that could occur by regular nearest neighbor joining.
#'
#' NOTE: The \code{matrix_scaling_fun} argument is a function that takes the distance matrix and transforms
#' it into scaled data appropriate for hierarchical clustering. (As such, it should return data with low values
#' indicating closer gene sets, as opposed to a Jaccard index where high values are closest.) Because this
#' function may transform the data from a scale where high values are close to one where low values are close,
#' such functions should return a matrix with a \code{lower_is_closer} attribute set as \code{TRUE} to indicate
#' that. If the \code{lower_is_closer} attribute is not set by \code{matrix_scaling_fun}, then it will be assumed
#' to be the same as the raw distance matrix, which may generate an error if the \code{optimal_extreme} of the
#' distance matrix is not \code{'min'}. This value will be used to set the corresponding
#' \code{$distances[[distance]]$pared_optimal_extreme} field in the GSNData object. In general, a scaling
#' transformation is necessary because some potential distance metrics are in log-space and have skewed
#' distributions and negative values (like log Fisher) or are actually similarity metrics, with higher values
#' being closer. In this way they differ from standard distances, and require transformation to be suitable for
#' hierarchical clustering. The default, \code{matrix_scaling_fun} argument, \code{\link{distMat2UnitNormRank}()}
#' scales the data to a range between 0 and 1, and converts it to a uniform distribution. This may be a bit
#' extreme for some purposes, but it allows the hierarchical clustering method to work simply with default values
#' for most users obviating the need to transform the data or adjust default parameters in many cases. Other
#' values for this argument are \code{\link[base]{identity}()} (which can be used when a transformation is not
#' desired) and \code{\link{complement}()} which for an input value \eqn{x} returns \eqn{1 - x}, useful for
#' transforming Jaccard indices and Szymkiewicz–Simpson overlap coefficients. To produce a plot of the relationship
#' between the raw and transformed/scaled pared distances, use \code{\link{gsnParedVsRawDistancePlot}()}.
#'
#' @examples
#'
#' library(GSNA)
#'
#' # In this example, we generate a gene set network from CERNO example
#' # data. We begin by subsetting the CERNO data for significant results:
#' sig_pathways.cerno <- subset( Bai_CiHep_DN.cerno, adj.P.Val <= 0.05 )
#'
#' # Now create a gene set collection containing just the gene sets
#' # with significant CERNO results, by subsetting Bai_gsc.tmod using
#' # the gene set IDs as keys:
#' sig_pathways.tmod <- Bai_gsc.tmod[sig_pathways.cerno$ID]
#'
#' # And obtain a background gene set from differential expression data:
#' background_genes <- toupper( rownames( Bai_CiHep_v_Fib2.de ) )
#'
#' # Build a gene set network:
#' sig_pathways.GSN <-
#'    buildGeneSetNetworkJaccard(geneSetCollection = sig_pathways.tmod,
#'                               ref.background = background_genes )
#'
#' # Now import the CERNO data:
#' sig_pathways.GSN <- gsnImportCERNO( sig_pathways.GSN,
#'                                     pathways_data = sig_pathways.cerno )
#'
#' # Now we can pare the network. By default, the distances are complemented
#' # and converted into ranks for the sake of generating a network.
#' sig_pathways.GSN <- gsnPareNetGenericHierarchic( object = sig_pathways.GSN )
#'
#' # However, for similarity metrics such as the Jaccard index or Simkiewicz-
#' # Simpson overlap coefficient, with a domain of 0 to 1, in which higher
#' # values are "closer", \code{\link{complement}()} might be a good
#' # transformation as well.
#' sig_pathways.GSN <- gsnPareNetGenericHierarchic( object = sig_pathways.GSN,
#'                                            matrix_scaling_fun = complement )
#'
#'
#' @seealso
#'  \code{\link{gsnPareNetGenericToNearestNNeighbors}}
#'  \code{\link{distMat2UnitNormRank}}
#'  \code{\link{gsnParedVsRawDistancePlot}}
#'
#' @importFrom stats cutree hclust as.dist
#' @importFrom tidyr gather
#' @importFrom tibble rownames_to_column
#' @export
#'
gsnPareNetGenericHierarchic <- function( object,
                                         distance = NULL,
                                         extreme = NULL,
                                         cutoff = NULL,
                                         keepOrphans = TRUE,
                                         #matrix_scaling_fun =distMat2UnitNormRank,# Scaling function for matrix
                                         matrix_scaling_fun = NULL,                # Scaling function for matrix
                                         lower_is_closer = NULL,                   # Lower values are treated as closer.
                                         k = NULL,                                 # Integer number of groups/clusters for cutree.
                                         h = NULL,                                 # Numeric scalar of height for cutting tree
                                         # If neither h or k is set, k will default to
                                         # round((number of nodes)^(2/3))
                                         method = "average"                        # hclust agglomeration method
){
  # Get call arguments
  paring_call_arguments <- as.list(c(paring_function_name="gsnPareNetGenericHierarchic", as.list(environment()) ) )
  paring_call_arguments$matrix_scaling_fun <- deparse(substitute(matrix_scaling_fun) )
  paring_call_arguments$object <- NULL

  stopifnot( "GSNData" %in% class( object ) )

  if( is.null(distance) ) distance <- object$default_distance
  if( is.null(distance) ) stop( 'distance parameter required (lf, jaccard, etc.).' )

  if( is.null( extreme ) ) extreme <- object$distances[[distance]]$optimal_extreme
  if( is.null( extreme ) ) stop( "Parameter 'extreme' is udefined." )

  if( is.null( lower_is_closer ) ) lower_is_closer <- extreme == "min"

  dist.matrix.scaled <- object$distances[[distance]]$matrix

  # Set the self distances to NA, if they're not already NA
  for( geneSetName in union( rownames( dist.matrix.scaled ), colnames( dist.matrix.scaled ) ) )
    dist.matrix.scaled[geneSetName,geneSetName] <- NA

  #if( is.infinite(N) || N > ncol( dist.matrix.scaled ) )
  #  N <- ncol( dist.matrix.scaled )

  # If cutoff is set, then we filter the data set, but this may not be desirable for hierarchical clustering.
  if( ! is.null(cutoff) ){
    if( extreme == "max" ){ # "max
      dist.matrix.scaled[dist.matrix.scaled > cutoff] <- NA
    } else {
      dist.matrix.scaled[dist.matrix.scaled < cutoff] <- NA
    }
  }

  #if( ! is.null( matrix_scaling_fun ) )
  #  dist.matrix.scaled <- matrix_scaling_fun( (2*lower_is_closer - 1) * dist.matrix.scaled )
  if( is.null( matrix_scaling_fun ) ){
    if( lower_is_closer )
      matrix_scaling_fun <- distMat2UnitNormRank  else
        matrix_scaling_fun <- function( mat ) distMat2UnitNormRank( mat = - mat )
  }
  dist.matrix.scaled <- matrix_scaling_fun( dist.matrix.scaled )

  # pared_optimal_extreme may be different from optimal_extreme
  pared_optimal_extreme <- extreme
  if( !is.null( attr( x = dist.matrix.scaled, which = 'lower_is_closer' ) ) )
    pared_optimal_extreme <- ifelse( test = attr( x = dist.matrix.scaled, which = 'lower_is_closer' ),
                                     yes = 'min', no = 'max' )

  object$distances[[distance]]$hclust <- stats::hclust( d = stats::as.dist(dist.matrix.scaled), method )

  clusters.v <- NULL

  if( is.null( h ) & is.null( k ) )
    k <- round(ncol( dist.matrix.scaled ) ^ (2/3))
  if( !is.null( k )){
    clusters.v <- stats::cutree( object$distances[[distance]]$hclust, k = k )
  } else {
    clusters.v <- stats::cutree( object$distances[[distance]]$hclust, h = h )
  }

  # Generate Empty Pared Matrix with NAs
  cs.matrix.pared <- matrix( nrow = nrow(dist.matrix.scaled), ncol = ncol(dist.matrix.scaled), dimnames = dimnames( dist.matrix.scaled ))

  dist.gather <- subset( tidyr::gather( data = tibble::rownames_to_column( as.data.frame(dist.matrix.scaled), "M1" ),  key = "M2", value = "DIST", - "M1" ),
                         !is.na(DIST) )

  # Order edges by DIST, taking pared_optimal_extreme into consideration:
  pared_optimal_extreme_sign <- 2 * ((pared_optimal_extreme == 'min') - 0.5)
  #dist.gather <- dist.gather[order(dist.gather$DIST) * pared_optimal_extreme_sign,]
  dist.gather <- dist.gather[order(dist.gather$DIST * pared_optimal_extreme_sign),]

  # Add edgeNo column
  dist.gather$edgeNo <- 1:nrow( dist.gather )

  # Empty edges.df data.frame.
  edges.df <- data.frame( M1 = c(), M2 = c(), DIST = c(), edgeNo = c(), cluster = c() )

  for( cluster.i in unique( clusters.v ) ){
    # Subset to vertices only in the cluster.i
    vertices.i = names( clusters.v[clusters.v == cluster.i] )
    if( length( vertices.i ) > 1 ) {
      edges.i <- data.frame( M1 = c(), M2 = c(), DIST = c(), edgeNo = c() )
      # Subset to edges that include only those vertices in cluster.i
      dist.i <- subset(dist.gather, M1 %in% vertices.i & M2 %in% vertices.i )
      dist.i$cluster <- cluster.i
      # Due to the ordering step, DISTs are ordered, and the first in every subset is the lowest.
      # First edge should be symmetric for symmetric distance metrics:
      seed.edges.i <- subset(dist.i, M1 %in% dist.i[1,c("M1","M2")] &  M2 %in% dist.i[1,c("M1","M2")] & DIST == dist.i[1,"DIST"] )
      # Joined vertices is a list of vertices that are already within a growing cluster. We add subsequent vertices to this growing set:
      joined_vertices <- unique(with(seed.edges.i, c(M1, M2) ) )
      #joined_edges <- seed.edges.i$edgeNo

      edges.i <- rbind(edges.i, seed.edges.i)

      dist.j <- subset( dist.i, (M1 %in% joined_vertices & ! M2 %in% joined_vertices ) )

      while(nrow(dist.j) > 0) {
        joined_vertices<-c(joined_vertices,dist.j[1,"M2"])
        edges.i <- rbind(edges.i,dist.j[1,])
        dist.j <- subset( dist.i, (M1 %in% joined_vertices & ! M2 %in% joined_vertices ) )
      }
      edges.df <- rbind( edges.df, edges.i )
    }
  }
  for( i in 1:nrow(edges.df) ){
    cs.matrix.pared[edges.df[i,"M1"],edges.df[i,"M2"]] <- edges.df[i,"DIST"]
  }

  object$distances[[distance]]$paring_call_arguments <- paring_call_arguments
  object$distances[[distance]]$pared <- cs.matrix.pared
  object$distances[[distance]]$pared_optimal_extreme <- pared_optimal_extreme
  object$distances[[distance]]$clusters <- clusters.v
  object$distances[[distance]]$edges <- subset(tidyr::gather( data  = tibble::rownames_to_column(as.data.frame(cs.matrix.pared), "M1" ),
                                                              key = "M2",
                                                              value = "Stat", -M1),
                                               !is.na(Stat))

  if( keepOrphans ){
    # Make Orphan vertices list
    orphanVertices <- colnames(object$distances[[distance]]$matrix)
    orphanVertices <- orphanVertices[!orphanVertices %in% unique(c( object$distances[[distance]]$edges$M1, object$distances[[distance]]$edges$M2))]
    object$distances[[distance]]$orphanVertices <- orphanVertices
    #
    if( length(orphanVertices) > 0 ){
      orphanVertices.df <- data.frame(M1 = orphanVertices, M2 = rep( NA, length(orphanVertices) ), Stat = rep( NA, length(orphanVertices) ) )
      object$distances[[distance]]$edges <- rbind( object$distances[[distance]]$edges, orphanVertices.df )
    }
  }

  object
}





