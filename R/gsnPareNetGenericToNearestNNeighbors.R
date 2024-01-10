
invisible(utils::globalVariables( c("M1", "Stat")))

#' gsnPareNetGenericToNearestNNeighbors
#'
#' @description General method to pare GSNData distance matrices to nearest neighbor subset, applying any low or high value
#' cutoffs that may be required.
#'
#' @param object An object of type \code{GSNData} containing a distance matrix.
#' @param distance (optional) character vector of length 1 indicating which pared distance matrix is to be used for assigning
#' subnets. This defaults to the 'default_distance'.
#' @param extreme (optional) Either \code{min} or \code{max} indicating whether low or high values are most significant,
#' i.e. to be interpreted as the shortest distance for nearest neighbor paring. This defaults to the value set for the
#' \code{optimal_extreme} field of the specified \code{distance} matrix.
#' @param cutoff (optional) A cutoff specifying a maximal of minimal value that will be retained, dependent on the distance
#' metric being used. The default value is 0, but this is likely incorrect for most purposes. For 'lf' and 'stlf' distances,
#' we recommend a value of -90. For 'jaccard' distances, we recommend 0.3-0.4. (see details)
#' @param keepOrphans A boolean indicating whether 'orphan' gene sets that have no nearest neighbors should be retained in
#' the final network. (default \code{TRUE} )
#' @param N Integer indicating the number of nearest neighbors to retain. (default 1)
#'
#' @return A GSNData object containing a pared distance matrix for the specified \code{distance} metric.
#'
#' @details This method pares the GSN networks down to N nearest neighbors, with several tunable parameters. It is generally
#' useful to include a cutoff for this method to remove weak associations between gene sets, but this is heavily dependent on
#' the distance metric being used. A histogram or density plot showing the distribution of \code{raw} distances
#' may be useful for determining a suitable value, since inflection points can guide selection of this cutoff. Such a plot
#' may be generated using the \code{gsnDistanceHistogram()} method.
#'
#' An alternative to this paring method is hierarchical clustering implemented in the \code{\link{gsnPareNetGenericHierarchic}}
#' method.
#'
#' @export
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
#' sig_pathways.GSN <- gsnPareNetGenericToNearestNNeighbors( object = sig_pathways.GSN )
#'
#'
#' @seealso
#'  \code{\link{gsnPareNetGenericHierarchic}}
#'  \code{\link{gsnDistanceHistogram}}
#'
#' @importFrom tidyr gather
#' @importFrom tibble rownames_to_column
#'
gsnPareNetGenericToNearestNNeighbors <- function( object,
                                                  distance = NULL,
                                                  extreme = NULL,
                                                  cutoff = 0,
                                                  keepOrphans = TRUE,
                                                  N = 1 ){
  # Get call arguments
  paring_call_arguments <- as.list(c(paring_function_name="gsnPareNetGenericToNearestNNeighbors", as.list(environment()) ) )
  paring_call_arguments$object <- NULL


  stopifnot( "GSNData" %in% class( object ) )

  if( is.null( distance ) ) distance <- object$default_distance
  if( is.null( extreme ) ) extreme <- object$distances[[distance]]$optimal_extreme

  cs.matrix.pared <- object$distances[[distance]]$matrix

  # Set the self distances to NA, if they're not already NA
  for( geneSetName in union( rownames( cs.matrix.pared ), colnames( cs.matrix.pared ) ) )
    cs.matrix.pared[geneSetName,geneSetName] <- NA

  if( is.infinite(N) || N > ncol( cs.matrix.pared ) )
    N <- ncol( cs.matrix.pared )

  if( extreme == "max" ){
    cs.matrix.pared[cs.matrix.pared < cutoff] <- NA
    for( i in 1:nrow(cs.matrix.pared) ){
      row.sorted <- rev(sort(cs.matrix.pared[i,]))
      # Get the Nth highest value in row.sorted, unless it is NA, then back up
      n <- N
      while( n > 1 && is.na(row.sorted[n]) ){
        n <- n - 1
      }
      row.cutoff <- row.sorted[n]
      if( ! is.na( row.cutoff ) )
        cs.matrix.pared[i,][cs.matrix.pared[i,] < row.cutoff] <- NA
    }
  } else if( extreme == "min" ) {
    cs.matrix.pared[cs.matrix.pared > cutoff] <- NA
    for( i in 1:nrow(cs.matrix.pared) ){
      row.sorted <- sort(cs.matrix.pared[i,])
      # Get the Nth lowest value in row.sorted, unless it is NA, then back up
      n <- N
      while( n > 1 && is.na(row.sorted[n]) ){
        n <- n - 1
      }
      row.cutoff <- row.sorted[n]
      if( ! is.na( row.cutoff ) )
        cs.matrix.pared[i,][cs.matrix.pared[i,] > row.cutoff] <- NA
    }
  } else { stop( 'extreme must be \'min\' or \'max\'.' )}

  #for( i in 1:nrow(cs.matrix.pared) ){
  #  row.sorted <- rev(sort(cs.matrix.pared[i,]))
  #  # Get the Nth lowest value in row.sorted, unless it is NA, then back up
  #  n <- N
  #  while( n > 1 && is.na(row.sorted[n]) ){
  #    n <- n - 1
  #  }
  #  row.cutoff <- row.sorted[n]
  #  if( ! is.na( row.cutoff ) ) cs.matrix.pared[i,][cs.matrix.pared[i,] < row.cutoff] <- NA
  #}
  object$distances[[distance]]$paring_call_arguments <- paring_call_arguments
  object$distances[[distance]]$pared <- cs.matrix.pared
  object$distances[[distance]]$pared_optimal_extreme <- extreme
  object$distances[[distance]]$edges <- subset(tidyr::gather( data  = tibble::rownames_to_column(as.data.frame(cs.matrix.pared), "M1" ),
                                                              key = "M2",
                                                              value = "Stat", -M1),
                                               !is.na(Stat))

  if( keepOrphans ){
    # Make Orphan vertices list
    orphanVertices <- colnames(object$distances[[distance]]$matrix)
    orphanVertices <- orphanVertices[!orphanVertices %in% unique(with(object$distances[[distance]]$edges, c( M1, M2)))]
    object$distances[[distance]]$orphanVertices <- orphanVertices
    #
    if( length(orphanVertices) > 0 ){
      orphanVertices.df <- data.frame(M1 = orphanVertices, M2 = rep( NA, length(orphanVertices) ), Stat = rep( NA, length(orphanVertices) ) )
      object$distances[[distance]]$edges <- rbind( object$distances[[distance]]$edges, orphanVertices.df )
    }
  }
  object
}

