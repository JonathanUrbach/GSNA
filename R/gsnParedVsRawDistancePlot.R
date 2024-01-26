
invisible( utils::globalVariables( c( 'Pared/Scaled Distances' ) ) )

#' gsnParedVsRawDistancePlot
#'
#' @description A method for generating a bivariate plot of pared/scaled distances vs. raw distances.
# This is mainly useful when distance scaling is turned on, as with hierarchical cluster-based paring,
# gsnPareNetGenericHierarchic.
#'
#' @param object An object of type \code{GSNData} containing a distance matrix.
#' @param distance (optional) character vector of length 1 indicating which pared distance matrix is to be used for assigning
#' subnets. This defaults to the 'default_distance'.
#' @param ... Additional graphical parameters to be passed to \code{plot.default()}.
#'
#' @returns A NULL value.
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
#' # Now we can pare the network and assign subnets:
#' sig_pathways.GSN <- gsnPareNetGenericHierarchic( object = sig_pathways.GSN )
#'
#' # Create a pared vs raw distance plot to examine the relationship between the
#' # raw and pared distances:
#' gsnParedVsRawDistancePlot( sig_pathways.GSN )
#'
#' @seealso \code{\link{gsnPareNetGenericHierarchic}}
#'
gsnParedVsRawDistancePlot <- function( object,
                                       distance = NULL,
                                       ... ){
  stopifnot( "GSNData" %in% class( object ) )
  if( is.null( distance ) ) distance <- object$default_distance
  if( is.null( distance ) ) stop( 'Need distance argument.' )
  if( is.null(object$distances[[distance]]) ) stop( 'Cannot find data for distance ', distance )
  if( is.null(object$distances[[distance]]$matrix) ) stop( 'Raw distance matrix is missing.' )
  if( is.null(object$distances[[distance]]$pared) ) stop( 'Pared distance matrix is missing. Did you pare the distance matrix?' )
  with( subset(with( object$distances[[distance]],
                     data.frame( `Pared/Scaled Distances` = as.vector(pared),
                                 `Raw Distances` = as.vector(matrix),
                                 check.names = FALSE ) ),
               !is.na(`Pared/Scaled Distances`) ),
        plot( x =`Raw Distances`, y = `Pared/Scaled Distances`, ...) )
}
