

invisible( utils::globalVariables( c("subnet") ) )

#' gsnMergePathways
#'
#' @description Merge pathways data and subnets into a data.frame that includes subnet assignment and intra-subnet rank.
#'
#' @param object A GSNData object upon which \code{gsnAssignSubnets()} has been called.
#' @param pathways.data (optional) data.frame containing a pathways results. Not necessary if pathways data have already
#' been imported.
#' @param distance (optional) character vector of length 1 indicating which set of subnets to be used if the GSNData object
#' contains subnets derived from more than one distance matrix.
#' @param id_col (optional) ID column to be used for merging subnets. Defaults to the value of \code{id_col} already set
#' during import of pathways data, if that has already been done.
#' @param stat_col (optional) The name of the column column containing the statistic to be used for ordering subnets
#' and performing intra-subnet ranking. Defaults to the value of \code{stat_col} already set during import of pathways
#' data, if that has already been done.
#' @param sig_order (optional) Character vector of length 1 indicating the whether low values of the statistic are most
#' significant ("loToHi", the default) or high values ("hiToLo") for ordering subnets and performing. Defaults to the
#' value of \code{sig_order} already set during import of pathways data, if that has already been done.
#'
#' @return A data.frame containing pathways data with merged subnet assignments and subnetRank values.
#'
#' @details In the standard workflow, just the object parameter is generally necessary. If subnets have been calculated for
#' multiple distance matrices and the subnets desired are not associated with the current default distance, then the
#' \code{distance} parameter can be specified.
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
#' # Now we can pare the network and assign subnets:
#' sig_pathways.GSN <- gsnPareNetGenericHierarchic( object = sig_pathways.GSN )
#' sig_pathways.GSN <- gsnAssignSubnets(  object = sig_pathways.GSN )
#'
#' # Now, we can use gsnMergePathways to output a table of pathway data
#' # with merged subnets:
#' gsnMergePathways( sig_pathways.GSN )
#'
#'
#' @seealso
#'  \code{\link{gsnAddPathwaysData}()}
#'  \code{\link{gsnImportCERNO}()}
#'  \code{\link{gsnImportGSNORA}()},
#'  \code{\link{gsnImportGSEA}()}
#'  \code{\link{gsnImportGenericPathways}()}
#'
#' @importFrom dplyr arrange
#'
gsnMergePathways <- function( object, pathways.data = NULL, distance = NULL, id_col = NULL, stat_col = NULL, sig_order = NULL ){
  stopifnot( "GSNData" %in% class( object ) )
  if( is.null( distance ) ) distance <- object$default_distance
  if( is.null( pathways.data ) ) pathways.data <- object$pathways$data

  if( is.null( distance ) ) stop( 'distance not defined' )
  if( is.null( pathways.data ) ) stop( 'pathways.data needed' )
  if( is.null( object$distances[[distance]]$vertex_subnets ) ) stop( 'No vertex_subnets data found. Did you call \'gsnAssignSubnets()\'?' )

  if( is.null( id_col ) ) id_col <- object$pathways$id_col
  if( is.null( id_col ) ) stop( 'id_col is NULL. Can\'t continue.' )

  if( is.null( stat_col ) ) stat_col <- object$pathways$stat_col
  if( is.null( stat_col ) ) warning( 'stat_col is NULL. Cannot order subnets.' )

  if( is.null( sig_order ) ) sig_order <- object$pathways$sig_order
  if( is.null( sig_order ) ) warning( 'sig_order is NULL. Cannot order subnets.' )

  PW.subnets <- merge( x = data.frame( subnet = object$distances[[distance]]$vertex_subnets$subnet,
                                       subnetRank = NA,
                                       ID = object$distances[[distance]]$vertex_subnets$vertex,
                                       stringsAsFactors = FALSE ),
                       y = pathways.data,
                       by.x = "ID",
                       by.y = id_col,
  )
  # Reorder columns:
  PW.subnets <- PW.subnets[,c("subnet", "subnetRank",
                                    colnames(PW.subnets)[!colnames(PW.subnets) %in% c("subnet", "subnetRank")] )]
  # Set order of gene sets within the subnets, order by subnet, then rank within subnets.
  if( ! is.null( stat_col ) && ! is.null( sig_order ) ){
    PW.subnets$subnetRank <- with( PW.subnets, ave( x = c("loToHi"= 1,"hiToLo" = -1)[[sig_order]] * get(stat_col), subnet, FUN = function(x) rank(x, ties.method = "min" ) ) )
    #PW.subnets <- PW.subnets[with(PW.subnets,order(as.numeric(subnet), c("loToHi"= 1,"hiToLo" = -1)[[sig_order]] * get(stat_col))),]
    PW.subnets <- dplyr::arrange( PW.subnets, as.numeric(subnet), as.numeric(subnetRank) )
  }
  PW.subnets
}
