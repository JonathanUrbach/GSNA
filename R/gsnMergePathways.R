

invisible( utils::globalVariables( c("subnet") ) )

#' gsnMergePathways
#'
#' @description Merge pathways data and subnets into a data.frame that includes subnet assignment and intra-subnet rank.
#'
#' @param object A GSNData object upon which \code{gsnAssignSubnets()} has been called.
#' @param pathways.data (optiional) data.frame containing a pathways results. Not necessary if pathways data have already
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
#' \dontrun{
#' analysis.mergePathways <- gsnMergePathways( object = analysis.GSN )
#' }
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
