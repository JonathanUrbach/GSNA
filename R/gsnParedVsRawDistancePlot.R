
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
#' @export
#'
#' @examples
#' \dontrun{
#'  gsnParedVsRawDistancePlot( object = analysis.GSN, col = "blue" )
#'  }
#'
#' @seealso \code{\link{gsnPareNetGenericHierarchic}}
#'
gsnParedVsRawDistancePlot <- function( object,
                                       distance = NULL,
                                       ... ){
  stopifnot( class( object ) == "GSNData" )
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
