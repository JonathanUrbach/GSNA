


#' gsnToIgraph
#'
#' @description For a \code{GSNData} object containing an edge list, generate an igraph object.
#'
#' @param object A \code{GSNData} object containing a pared distance matrix and an edge list.
#' @param distance (optional) A character vector specifying a distance to use. If no \code{distance} is
#' specified, the value of the \code{default_distance} will be used.
#'
#' @return Returns an \code{igraph} object corresponding to the edges and vertices in the \code{GSNData}
#' object's edge-list data.frame.
#'
#' @details This is used by gsnPlotNetwork to generate an \code{igraph}. Users will probably not need to call
#' gsnToIgraph, for most cases. If edges are not found, it will emit an error.
#'
#' @export
#'
#' @seealso \code{\link{gsnPlotNetwork}()}, \code{\link{plot.GSNData}()}
#'
#' @examples
#' \dontrun{
#'    network.igraph <- gsnToIgraph( object = object.GSN, distance = 'stlf' )
#' }
#'
#'
gsnToIgraph <- function( object, distance = NULL ){
  stopifnot( "GSNData" %in% class( object ) )
  if( is.null(distance) ) distance <- object$default_distance
  if( is.null(distance) ) stop( 'Need distance parameter.' )
  if( is.null(object$distances[[distance]]$edges) )
    stop("No edges found for distance '", distance, "'. Need to pare network and assign subnets.")
  with( object$distances[[distance]],
        igraph::graph_from_data_frame( d = subset(edges, !is.na(M1) & !is.na(M2))[,c("M1","M2")],
                                       directed = TRUE,
                                       vertices ))
}

