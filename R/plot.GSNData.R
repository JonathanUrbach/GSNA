#' plot
#'
#' @description Plot method for the networks within GSNData objects, implemented with \code{\link{gsnPlotNetwork}}.
#'
#' @param x A GSNData object containing a pared distance matrix with \code{edges}.
#' @inherit gsnPlotNetwork
#' @inheritDotParams gsnPlotNetwork
#' @export
#' @exportS3Method GSNA::plot
#' @seealso \code{\link{gsnPlotNetwork}}, \code{\link{gsnToIgraph}}, \code{\link[igraph]{plot.igraph}}
#'
plot.GSNData <- function(x,...){
  gsnPlotNetwork( object = x, ... )
}
