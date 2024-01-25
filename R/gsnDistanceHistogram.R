
invisible( utils::globalVariables(c("Distance Matrix", "..count..", "count")) )


#' gsnDistanceHistogram
#'
#' @description Generate a Histogram of Distances
#'
#' @param object A GSNData object
#' @param distance A distance, or even a character vector of distances, e.g. c("lf", "jaccard", "stlf").
#' @param dist.matrix The names of distance matrices, which can be "raw" for the distance stored in "matrix",
#' "pared" for the distances stored in "pared", and "edges" for the distances stored in "edges". In general,
#' the distances in "edges" will be the same as those in "pared", but this may not always be true.
#' @param stat Can be "percent", "count", "density", or "cumulative". This determines how the data are visualized.
#' @param colors Currently, this does nothing, but will eventually allow the user to specify custom colors. Stay
#' tuned.
#' @param bins The number of bins, for histograms ("percent" or "count").
#'
#' @return A ggplot2 graphical object.
#'
#' @export
#'
#' @details This function is useful for such purposes as assessing the effects of paring on the distribution of
#' distances.
#'
#'
#' @seealso
#'  \code{\link{plot.GSNData}}
#'  \code{\link{gsnPlotNetwork}}
#'  \code{\link{gsnHierarchicalDendrogram}}
#'
gsnDistanceHistogram <- function( object,
                                  distance = NULL,
                                  dist.matrix = c( "raw", "pared", "edges" ),
                                  stat = "percent", # Can also be "count" or "density"
                                  colors = NULL,
                                  bins = 100){
  stopifnot( "GSNData" %in% class( object ) )
  if( is.null( distance ) ) distance <- object$default_distance
  if( is.null( distance ) ) stop( 'Need distance argument.' )
  if( is.null(object$distances[[distance]]) ) stop( 'Cannot find data for distance ', distance )

  data.df <- data.frame( d = c(), `Distance Matrix` = c(), check.names = FALSE )

  for( d in distance ){
    for( dm in dist.matrix ){
      dist <- NULL
      if( dm == 'raw' && !is.null(object$distances[[distance]]$matrix) ){
        dist <- as.vector(object$distances[[distance]]$matrix)
      } else if( dm == 'pared' && !is.null(object$distances[[distance]]$pared) ){
        dist <- as.vector(object$distances[[distance]]$pared )
      } else if( dm == 'edges' && !is.null(object$distances[[distance]]$edges)){
        dist <- as.vector(object$distances[[distance]]$edges$Stat)
      }
      if( ! is.null( dist ) ){
        dist <- dist[!is.na(dist)]
        data.df <- rbind( data.df,
                          data.frame( dist = dist, `Distance Matrix` = paste0( d, ': ', dm ), check.names = FALSE) )
      }
    }
  }
  if( stat == "percent" ){
    # ..count.. is deprecated
    #p <- ggplot2::ggplot( data = data.df, mapping = ggplot2::aes( x = dist, fill = `Distance Matrix` ) ) + ggplot2::geom_histogram(ggplot2::aes(y = 100*(..count..)/sum(..count..), group = `Distance Matrix`), position = "dodge", bins = bins ) + ggplot2::ylab( "%" )
    p <- ggplot2::ggplot( data = data.df, mapping = ggplot2::aes( x = dist, fill = `Distance Matrix` ) ) + ggplot2::geom_histogram(ggplot2::aes(y = 100*(ggplot2::after_stat(count))/sum(ggplot2::after_stat(count)), group = `Distance Matrix`), position = "dodge", bins = bins ) + ggplot2::ylab( "%" )
  } else if ( stat == "count" ) {
    p <- ggplot2::ggplot( data = data.df, mapping = ggplot2::aes( x = dist, fill = `Distance Matrix` ) ) + ggplot2::geom_histogram( position = "dodge", bins = bins )
  } else if ( stat == "density" ) {
    p <- ggplot2::ggplot( data = data.df, mapping = ggplot2::aes( x = dist, color = `Distance Matrix` ) ) + ggplot2::geom_line(stat="density")
  } else if( stat == "cumulative" ){
    p <- ggplot2::ggplot( data = data.df, mapping = ggplot2::aes( x = dist, color = `Distance Matrix` ) ) + ggplot2::stat_ecdf(geom = "step")
  } else {
    stop( "Don't understand stat ", stat )
  }
  p
}

