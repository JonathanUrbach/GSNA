
# Assumes edges.df and vertices.v both exist

#' recursiveGetConnectedVertices
#'
#' @description Internal function used by the \code{assignSubnets()} function. When passed the name of a vertex,
#' this function recursively retrieves a full set of connected vertices to populate a character vector named
#' \code{vertices.v}. The function requires that the \code{edges.df} data.frame containing an edge list and
#' the \code{vertices.v} vector, which it populates with the names of connected vertices, to be present in
#' the environment passed using the \code{e} argument. The default environment is the calling environment.
#'
#' @param vertx Name of a vertex.
#' @param e An environment containing an edge list data.frame (\code{edges.df}) and a vertices.v character vector.
#'
#' @details This method is currently of limited use outside of assignSubnets, for which it does most of the work.
#'
#' @noRd
#' @keywords internal
recursiveGetConnectedVertices <- function( vertx, e=environment() ){
  if( ! vertx %in% e$vertices.v )
    e$vertices.v <- c(e$vertices.v, vertx)
  newVertices.v <- unique(c(e$edges.df[(e$edges.df[,1] == vertx),2], e$edges.df[(e$edges.df[,2] == vertx),1]))
  # Remove NA vertices:
  newVertices.v <- newVertices.v[!is.na(newVertices.v)]
  # This is necessary to prevent infinite recursion!
  newVertices.v <- newVertices.v[!newVertices.v %in% e$vertices.v]
  e$vertices.v <- c(e$vertices.v, newVertices.v)
  for( newVertx in newVertices.v ){
    recursiveGetConnectedVertices( newVertx, e )
  }
}
