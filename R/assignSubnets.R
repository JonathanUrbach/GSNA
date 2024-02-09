
#' assignSubnets
#'
#' @description Utility function for assigning subnets. Not usually called directly by most users. Instead,
#' use \code{gsnAssignSubnets()}.
#'
#' @param edges.df A data frame with at least 2 columns. The first two character vector columns indicate vertices. Additional
#' columns are numeric scores for ranking edges.
#' @param scoreCol (optional) A score column used for ordering edges. See explanation below. If there are 3 or more columns
#' the last one is presumed to be the score column and used for ordering. The score is usually derived from a pathways
#' score but may also be derived the pared distance matrix.
#' @param highToLow (optional) A boolean indicating how scores are to be ordered based on significance, low to high, or
#' high to low.
#'
#' @details The \code{assignSubnets} method uses distances derived from pathways data or from the pared distance matrix to
#' join subnets, starting with the most significant edge scores in a subnet, and subsequently joining additional vertices
#' in order of the best score.
#'
#' @return The function returns a list containing:
#'\describe{
#'   \item{\code{edges}}{The edges data.frame, but with a subnet column added.}
#'   \item{\code{subnets}}{A list of vectors such that the names of the vectors are the names of subnets, and the contents
#'         of each vector are the gene sets making up that vector.}
#'   \item{\code{vertex_subnets}}{A data.frame containing the name of a vertex and its assigned subnet.}
#'   }
#'
#' examples
#' \code{
#'    subnets.l <- assignSubnets( edges.df = edges.df, scoreCol = "p.adj", highToLow = FALSE )
#' }
#'
#' @seealso \code{\link{gsnAssignSubnets}}
#'
#' @noRd
#' @keywords internal
assignSubnets <- function(edges.df, scoreCol = NULL, highToLow = NULL ){
  # Columns 1 and 2 are To and From
  # If there are > 2 columns, use the column indicated by 'scoreCol' as a basis for sorting.
  #    This defaults to the last column.
  # highToLow == TRUE directs sort to start with highest value, FALSE reverses that.
  if(ncol(edges.df) >= 3){
    if( is.null(scoreCol) ) scoreCol <- ncol( edges.df )
    edges.df <- edges.df[order( edges.df[,scoreCol] * 2*(0.5-highToLow) ),]
  }
  edges.df$subNet <- NA
  subnet.l <- list()
  subnetNum <- 0
  for( edj in 1:nrow(edges.df) ){
    #for(edj in 1){
    if( is.na(edges.df[edj, "subNet"]) ){
      subnetNum <- subnetNum + 1
      vertices.v <- c(edges.df[edj,1], edges.df[edj,2]) # Start with first 2 nodes.
      vertices.v <- vertices.v[!is.na(vertices.v)]
      for( newVertx in vertices.v ){
        recursiveGetConnectedVertices( newVertx, environment() )
      }
      edges.df[edges.df[,1] %in% vertices.v | edges.df[,2] %in% vertices.v, "subNet"] <- subnetNum
      subnet.l[[as.character(subnetNum)]] <- vertices.v
    }
  }
  vertex_subnets.df <- antiSplit( .l = subnet.l, col.names = c("subnet","vertex"))
  list( edges = edges.df, subnets = subnet.l, vertex_subnets = vertex_subnets.df[, c("vertex","subnet")])
}
