#' gsnPathways
#'
#' @description This function (or more specifically, pair of functions) can be used to return an
#' imported pathways data.frame, or optionally, an individual column or multiple columns specified
#' by name. When used as the LVALUE of an assignment, a new column or columns can be assigned to
#' the pathways data.frame.
#'
#'
#' @param object A \code{GSNData} object with associated pathways data.
#' @param ... The (optional) names of specific columns. If no column names are given, the whole
#' pathways data.frame is returned. When used as an LVALUE for assignment, column names must be
#' provided.
#' @param value When used in assignment, a list, data.frame, or vector RVALUE for assignment. If
#' value is a list or data.frame, the it must have as many members or columns as there are
#' named columns (see the ... argument above). If value is a vector, it is embedded into a list
#' and treated as a single column.
#'
#' @returns \code{gsnPathways()} returns an imported pathways data.frame. When used as an LVALUE,
#' \code{`gsnPathways<-`()} returns a copied GSNData object to which RVALUES have been assigned.
#'
#' @export
#'
#' @rdname gsnPathways
gsnPathways <- function( object, ... ){
  stopifnot( "GSNData" %in% class( object ) )
  .dots <- list(...)
  if( length( .dots ) == 0 ) return( object$pathways$data )
  return( object$pathways$data[,as.character(.dots)] )
}

#' @export
#' @rdname gsnPathways
`gsnPathways<-` <- function( object, ..., value ){
  stopifnot( "GSNData" %in% class( object ) )
  if( is.null( object$pathways ) || is.null( object$pathways$data ) )
    stop("object$pathways$data is not found. Use gsnAddPathwaysData() to add a pathways data set to the GSNData object.")
  .dots <- as.character(list(...))
  if( is.vector( value ) ){
    .value <- list(value)
  } else if( is.list( value ) ){
    .value <- as.list( value )
  }

  if( length( .dots ) != length( .value ) )
    stop("The number of column name arguments must be equal to the length of the list or data.frame being assigned.")
  for( i in 1:length(.dots) ){
    .dot.i <- .dots[i]
    .value.i <- .value[[i]]
    object$pathways$data[[.dot.i]] <- .value.i
  }
  object
}

