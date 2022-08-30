
#'
#' @name buildGeneSetNetworkLF
#' @title buildGeneSetNetworkLF, buildGeneSetNetworkLF-deprecated
#'
#' @description Using a gene set collection and a background of observable genes, calculate log Fisher *p*-value
#' distances and return the results as a GSNData object. Note: \code{buildGeneSetNetworkLFFast} is deprecated.
#' Please use \code{buildGeneSetNetworkLF} instead.
#'
#' @param object An object of type GSNData. If NULL, a new one is instantiated.
#' @param ref.background (required) A character vector corresponding to the genes observable in a differential
#' expression, ATACSeq or other dataset. This corresponds to the background used in tools like DAVID.
#' This is **required**, unless object already exists and contains a genePresenceAbsence matrix field.
#' @param geneSetCollection (required) A gene set collection either in the form of a tmod object, or a list of
#' gene sets / modules as character vectors containing gene symbols and names corresponding to the
#' gene module identifier.
#' This is **required**, unless object already exists and contains a genePresenceAbsence matrix field.
#'
#' @param distMatrixFun Function used to calculate distances. Takes a genePresenceAbsence matrix and
#' returns a distance matrix. (defaults to scoreLFMatrix_C)
#'
#' @return This function returns a GSNData object with the \code{$default_distance} field set as
#' \code{'lf'} and \code{$distances$lf$optimal_extreme} set to \code{'min'}.
#'
#' @details This function wraps the process of creating a GSNData object and calculating a log Fisher
#' *p*-value distance matrix. The distance matrix is calculated using \code{scoreLFMatrix_C()}.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' library(GSNA)
#' observable_genes <- rownames(FILTERED_RNASEQ_COUNT_MATRIX)
#' msig.subset <- msig[cerno_results$ID,]
#' GSN <- buildGeneSetNetworkLF( object = NULL,
#'                                   ref.background = observable_genes,
#'                                   geneSetCollection = msig.subset )
#' }
#'
#' @seealso \code{\link{scoreLFMatrix_C}}, \code{\link{scoreSTLFMatrix}}, \code{\link{scoreJaccardMatrix_C}}
#'
#' @importFrom Matrix as.matrix
#'
buildGeneSetNetworkLF <- function( object = NULL, ref.background = NULL, geneSetCollection = NULL, distMatrixFun = function( geneSetCollection ) scoreLFMatrix_C(geneSetCollection, alternative = 4) ){
  if( is.null( object ) ) object <- GSNData()

  if( ! is.null( ref.background ) && !is.null( geneSetCollection ) ){
    geneSetCollectionFilt.mat <- makeFilteredGenePresenceAbsenceMatrix( ref.background = ref.background,
                                                                        geneSetCollection = geneSetCollection )
    object$genePresenceAbsence <- Matrix::Matrix( geneSetCollectionFilt.mat, sparse = TRUE )
  } else if( ! is.null( object$genePresenceAbsence ) ){
    geneSetCollectionFilt.mat <- Matrix::as.matrix( object$genePresenceAbsence )
  } else {
    stop("Need ref.background and geneSetCollection arguments.")
  }

  mat <- distMatrixFun(geneSetCollectionFilt.mat)
  distance <- 'lf'
  optimal_extreme <- "min"
  if( ! is.null( attr( x = mat, which = "distance" ) ) ) distance <- attr( x = mat, which = "distance" )
  if( ! is.null( attr( x = mat, which = "lower_is_closer" ) ) )
    optimal_extreme = ifelse(attr( x = mat, which = "lower_is_closer" ), "min", "max" )


  object$distances[[distance]] <- list( matrix = mat,
                                        optimal_extreme = optimal_extreme,
                                        vertices = colnames(geneSetCollectionFilt.mat) )
  object$default_distance <- distance

  #object$distances$lf <- list( matrix = distMatrixFun(geneSetCollectionFilt.mat),
  #                             optimal_extreme = "min",
  #                             vertices = colnames(geneSetCollectionFilt.mat))
  #object$default_distance <- 'lf'

  object
}



#' @rdname buildGeneSetNetworkLF

buildGeneSetNetworkLFFast <- function( ... ){
  warning( "Deprecated. Use buildGeneSetNetworkLF()." )
  buildGeneSetNetworkLF( ... )
}









