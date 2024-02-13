

#' buildGeneSetNetworkGeneric
#'
#' @description General function to create a \code{GSNData} object and calculate a distance matrix within.
#' Employed by \code{buildGeneSetNetworkSTLF()}, \code{buildGeneSetNetworkLF()}, \code{buildGeneSetNetworkJaccard()}
#' and \code{buildGeneSetNetworkJaccard()} functions.
#'
#' @param object An object of type GSNData. If NULL, a new one is instantiated.
#'
#' @param ref.background (required) A character vector corresponding to the genes observable in a differential
#' expression, ATAC-Seq or other dataset. This corresponds to the background used in tools like DAVID.
#'
#' @param geneSetCollection (required) A gene set collection either in the form of a tmod object, or a list of
#' gene sets / modules as character vectors containing gene symbols and names corresponding to the
#' gene module identifier.
#'
#' @param distMatrixFun (required) Function for calculating the distance matrix. Functions used for this purpose
#' are expected to return a square numeric matrix corresponding to the distances between all gene sets. (see
#'  \code{\link{scoreLFMatrix_C}},  \code{\link{scoreJaccardMatrix_C}}, \code{\link{scoreOCMatrix_C}} )
#'
#' @param distance (required) Name of the distance matrix being calculated.
#'
#' @param optimal_extreme (required) Indicates whether max or min values are most significant in the specified distance
#' matrix. Can be \code{'max'} or \code{'min'}.
#'
#' @return This function returns a GSNData object.
#'
#' @details In most cases, users will want to run the specific
#' \code{buildGeneSetNetworkSTLF()},
#' \code{buildGeneSetNetworkLF()},
#' \code{buildGeneSetNetworkJaccard()}
#' or \code{buildGeneSetNetworkJaccard()} functions instead of this, but this function can be used for
#' adding support for new distance metrics.
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
#' # Build a gene set network. This does the same thing as
#' # buildGeneSetNetworkSTLF(), but can be adapted to novel distance
#' # metrics by providing a different matrix scoring function, distance
#' # name, and optimal_extreme:
#' sig_pathways.GSN <-
#'    buildGeneSetNetworkGeneric( geneSetCollection = sig_pathways.tmod,
#'                                ref.background = background_genes,
#'                                distMatrixFun = scoreLFMatrix_C,
#'                                distance = 'stlf',
#'                                optimal_extreme = "min"
#'                                 )
#'
#' @seealso
#'  \code{\link{buildGeneSetNetworkJaccard}}
#'
#'  \code{\link{buildGeneSetNetworkOC}}
#'
#'  \code{\link{buildGeneSetNetworkLF}}
#'
#'  \code{\link{buildGeneSetNetworkSTLF}}
#'
#' @importFrom Matrix as.matrix
buildGeneSetNetworkGeneric <- function( object = NULL, ref.background = NULL, geneSetCollection = NULL, distMatrixFun, distance, optimal_extreme ){
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

  if( ! is.null( attr( x = mat, which = "distance" ) ) ) distance <- attr( x = mat, which = "distance" )
  if( ! is.null( attr( x = mat, which = "lower_is_closer" ) ) )
    optimal_extreme = ifelse(attr( x = mat, which = "lower_is_closer" ), "min", "max" )

  object$distances[[distance]] <- list( matrix = mat,
                                        optimal_extreme = optimal_extreme,
                                        vertices = colnames(geneSetCollectionFilt.mat) )
  object$default_distance <- distance

  object
}
