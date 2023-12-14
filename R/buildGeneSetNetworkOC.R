

#' buildGeneSetNetworkOC
#'
#' @description Using a gene set collection and a background of observable genes, calculate a matrix of
#' Szymkiewicz–Simpson overlap coefficients and return a GSNData object.
#'
#' @param object An object of type GSNData. If NULL, a new one is instantiated.
#'
#' @param ref.background (required) A character vector corresponding to the genes observable in a differential
#' expression, ATACSeq or other dataset. This corresponds to the background used in tools like DAVID.
#'
#' @param geneSetCollection (required) A gene set collection either in the form of a tmod object, or a list of
#' gene sets / modules as character vectors containing gene symbols and names corresponding to the
#' gene module identifier.
#'
#' @param distMatrixFun (optional) Function for calculating the distance matrix. Defaults to
#' \code{scoreOCMatrix_C}. Functions used for this purpose are expected to return a square
#' numeric matrix corresponding to the distances between all gene sets.
#'
#' @return This function returns a GSNData object with the \code{$default_distance} field set as
#' \code{'oc'} and \code{$distances$lf$optimal_extreme} set to \code{'max'}.
#'
#' @details This function wraps the process of creating a GSNData object and calculating a Szymkiewicz–Simpson overlap
#' coefficient matrix. The Szymkiewicz–Simpson overlap coefficient matrix is calculated using \code{scoreOCMatrix()},
#' which is implemented in C++.
#'
#' **Note:** Because with Szymkiewicz–Simpson overlap coefficients, higher values indicate a closer match between sets,
#' they are unlike standard metrics of distance. Therefore the optimal_extreme is \code{"max"}, and for certain
#' operations, such as construction of a hierarchical tree, they may require transformation for use in clustering.
#' Secondly, since the Szymkiewicz–Simpson method often produces a large number of tie values in a distance matrix,
#' we recommend paring using hierarchical clustering (with \code{\link{gsnPareNetGenericHierarchic}}) instead of
#' nearest neighbor clustering.
#'
#' @export
#'
#' @examples
#'
#' library(GSNA)
#'
#' \dontrun{
#' observable_genes <- rownames(FILTERED_RNASEQ_COUNT_MATRIX)
#' msig.subset <- msig[cerno_results$ID,]
#' GSN <- buildGeneSetNetworkOC( object = NULL,
#'                               ref.background = observable_genes,
#'                               geneSetCollection = msig.subset )
#' }
#'
#' @seealso
#'  \code{\link{scoreOCMatrix_C}}
#'  \code{\link{buildGeneSetNetworkJaccard}}
#'  \code{\link{buildGeneSetNetworkLFFast}}
#'  \code{\link{buildGeneSetNetworkSTLF}}
#'
#' @importFrom Matrix as.matrix

buildGeneSetNetworkOC <- function( object = NULL, ref.background = NULL, geneSetCollection = NULL, distMatrixFun = scoreOCMatrix_C ){
  buildGeneSetNetworkGeneric(object, ref.background, geneSetCollection, distMatrixFun, distance = 'oc', optimal_extreme = "max" )
}


