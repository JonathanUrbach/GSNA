

#' buildGeneSetNetworkSTLF
#'
#' @description Using a gene set collection and a background of observable genes, calculate single-tailed
#' log Fisher *p*-value distances and return the results as a GSNData object.
#'
#' @param object An object of type GSNData. If NULL, a new one is instantiated.
#'
#' @param ref.background A character vector corresponding to the genes observable in a differential
#' expression, ATAC-Seq or other dataset. This corresponds to the background used in tools like DAVID.
#' This is **required**, unless object already exists and contains a genePresenceAbsence matrix field.
#'
#' @param geneSetCollection (required) A gene set collection either in the form of a tmod object, or a list of
#' gene sets / modules as character vectors containing gene symbols and names corresponding to the
#' gene module identifier.
#' This is **required**, unless object already exists and contains a genePresenceAbsence matrix field.
#'
#' @param distMatrixFun Function used to calculate distances. Takes a genePresenceAbsence matrix and
#' returns a distance matrix. (defaults to scoreLFMatrix_C )
#'
#' @return This function returns a GSNData object with the \code{$default_distance} field set as
#' \code{'stlf'} and \code{$distances$lf$optimal_extreme} set to \code{'min'}.
#'
#' @details This function wraps the process of creating a GSNData object and calculating a log Fisher
#' *p*-value distance matrix. The distance matrix is calculated using \code{scoreLFMatrix_C ()}, which
#' is currently implemented in R and about eight- to tenfold slower than \code{buildGeneSetNetworkLFFast()}.
#'
#' @export
#'
#' @examples
#'
#' library(GSNA)
#' library(tmod)
#'
#' # With tmod version >= 0.50.11, convert exported Bai_gsc.tmod **tmod** object to **tmodGS**:
#' if( utils::packageVersion( 'tmod' ) >= '0.50.11' )
#'   Bai_gsc.tmod <- tmod::tmod2tmodGS( GSNA::Bai_gsc.tmod )
#'
#' # Get list of observable genes from expression data:
#' observable_genes <- toupper( rownames( Bai_empty_expr_mat ) )
#'
#' # Subset GSEA data for significant results.
#' significant.Gsea <- subset( Bai_CiHep_dorothea_DN.Gsea, `FDR q-val` <= 0.05 )
#'
#' # Subset tmod object for
#' gsc_subset.tmod <- Bai_gsc.tmod[ significant.Gsea$NAME ]
#'
#' # Now, create a GSN object with single tail log Fisher values:
#' GSN <- buildGeneSetNetworkSTLF( ref.background = observable_genes,
#'                                 geneSetCollection = gsc_subset.tmod )
#'
#' @seealso
#'  \code{\link{scoreLFMatrix_C}}
#'  \code{\link{buildGeneSetNetworkLFFast}},
#'  \code{\link{buildGeneSetNetworkJaccard}}
#'
#' @importFrom Matrix as.matrix
#'
buildGeneSetNetworkSTLF <- function( object = NULL, ref.background = NULL, geneSetCollection = NULL, distMatrixFun = scoreLFMatrix_C  ){
  buildGeneSetNetworkGeneric(object, ref.background, geneSetCollection, distMatrixFun, distance = 'stlf', optimal_extreme = "min" )
}


