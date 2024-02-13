
#'
#' @name buildGeneSetNetworkLF
#' @title buildGeneSetNetworkLF, buildGeneSetNetworkLFFast-deprecated
#'
#' @description Using a gene set collection and a background of observable genes, calculate log partial
#' Fisher *p*-value distances and return the results as a GSNData object. For a 2x2 contingency matrix
#' of the form:
#'
#' \deqn{\biggl[\begin{matrix}a & b \\ c & d\end{matrix}\biggr]}
#'
#' the log Fisher *p*-value is equal to:
#'
#' \deqn{log(P) = log\biggl(\dfrac{(a+b)!(c+d)!(a+c)!(b+d)!}{a!b!c!d!(a+b+c+d)!}\biggr)}
#'
#' This differs from the \code{buildGeneSetNetworkSTLF} in that only the one value of P is summed, whereas in
#' \code{buildGeneSetNetworkSTLF}, all more extreme values are summed (prior to log-transformation), generating an
#' actual single-sided *p*-value.
#'
#' This statistic behaves approximately like a 2-sided Fisher exact test, but may not be appropriate for
#' most purposes. It is also somewhat faster to calculate than STLF (single tailed log-Fisher). Unless speed is an issue,
#' we recommend using \code{buildGeneSetNetworkSTLF} Note: \code{buildGeneSetNetworkLFFast} is deprecated. Please use
#' \code{buildGeneSetNetworkLF}() instead.
#'
#' @param object An object of type GSNData. If NULL, a new one is instantiated.
#' @param ref.background (required) A character vector corresponding to the genes observable in a
#' differential expression, ATAC-Seq or other dataset. This corresponds to the background used in
#' tools like DAVID. This is **required**, unless object already exists and contains a
#' genePresenceAbsence matrix field.
#' @param geneSetCollection (required) A gene set collection either in the form of a tmod object,
#' or a list of gene sets / modules as character vectors containing gene symbols and names
#' corresponding to the gene module identifier. This is **required**, unless object already exists
#' and contains a genePresenceAbsence matrix field.
#' @param distMatrixFun Function used to calculate distances. Takes a genePresenceAbsence matrix and
#' returns a distance matrix. (defaults to scoreLFMatrix_C)
#'
#' @return This function returns a GSNData object with the \code{$default_distance} field set as
#' \code{'lf'} and \code{$distances$lf$optimal_extreme} set to \code{'min'}.
#'
#' @details This function wraps the process of creating a GSNData object and calculating a log Fisher
#' *p*-value distance matrix. The distance matrix is calculated using \code{scoreLFMatrix_C()}.
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
#' # Now, create a GSN object with partial log Fisher values:
#' GSN <- buildGeneSetNetworkLF( ref.background = observable_genes,
#'                               geneSetCollection = gsc_subset.tmod )
#'
#'
#' @seealso
#'  \code{\link{scoreLFMatrix_C}}
#'  \code{\link{scoreJaccardMatrix_C}}
#'  \code{\link{scoreOCMatrix_C}}
#'
#' @export
#' @importFrom Matrix as.matrix
#'
buildGeneSetNetworkLF <- function( object = NULL,
                                   ref.background = NULL,
                                   geneSetCollection = NULL,
                                   distMatrixFun = function( geneSetCollection )
                                     scoreLFMatrix_C(geneSetCollection, alternative = 4)
                                   ){
  buildGeneSetNetworkGeneric(object, ref.background, geneSetCollection, distMatrixFun, distance = 'lf', optimal_extreme = "min" )
}



#' buildGeneSetNetworkLFFast
#'
#' @describeIn buildGeneSetNetworkLF
#' Deprecated, use \code{\link{buildGeneSetNetworkLF}()}.
#'
#' @param ... : Arguments passed to \code{\link{buildGeneSetNetworkLF}()}.
#'
#' @section \code{buildGeneSetNetworkLFFast}:
#' For \code{buildGeneSetNetworkLFFast()}, use \code{\link{buildGeneSetNetworkLF}()}.
#'
#' @export
buildGeneSetNetworkLFFast <- function( ... ){
  .Deprecated( "buildGeneSetNetworkLF()" )
  buildGeneSetNetworkLF( ... )
}









