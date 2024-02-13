

#' makeFilteredGenePresenceAbsenceMatrix
#'
#' @description Take character vector containing the set of observable genes in a data set and a gene set
#' collection and generate a presence/absence matrix of observable genes in each gene set/module.
#'
#' @param ref.background (required) A character vector corresponding to the genes observable in a differential
#' expression, ATAC-Seq or other dataset. This corresponds to the background used in tools like DAVID.
#'
#' @param geneSetCollection (required) A gene set collection either in the form of a tmod object, or a list of
#' gene sets / modules as character vectors containing gene symbols and names corresponding to the
#' gene module identifier.
#'
#' @return This returns a gene presence/absence matrix with genes corresponding to rows, gene sets/modules
#' corresponding to columns, and TRUE or FALSE values corresponding to presence or absence of a particular
#' gene in a particular gene set/module. This matrix has been filtered to only include genes observable in
#' a data set.
#'
#' @export
#'
#' @examples
#'
#' library(GSNA)
#'
#' # And obtain a background of observable genes from differential
#' # expression data:
#' background_genes <- toupper( rownames( Bai_CiHep_v_Fib2.de ) )
#'
#' # Using Bai_gsc.tmod, the tmod format gene set collection in the
#' # sample data, we can now generate a filtered gene presence
#' # absence matrix. The columns of the matrix correspond to gene
#' # sets, whereas the rows are genes.
#' filteredGenePresenceAbsence_Matrix <-
#'           makeFilteredGenePresenceAbsenceMatrix( ref.background = background_genes,
#'                                                  geneSetCollection = Bai_gsc.tmod )
#'
#' @seealso
#'  \code{\link{buildGeneSetNetworkLFFast}}
#'  \code{\link{buildGeneSetNetworkSTLF}}
#'  \code{\link{buildGeneSetNetworkJaccard}}
#'
makeFilteredGenePresenceAbsenceMatrix <- function( ref.background, geneSetCollection ){
  # The geneSetCollection argument can be a tmod object or a list of vectors containing appropriate gene symbols,
  # for example, the $MODULES2GENES field of a tmod object. If it's a tmod object, the $MODULES2GENES is used.
  if( any( c( 'tmod', 'tmodGS' ) %in% class( geneSetCollection ) ) )
    geneSetCollection <- tmod2gsc( geneSetCollection )

  # A quick sanity check. Check how many of the genes in geneSetCollection are actually in ref.background.
  # If the fraction is less than min_genes_found_frx, then emit a warning.
  checkGSGenesFraction(ref.background = ref.background, geneSetCollection = geneSetCollection, min_genes_found_pct = 20)

  # Not all gene SYMs from the background will appear in gene lists, so filter the collection subset to remove the missing ones
  geneSetCollectionFilt.df <- list()
  for( geneListName in names(geneSetCollection) ){
    geneSetCollectionFilt.df[[geneListName]] <- ref.background %in% geneSetCollection[[geneListName]]
  }
  as.matrix( as.data.frame( geneSetCollectionFilt.df, row.names = ref.background, check.names = FALSE ) )
}


checkGSGenesFraction <- function( ref.background, geneSetCollection, min_genes_found_pct = 20 ){
  # A quick sanity check. Check how many of the genes in geneSetCollection are actually in ref.background.
  # If the percent of genes found in ref.bqckground is less than min_genes_found_pct, then emit a warning.
  unique_genes <- unique( unlist( geneSetCollection ) )
  genes_found_pct <- 100 * sum( unique_genes %in% ref.background ) / length( unique_genes )
  if( genes_found_pct < min_genes_found_pct ){
    warning( "Only ", sprintf( "%3.5f", genes_found_pct ), " % of genes in geneSetCollection were detected in ref.background. Are the gene indentifiers in the correct case?\n" )
  }
}
