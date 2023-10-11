

#' makeFilteredGenePresenceAbsenceMatrix
#'
#' @description Take character vector containing the set of observable genes in a data set and a gene set
#' collection and generate a presence/absence matrix of observable genes in each gene set/module.
#'
#' @param ref.background (required) A character vector corresponding to the genes observable in a differential
#' expression, ATACSeq or other dataset. This corresponds to the background used in tools like DAVID.
#'
#' @param geneSetCollection (required) A gene set collection either in the form of a tmod object, or a list of
#' gene sets / modules as character vectors containing gene symbols and names corresponding to the
#' gene module identifier.
#'
#' @return This returns a gene presence/absence matrix with genes corresponding to rows, gene sets/modules
#' corresponding to columns, and TRUE or FALSE values corresponding to presence or absence of a particular
#' gene in a particular gene set/module. This matrix has been filtered to only enclude genes observable in
#' a data set.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' library(GSNA)
#' observable_genes <- rownames(FILTERED_RNASEQ_COUNT_MATRIX)
#' msig.subset <- msig[cerno_results$ID,]
#' filteredGenePresenceAbsence_Matrix <-
#'        makeFilteredGenePresenceAbsenceMatrix( ref.background = observable_genes,
#'                                               geneSetCollection = msig.subset )
#' }
#'
#' @seealso \code{\link{buildGeneSetNetworkLFFast}}, \code{\link{buildGeneSetNetworkSTLF}}, \code{\link{buildGeneSetNetworkJaccard}}
#'
makeFilteredGenePresenceAbsenceMatrix <- function( ref.background, geneSetCollection ){
  # The geneSetCollection argument can be a tmod object or a list of vectors containing appropriate gene symbols,
  # for example, the $MODULES2GENES field of a tmod object. If it's a tmod object, the $MODULES2GENES is used.

  # if( 'tmod' %in% class( geneSetCollection ) ){
  #   geneSetCollection <- geneSetCollection$MODULES2GENES
  # } else if( 'tmodGS' %in% class( geneSetCollection ) ){
  #   # This maps the numerical coded genes and gene sets to a named list of character vectors.
  #   gsc <- lapply( X = geneSetCollection$gs2gv,
  #                  FUN = function( gs ){
  #                    geneSetCollection$gv[unlist(gs)]
  #                  } )
  #   names(gsc) <- geneSetCollection$gs$ID
  #   geneSetCollection <- gsc
  # }
  if( any( c( 'tmod', 'tmodGS' ) %in% class( geneSetCollection ) ) )
    geneSetCollection <- tmod2gsc( geneSetCollection )

  # Not all gene SYMs from the background will appear in gene lists, so filter the collection subset to remove the missing ones
  geneSetCollectionFilt.df <- list()
  for( geneListName in names(geneSetCollection) ){
    geneSetCollectionFilt.df[[geneListName]] <- ref.background %in% geneSetCollection[[geneListName]]
  }
  as.matrix( as.data.frame( geneSetCollectionFilt.df, row.names = ref.background, check.names = FALSE ) )
}



makeFilteredGenePresenceAbsenceMatrix.old <- function( ref.background, geneSetCollection ){
  # The geneSetCollection argument can be a tmod object or a list of vectors containing appropriate gene symbols,
  # for example, the $MODULES2GENES field of a tmod object. If it's a tmod object, the $MODULES2GENES is used.
  #if( 'tmod' %in% class( geneSetCollection ) ) geneSetCollection <- geneSetCollection$MODULES2GENES
  if( any( c( 'tmod', 'tmodGS' ) %in% class( geneSetCollection ) ) ) geneSetCollection <- tmod2gsc( geneSetCollection )

  # Not all gene SYMs from the background will appear in gene lists, so filter the collection subset to remove the missing ones
  geneSetCollectionFilt.df <- list()
  for( geneListName in names(geneSetCollection) ){
    geneSetCollectionFilt.df[[geneListName]] <- ref.background %in% geneSetCollection[[geneListName]]
  }
  as.matrix( as.data.frame( geneSetCollectionFilt.df, row.names = ref.background, check.names = FALSE ) )
}
