
invisible( utils::globalVariables( "adj.P.1S" ) )

#' gsnORAtest
#'
#' @description Perform an ORA test using an experimentally-derived gene set to query a gene set collection.
#'
#' @param l A vector containing an experimentally-derived set of genes. These may be significantly differentially
#' expressed genes, genes with differential chromatin accessability or positives from a screen.
#' @param bg A vector containing a background of observable genes.
#' @param geneSetCollection A gene set collection to query, either a tmod object or a list of character vectors containing
#' gene sets for which the list element names are the gene set IDs.
#' @param Alpha The alpha value setting the significance cutoff adjusted p-value.
#' @param full This gives additional data in the results set, specifically the contingency table values.
#'
#' @return Returns a data.frame with an ORA (overrepresentation analysis) results set containing the following columns:
#'
#' 1. ID: The main identifier of the gene set
#' 2. Title: The "Title" field from \code{tmod} class gene set collection objects, corresponding to the reformatted
#' \code{STANDARD_NAME} field in an MSigDB xml file, with spaces substituted for underscores and initial only uppercase.
#' **NOTE:** If the search is done using a list of gene sets rather than a \code{tmod} object, this column will contain
#' NA.
#' 3. Enrichment: The fraction of genes in a experimental gene set that belong to a gene set divided by the fraction of
#' genes in the background of observable genes (including the experimental gene set) that belong to that gene set.
#' 4. P.2S: Raw 2-sided Fisher *p*-value
#' 5. adj.P.2S: 2-sided Fisher *p*-value corrected using the method of Benjamini & Hochberg^1 and implemented in
#' the \code{stats} package.
#' 6. P.1S: Raw, 2-sided Fisher *p*-value
#' 7. adj.P.1S: 2-sided Fisher *p*-value corrected using the method of Benjamini & Hochberg^1 and implemented in
#' the \code{stats} package.
#'
#' @details This function is provided to allow rapid and easy overrepresentation analysis using an unordered experimental
#' gene set to query a gene set collection that may be either an arbitrary list of gene-sets, or an \code{tmod} class
#' gene set collection. The statistical tests provided include both the standard two-sided Fisher and a 1-sided Fisher
#' test, similar to what is provided by the DAVID pathways analysis web application^2.
#'
#' If a list of gene sets is provided as the \code{geneSetCollection} argument, it must be structured as a list of
#' character vectors containing gene symbols (or whatever identifiers are used for the supplied experimental gene set),
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' results.ORA <- gsnORAtest(l = DE_GENES_UP,
#'                           bg = ALL_GENES_IN_DATASET,
#'                           geneSetCollection = msig.tmod )
#'}
#'
#' @seealso \code{\link[stats]{p.adjust}}
#'
#' @references
#' 1. Benjamini, Y., and Hochberg, Y. (1995). Controlling the false discovery rate: a practical and powerful approach to multiple testing. *Journal of the Royal Statistical Society Series B*, **57**, 289–300. <http://www.jstor.org/stable/2346101>.
#' 2. Dennis G Jr, Sherman BT, Hosack DA, Yang J, Gao W, Lane HC, Lempicki RA. (2003). DAVID: Database for Annotation, Visualization, and Integrated Discovery. *Genome Biol.*, **4**(5):P3. Epub 2003 Apr 3.
#'
#' @importFrom stats p.adjust
#'
gsnORAtest <- function( l, bg, geneSetCollection, Alpha = 0.05, full = FALSE ){
  if( class(geneSetCollection) == 'tmod' ){
    m2g <- geneSetCollection$MODULES2GENES
    modules <- geneSetCollection$MODULES
  } else if(class(geneSetCollection) == 'list' ){
    m2g <- geneSetCollection
    modules <- NULL
  }

  out.df <- gsnORAtest_cpp( l = l, bg = bg, geneSetCollection = m2g )

  out.df <- tibble::add_column( .data = out.df, Title = NA, .after = 'ID' )

  # The out.df may not conatain the full set of IDs since some gene sets may be lost in the filtering step:
  if( ! is.null( modules) ){
    Title4ID <- with( modules, structure( as.character( Title ), names = as.character( ID ) ) )
    #out.df$titles <- modules[names( m2g ), "Title"]
    out.df$Title <- Title4ID[ as.character(out.df$ID) ]
  }

  #out.df <- tibble::add_column( .data = out.df, N = with( out.df, b + d), .after = 'd' )
  out.df <- tibble::add_column( .data = out.df, adj.P.2S = stats::p.adjust( out.df$P.2S, method = "BH" ), .after = 'P.2S' )
  out.df <- tibble::add_column( .data = out.df, adj.P.1S = stats::p.adjust( out.df$P.1S, method = "BH" ), .after = 'P.1S' )

  if( !full )
    out.df <- within( out.df, {a <- NULL; b <- NULL; c <- NULL; d <- NULL } )

  return( subset( out.df[ order( out.df$P.2S ), ], adj.P.1S <= Alpha ) )
}


#
# gsnORAtest <- function( l, bg, geneSetCollection, Alpha = 0.05, full = FALSE ){
#   l.0 <- intersect( l, bg )    # Only take list genes that are in the background.
#   bg.0 <- bg[!bg %in% l.0]     # For bg.0, take bg genes not in l
#
#   p.1S <- c()
#   p.2S <- c()
#
#   enrichment <- c()
#
#   A <- c() # Number of "l.0" in set
#   B <- c() # Number of "l.0" not in set
#   C <- c() # Number of "bg.0" in set
#   D <- c() # Number of "bg.0" not in set
#
#   if( class(geneSetCollection) == 'tmod' ){
#     m2g <- geneSetCollection$MODULES2GENES
#     modules <- geneSetCollection$MODULES
#   } else if(class(geneSetCollection) == 'list' ){
#     m2g <- geneSetCollection
#     modules <- NULL
#   }
#
#   for( geneSetID in  names( m2g ) ){
#     contingency.matrix <-  matrix( data = c( sum( l.0 %in% m2g[[geneSetID]] ),
#                                              sum( ! l.0 %in% m2g[[geneSetID]] ),
#                                              sum( bg.0 %in% m2g[[geneSetID]] ),
#                                              sum( ! bg.0 %in% m2g[[geneSetID]] )),
#                                    nrow = 2, ncol = 2 )
#     p.2S[[geneSetID]] <- fisher.test( x = contingency.matrix, alternative = "two.sided" )$p.value
#     p.1S[[geneSetID]] <- fisher.test( x = contingency.matrix, alternative = "greater" )$p.value
#     enrichment[[geneSetID]] <- (contingency.matrix[1,1] / (contingency.matrix[1,1] + contingency.matrix[2,1])) /
#       ((contingency.matrix[1,1] + contingency.matrix[1,2]) / sum( contingency.matrix))
#     A[[geneSetID]] <- as.vector( contingency.matrix )[[1]]
#     B[[geneSetID]] <- as.vector( contingency.matrix )[[2]]
#     C[[geneSetID]] <- as.vector( contingency.matrix )[[3]]
#     D[[geneSetID]] <- as.vector( contingency.matrix )[[4]]
#   }
#
#   titles <- NA
#   if( ! is.null( modules) ) titles <- modules[names( m2g ), "Title"]
#
#   if( full ){
#     out.df <- data.frame(
#       ID = names( m2g ),
#       Title = titles,
#       Enrichment = round( enrichment, 3),
#       P.2S = p.2S,
#       adj.P.2S = p.adjust( p.2S, method = "BH" ),
#       P.1S = p.1S,
#       adj.P.1S = p.adjust( p.1S, method = "BH" ),
#       A = A,
#       B = B,
#       C = C,
#       D = D,
#       stringsAsFactors = FALSE
#     )
#   } else {
#     out.df <- data.frame(
#       ID = names( m2g ),
#       Title = titles,
#       Enrichment = round( enrichment, 3),
#       P.2S = p.2S,
#       adj.P.2S = p.adjust( p.2S, method = "BH" ),
#       P.1S = p.1S,
#       adj.P.1S = p.adjust( p.1S, method = "BH" ),
#       stringsAsFactors = FALSE
#     )
#   }
#
#   subset( out.df[ order( p.2S ), ], adj.P.1S <= Alpha )
# }
#
