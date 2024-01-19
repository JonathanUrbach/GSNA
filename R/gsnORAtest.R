
invisible( utils::globalVariables( "adj.P.1S" ) )

#' gsnORAtest
#'
#' @description Perform an ORA test using an experimentally-derived gene set to query a gene set collection.
#'
#' @param l A vector containing an experimentally-derived set of genes. These may be significantly differentially
#' expressed genes, genes with differential chromatin accessibility or positives from a screen.
#' @param bg A vector containing a background of observable genes.
#' @param geneSetCollection A gene set collection to query, either a tmod object or a list of character vectors containing
#' gene sets for which the list element names are the gene set IDs.
#' @param Alpha The alpha value setting the significance cutoff adjusted p-value.
#' @param full This gives additional data in the results set, specifically the contingency table values.
#'
#' @return Returns a data.frame with an ORA (overrepresentation analysis) results set containing the following columns:
#'
#' \itemize{
#'   \item{*ID*: the gene set identifiers.}
#'   \item{*Title*: The "Title" field from \code{tmod} class gene set collection objects, corresponding to the reformatted
#'           \code{STANDARD_NAME} field in an MSigDB xml file, with spaces substituted for underscores and initial only
#'           uppercase. **NOTE:** If the search is done using a list of gene sets rather than a \code{tmod} object, this
#'           column will contain NA.}
#'   \item{*a*: the number of genes observed in the background but not in *l* or the queried gene set. (present
#'          only if \code{full == TRUE})}
#'   \item{*b*: the number of observed genes in *l* but not the queried gene set. (present only if \code{full == TRUE})}
#'   \item{*c*: the number of observed genes in the queried gene set but not *l*. (present only if \code{full == TRUE})}
#'   \item{*d*: the number of observed genes in both *l* and the queried gene set, i.e. the overlap.  (present only
#'          if \code{full == TRUE})}
#'   \item{*N*: the number of observed genes the queried gene set.}
#'   \item{*Enrichment*: The fold overrepresentation of genes in the overlap set *d* calculated as:
#'       \deqn{E = (d / (c+d)) / ((b+d)/(a+b+c+d))}
#'        }
#'   \item{*P_2S*: 2-sided Fisher *p*-value. (*NOT* log-transformed, present only if \code{full == TRUE})}
#'   \item{*adj.P.2S*: 2-sided Fisher *p*-value corrected using the method of Benjamini & Hochberg(1) and implemented in
#'         the \code{stats} package. (present only if \code{full == TRUE})}
#'   \item{*P_1S*: 1-sided Fisher *p*-value. (*NOT* log-transformed.)}
#'   \item{*adj.P.1S*: 1-sided Fisher *p*-value corrected using the method of Benjamini & Hochberg(1) and implemented in
#'         the \code{stats} package. (present only if \code{full == TRUE})}
#' }
#'
#' @details This function is provided to allow rapid and easy overrepresentation analysis using an unordered experimental
#' gene set to query a gene set collection that may be either an arbitrary list of gene-sets, or an \code{tmod} class
#' gene set collection. The statistical tests provided include both the standard two-sided Fisher and a 1-sided Fisher
#' test, similar to what is provided by the DAVID pathways analysis web application(2).
#'
#' If a list of gene sets is provided as the \code{geneSetCollection} argument, it must be structured as a list of
#' character vectors containing gene symbols (or whatever identifiers are used for the supplied experimental gene set),
#'
#' @export
#'
#' @examples
#'
#' library(GSNA)
#'
#' # From a differential expression data set, we can generate a
#' # subset of genes with significant differential expression,
#' # up or down. Here we will extract genes with significant
#' # negative differential expression with
#' # avg_log2FC < 0 and p_val_adj <= 0.05 from **Seurat** data:
#'
#' sig_DN.genes <-
#'    toupper( rownames(subset( Bai_CiHep_v_Fib2.de,
#'                        avg_log2FC < 0  & p_val_adj < 0.05 )))
#'
#' # Using all the genes in the differential expression data set,
#' # we can obtain a suitable background:
#' bg <- toupper(rownames( Bai_CiHep_v_Fib2.de ))
#'
#' # Now, we can do a overrepresentation analysis search on this
#' # data using the Bai_gsc.tmod gene set collection included in
#' # the sample data:
#' sig_DN.gsnora <- gsnORAtest( l = sig_DN.genes,
#'                              bg = bg,
#'                              geneSetCollection = Bai_gsc.tmod )
#'
#' @seealso
#'  \code{\link{gsnORAtest_cpp}}
#'  \code{\link[stats]{p.adjust}}
#'
#' @references
#' 1. Benjamini, Y., and Hochberg, Y. (1995). Controlling the false discovery rate: a practical and powerful approach to multiple testing. *Journal of the Royal Statistical Society Series B*, **57**, 289–300. <\href{http://www.jstor.org/stable/2346101}{http://www.jstor.org/stable/2346101}>.
#' 2. Dennis G Jr, Sherman BT, Hosack DA, Yang J, Gao W, Lane HC, Lempicki RA. (2003). DAVID: Database for Annotation, Visualization, and Integrated Discovery. *Genome Biol.*, **4**(5):P3. Epub 2003 Apr 3.
#'
#' @importFrom stats p.adjust
#'
gsnORAtest <- function( l, bg, geneSetCollection, Alpha = 0.05, full = FALSE ){
  if( 'tmod' %in% class(geneSetCollection) ){
    m2g <- geneSetCollection$MODULES2GENES
    modules <- geneSetCollection$MODULES
  } else if ( 'tmodGS' %in% class(geneSetCollection) ){
    m2g <- tmod2gsc( geneSetCollection )
    modules <- geneSetCollection$gs
  } else if( 'list' %in% class(geneSetCollection) ){
    m2g <- geneSetCollection
    modules <- NULL
  }
  if( ! 'character' %in% class( l ) ) stop( 'Argument l must be a characer vector' )
  if( ! 'character' %in% class( bg ) ) stop( 'Argument bg must be a characer vector' )

  out.df <- gsnORAtest_cpp( l = l, bg = bg, geneSetCollection = m2g )

  out.df <- tibble::add_column( .data = out.df, Title = NA, .after = 'ID' )

  # The out.df may not contain the full set of IDs since some gene sets may be lost in the filtering step:
  if( ! is.null( modules) ){
    Title4ID <- with( modules, structure( as.character( Title ), names = as.character( ID ) ) )
    #out.df$titles <- modules[names( m2g ), "Title"]
    out.df$Title <- Title4ID[ as.character(out.df$ID) ]
  }

  #out.df <- tibble::add_column( .data = out.df, N = with( out.df, b + d), .after = 'd' )
  out.df <- tibble::add_column( .data = out.df, adj.P.2S = stats::p.adjust( out.df$P.2S, method = "BH" ), .after = 'P.2S' )
  out.df <- tibble::add_column( .data = out.df, adj.P.1S = stats::p.adjust( out.df$P.1S, method = "BH" ), .after = 'P.1S' )

  if( !full )
    out.df <- within( out.df, {a <- NULL; b <- NULL; c <- NULL; d <- NULL; adj.P.2S <- NULL; P.2S <- NULL } )

  rownames(out.df) <- NULL

  return( subset( out.df[ order( out.df$P.1S ), ], adj.P.1S <= Alpha ) )
}


