
#' gsnImportCERNO
#'
#' @description Add a CERNO^1 analysis pathways result set to a GSNData object. The data set can be either in the
#' form of a data.frame or specified as import from a delimited text file.
#'
#' @param object A GSNData object.
#' @param pathways_data An (optional) data.frame containing the results of CERNO analysis. (Either this or the
#' \code{filename} argument must be set.
#' @param filename An (optional) filename for data sets read from a text file containing CERNO results. This is ignored
#' if the \code{pathways_data} argument is set.
#' @param id_col (optional) A character vector of length 1 indicating the name of the column used as a key for gene
#' sets or modules. This is normally the \code{ID} field of CERNO data, which must be the same as the names of gene sets
#' specified in the tmod object or in the list of gene set vectors specified with the \code{geneSetCollection} argument
#' used when building the gene set network. By default this value is \code{'ID'}, however if the user has added additional
#' IDs to a CERNO results set, such as GO_ACCESSION, that can be specified here. The IDs must correspond to the names of
#' the gene sets provided, or an error will be thrown.
#' @param stat_col (optional) A character vector of length 1 indicating the name of the column used as a statistic
#' to evaluate the quality of pathways results. By default, this is 'adj.P.val' for CERNO.
#' @param sig_order (optional) Either \code{'loToHi'} (default) or \code{'hiToLo'} depending on the statistic used to
#' evaluate pathways results.
#' @param n_col (optional) Specifies the column containing the number of genes in the gene set. Generally, this is the number
#' of genes in the gene set that are attested in an expression data set (Defaults to 'N1').
#' @param sep A separator for text file import, defaults to "\\t". Ignored if the \code{filename} argument is not specified.
#'
#' @return This returns a GSNData object containing imported pathways data.
#'
#' @details This method imports a CERNO^1 data set created by the tmod^2 package into a GSNData object.
#'
#' Note: An error is thrown if all gene set IDs in the genePresenceAbsense are not present in the CERNO ID column.
#' On the other hand, if there are gene set IDs present in the pathways data that are absent from the genePresenceAbsence
#' matrix, then these methods emit a warning. It also checks for the standard CERNO data set column names, and if some are
#' missing, it will throw an error. They can still be imported via \code{gsnImportGenericPathways}.
#'
#' @export
#'
#' @examples
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
#' # Build a gene set network:
#' sig_pathways.GSN <-
#'    buildGeneSetNetworkJaccard(geneSetCollection = sig_pathways.tmod,
#'                               ref.background = background_genes )
#'
#' # Now import the CERNO data:
#' sig_pathways.GSN <- gsnImportCERNO( sig_pathways.GSN,
#'                                     pathways_data = sig_pathways.cerno )
#'
#' @seealso
#'  \code{\link{gsnAddPathwaysData}}
#'  \code{\link{gsnImportGSEA}}
#'  \code{\link{gsnImportGSNORA}}
#'  \code{\link{gsnImportGenericPathways}}
#'
#' @references
#' 1. Zyla J, Marczyk M, Domaszewska T, Kaufmann SHE, Polanska J, Weiner J. Gene set enrichment for reproducible science: comparison of CERNO and eight other algorithms. *Bioinformatics*. 2019;**35**: 5146–5154. doi:10.1093/bioinformatics/btz447
#' 2. Weiner 3rd J, Domaszewska T. tmod: an R package for general and multivariate enrichment analysis. *PeerJ Preprints*; 2016 Sep. doi:10.7287/peerj.preprints.2420v1
#'
#' @importFrom utils read.table
#'
gsnImportCERNO <- function( object, pathways_data = NULL, filename = NULL, id_col = NULL, stat_col = NULL, sig_order = NULL, n_col = NULL, sep = "\t" ){
  stopifnot( "GSNData" %in% class( object ) )

  if( is.null( pathways_data ) && is.null( filename ) ) stop( "The 'pathways_data' and 'filename' arguments cannot both be NULL." )
  if( is.null( pathways_data ) ){
    pathways_data <- utils::read.table( file = filename, header = TRUE, sep = sep, stringsAsFactors = FALSE )
  }
  if( !is.null(sig_order) && ! sig_order %in% c( "loToHi", "hiToLo" ) )
    stop( "Invalid sig_order: ", as.character( sig_order ) )
  if( ! is.null(stat_col) && ! stat_col %in% colnames( pathways_data ) )
    stop( "stat_col '", stat_col, "' not found in pathways data."  )

  cerno_fieldnames <- c("ID", "Title", "cerno", "N1", "AUC", "cES", "P.Value", "adj.P.Val" )

  if( length( missing_fieldnames <- cerno_fieldnames[! cerno_fieldnames %in% colnames(pathways_data)] ) > 0 ){
    warning( "Data is missing the following CERNO fields:", paste0( missing_fieldnames,  collapse = ", " ) )
  }

  pathways <- list( data = pathways_data, type = "cerno", id_col = "ID", stat_col = "adj.P.Val", sig_order = "loToHi", n_col = "N1" )
  if( !is.null(id_col) ) pathways$id_col <- id_col
  if( !is.null(stat_col) ) pathways$stat_col <- stat_col
  if( !is.null(sig_order) ) pathways$sig_order <- sig_order
  if( !is.null(n_col) ) pathways$n_col <- n_col

  if( ! all( colnames( object$genePresenceAbsence ) %in% pathways$data[[pathways$id_col]] ) )
    stop("Error: Pathways data do not match gene set collection. They are missing gene sets from gene set collection.")
  if( ! all( pathways$data[[pathways$id_col]] %in% colnames( object$genePresenceAbsence ) ) )
    warning("Warning: Pathways data do not match gene set collection. They contain gene sets not present in gene set collection.")

  object$pathways <- pathways
  object
}

