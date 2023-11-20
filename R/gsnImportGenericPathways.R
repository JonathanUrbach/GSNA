
#' gsnImportGenericPathways
#'
#' @description Import a data.frame or text file containing a pathways result set to a GSNData object.
#' The \code{id_col} and \code{stat_col} should be specified, but if they are not, the function attempts to guess.
#'
#' @param object A GSNData object.
#' @param pathways_data An (optional) data.frame containing the pathways analysis. (Either this or the
#' \code{filename} argument must be set.
#' @param filename An (optional) filename for data sets read from a text file containing pathways results. This is ignored
#' if the \code{pathways_data} argument is set.
#' @param type A cbaracter vector of length 1 indicating the type of result set. This defaults to \code{'generic'}.
#' @param id_col (optional) A character vector of length 1 indicating the name of the column used as a key for gene
#' sets or modules. This should be the same as the set of names of gene sets in the gene set collection specified by the
#' \code{geneSetCollection} argument used in building gene set networks. If not specified, the function will search for
#' \code{"ID"}, \code{"id"}, \code{"NAME"} & \code{"Term"} in the data set's column names, in that order, taking the first
#' one it finds. The values in the column must correspond to the names of the gene sets provided, or an error will be thrown.
#' @param stat_col (optional) A character vector of length 1 indicating the name of the column used as a statistic
#' to evaluate the quality of pathways results. If unspecified, the function uses regular expressions to search for
#' a column that is labeled as a p-value or p-adj.
#' @param stat_col_2 (optional) A character vector of length 1 indicating the name of the column used as an optional
#' second statistic to evaluate the quality of pathways results. If unspecified, the value is NULL.
#' @param sig_order (optional) Either \code{'loToHi'} (default) or \code{'hiToLo'} depending on the statistic used to
#' evaluate pathways results.
#' @param sig_order_2 (optional) Either \code{'loToHi'} (default) or \code{'hiToLo'} depending on the \code{stat_col_2}
#' statistic used to evaluate pathways results.
#' @param sep A separator for text file import, defaults to "\\t". Ignored if the \code{filename} argument is not specified.
#'
#' @return This returns a GSNData object containing imported pathways data.
#'
#' Note: An error is thrown if all gene set IDs in the \code{$genePresenceAbsence} field are not present in the GSNORA ID
#' column. On the other hand, if there are gene set IDs present in the pathways data that are absent from the
#' genePresenceAbsence matrix, then thes methods emit a warning. It also checks for the standard GSNORA data set column
#' names, and if some are missing, it will throw an error.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' gsn_object <- gsnImportGSNORA( object = gsn_object, pathways_data = dat.cerno )
#' }
#'
#' @seealso
#'  \code{\link{gsnAddPathwaysData}}
#'  \code{\link{gsnImportCERNO}}
#'  \code{\link{gsnImportGSEA}}
#'  \code{\link{gsnImportGenericPathways}}
#'
#' @importFrom utils read.table
#'

gsnImportGenericPathways <- function( object, pathways_data = NULL, filename = NULL, type = 'generic', id_col = NULL, stat_col = NULL, stat_col_2 = NULL, sig_order = NULL, sig_order_2 = NULL, sep = "\t" ){
  stopifnot( "GSNData" %in% class( object ) )

  # Not searching for *all* the fields, just the critical ones. (Some are repeats, with weird names.)
  if( is.null( pathways_data ) && is.null( filename ) ) stop( "The 'pathways_data' and 'filename' arguments cannot both be NULL." )
  if( is.null( pathways_data ) ){
    pathways_data <- utils::read.table( file = filename, header = TRUE, sep = sep, stringsAsFactors = FALSE, check.names = FALSE )
  }
  if( !is.null(sig_order) && ! sig_order %in% c( "loToHi", "hiToLo" ) )
    stop( "Invalid sig_order: ", as.character( sig_order ) )
  if( !is.null(sig_order_2) && ! sig_order_2 %in% c( "loToHi", "hiToLo" ) )
    stop( "Invalid sig_order_2: ", as.character( sig_order_2 ) )
  if( ! is.null(stat_col) && ! stat_col %in% colnames( pathways_data ) )
    stop( "stat_col '", stat_col, "' not found in pathways data."  )
  if( ! is.null(stat_col_2) &&  ! stat_col_2 %in% colnames( pathways_data ) )
    stop( "stat_col_2 '", stat_col_2, "' not found in pathways data."  )

  field_names <- colnames( pathways_data )

  pathways <- list( data = pathways_data, type = type )

  if( any( c("ID","id", "NAME", "Term" ) %in% field_names ) )
    pathways$id_col <- match.arg( arg = field_names, choices = c("ID","id", "NAME", "Term" ), several.ok = TRUE  )


  # Find the first column in the data to match a N1/N/SIZE etc. regex. (This is a bit of a guess.)
  pathways$n_col = field_names[which(stringi::stri_detect_regex(str = field_names,
                                                                pattern = "(?:N1\\b|N\\b|SIZE\\b)",
                                                                opts_regex=stringi::stri_opts_regex(case_insensitive=TRUE)))[1]]
  if( is.na( pathways$n_col ) ){ # Fix n_col if NA
    pathways$n_col <- NULL
  }

  # Find the first column in the data to match a p-val/q-val/FDR etc. regex. (This is a bit of a guess.)
  pathways$stat_col = field_names[which(stringi::stri_detect_regex(str = field_names, pattern = "(?:adj|fdr|fwer).[pq][\\s\\-\\.]?val|FDR|Benjamini|Bonferroni", opts_regex=stringi::stri_opts_regex(case_insensitive=TRUE)))[1]]
  if( is.na( pathways$stat_col ) ){ # Fix stat_col if NA
    pathways$stat_col <- NULL
  } else {
    # P-values and related statistics are loToHi
    pathways$sig_order = "loToHi"
  }

  # Add a Title column to gsea data for later:
  if( ! "Title" %in% field_names ){
    title_choices <- c("NAME", "Name", "TITLE", "TERM", "Term" )
    if( length(title_matches <- title_choices[title_choices %in% field_names]) > 0 )
      pathways$data$Title <- stringr::str_to_title( gsub( pattern = "_", replacement = " ", x = pathways$data[[title_matches[1]]] ) )
  }

  mesgs <- c()
  if( !is.null(id_col) ){
    pathways$id_col <- id_col
  } else {
    mesgs <- c( mesgs, paste0( " id_col = ", pathways$id_col ) )
  }
  if( !is.null(stat_col) ){
    pathways$stat_col <- stat_col
  } else {
    mesgs <- c( mesgs, paste0( " stat_col = ", pathways$stat_col ) )
  }
  if( !is.null(sig_order) ){
    pathways$sig_order <- sig_order
  }else {
    mesgs <- c( mesgs, paste0( " sig_order = ", pathways$sig_order ) )
  }
  if( is.null( pathways$id_col ) ) stop( "id_col (ID Column specification) required." )

  # For the optional stat_col_2 and sig_order_2
  if( !is.null(stat_col_2) ){
    pathways$stat_col_2 <- stat_col_2
  }
  if( !is.null(sig_order_2) ){
    pathways$sig_order_2 <- sig_order_2
  }

  if(length(mesgs) > 0) message(paste0( c("Using:", mesgs), collapse = "\n" ))

  if( ! all( colnames( object$genePresenceAbsence ) %in% pathways$data[[pathways$id_col]] ) )
    stop("Error: Pathways data do not match gene set collection. They are missing gene sets from gene set collection.")
  if( ! all( pathways$data[[pathways$id_col]] %in% colnames( object$genePresenceAbsence ) ) )
    warning("Warning: Pathways data do not match gene set collection. They contain gene sets not present in gene set collection.")

  object$pathways <- pathways
  object
}


