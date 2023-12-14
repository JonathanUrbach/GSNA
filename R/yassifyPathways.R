



#' yassifyPathways
#'
#' @description Takes a data.frame and outputs an attractively formatted HTML table widget for reports via the
#' using the DT and DataTables package. Optionally, the user can specify, via the \code{n} parameter, the number
#' of rows to display in the HTML table. Optionally, IDs in specific columns can be mapped to URLs as links.
#'
#' @param pathways A data.frame containing pathways data.
#' @param n (optional) The number of rows that the user wishes to display. This defaults to the total number of rows.
#' @param url_map_list (optional) A list of vectors containing unique IDs and their corresponding URLs as name:value pairs. In the
#' enclosing list, the element names are the names of the columns in the data.frame containing the fields needing to be
#' converted to URL links.
#' @param url_map_by_words_list (optional) Similar to url_map_list, except that instead of mapping the full value of a text
#' field, the function looks for occurrences of key values within the text using \code{gsub()} to substitute a URL link tag.
#' This allows fields containing multiple IDs to be converted to a group of URL links.
#' @param min_decimal (optional) The minimal value for decimal format. Below this, scientific notation is used (default 0.0005).
#' @param quiet (optional) If \code{FALSE}, this tells the function to emit warnings when an identifier term has no matching
#' URL. By default, this value is \code{TRUE}, suppressing this behavior.
#' @param table_row_colors (optional) This argument specifies the row background colors used to contrast different subnets.
#' (default: c("1"="#EEF","2"="#FFD"), pale blue and pale yellow)
#' @param ... Additional arguments passed to \code{DT::datatable}.
#' @return An attractive HTML table widget, optionally with unique IDs represented as links.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'    # Export merged pathways/subnets data:
#'    analysis.mergePathways <- gsnMergePathways( object = analysis.GSN )
#'
#'    # Convert IDs in analysis.mergePathways into a named list of URLs:
#'    url_map_l <- list()
#'
#'    # To link to a gene set page on MSigDB's website, this base URL can be used:
#'    msig_url <- "http://www.gsea-msigdb.org/gsea/msigdb/geneset_page.jsp"
#'
#'    # This method works for MSigDB IDs (e.g. "M5928"), with parameter 'systematicName':
#'    url_map_l[['ID']] <-
#'      with( analysis.mergePathways,
#'        structure(paste0( msig_url, "?systematicName=", ID),
#'                   names = ID
#'                )
#'           )
#'
#'    # If MSigDB STANDARD_NAMES (e.g. "GO_RESPONSE_TO_GLUCAGON") are present in the
#'    # pathways data, they can be linked to URLs using parameter 'geneSetName':
#'    url_map_l[['STANDARD_NAME']] <-
#'        with( analysis.mergePathways,
#'          structure( paste0(msig_url, "?geneSetName=", STANDARD_NAME),
#'                     names = STANDARD_NAME
#'                   )
#'            )
#'    # Print HTML table:
#'    yassifyPathways( pathways = analysis.mergePathways,
#'                     n = 200,
#'                     url_map_list = url_map_l )
#'
#' }
#'
#'
#' @importFrom utils head
# This version of yassifyPathways allows mapping of IDs by word. It's less efficient and slower, though.
yassifyPathways <- function( pathways,
                             n = NULL,
                             url_map_list = list(),
                             url_map_by_words_list = list(),
                             min_decimal = 0.0005,
                             quiet = TRUE,
                             table_row_colors = c("1"="#EEF","2"="#FFD"),
                             ...
){
  if( is.null(n) ){ n <- nrow(pathways) }

  small_numeric_cols <- character()

  other_numeric_cols <- colnames( pathways )[ sapply( X = colnames(pathways),
                                                      FUN = function(x)
                                                        class( pathways[[x]] ) == "numeric" && ! x %in% small_numeric_cols   ) ]

  for( column in other_numeric_cols ){
    pathways[[column]] <- sapply( X = pathways[[column]],
                                  FUN = function(x){
                                    if( is.na( x ) ) return( NA )
                                    if( abs( x ) > 1000000 || abs(x) < min_decimal )
                                      return( format( x, scientific = TRUE, digits = 5 ) )
                                    return( format( x, scientific = FALSE, digits = 5 ) )
                                  } )
  }

  for( column in names( url_map_list ) ){
    if( ! is.null( pathways[[column]] ) ){
      pathways[[column]] <- sapply( X = pathways[[column]],
                                    FUN = function(x){
                                      if( is.na( x ) ) return( "" )
                                      if( ! x %in% names(url_map_list[[column]]) ){
                                        if( ! quiet ) warning("Term '", x, "' not found.\n")
                                        return( x )
                                      }
                                      url <- try( url_map_list[[column]][[x]] )
                                      if( is.na( url ) ) return( x )
                                      if( "try-error" %in% class( url ) ){
                                        return( x )
                                      }
                                      paste0( "<a href=\"",url,"\" target=\"_blank\">", x, "</a>" )
                                    } )
    }
  }

  for( column in names( url_map_by_words_list ) ){
    if( ! is.null( pathways[[column]] ) ){
      .map <- as.list(url_map_by_words_list[[column]])
      pathways[[column]] <- sapply( X = pathways[[column]],
                                    FUN = function(x) try( {
                                      ids_v <- unlist(stringr::str_match_all( string = x, pattern = "\\w+" ))
                                      x_cp <- x
                                      for( id in ids_v ){
                                        url <- .map[[id]]
                                        if( !is.null( url ) & !is.na( url ) & nchar(url) > 0 )
                                          x_cp <- gsub( pattern = id,
                                                        replacement = paste0( "<a href=\"",url,"\" target=\"_blank\">",
                                                                              id, "</a>" ),
                                                        x = x_cp )
                                      }
                                      x_cp
                                    } ) )
    }
  }
  pathways <- utils::head( pathways, n )
  .dt <- DT::datatable( pathways,
                        escape=FALSE,
                        rownames=FALSE, ... )

  #if( all( c("subnet", "subnetRank" ) %in% colnames( pathways ) ) ){
  if( "subnet" %in% colnames( pathways ) && !is.null(table_row_colors) && length(table_row_colors) > 0 ){
    subnet.id <- unique( pathways$subnet )

    # Convert to integers. If all subnet values are numeric and > 0 integer:
    if( !  any( suppressWarnings( is.na( as.numeric( subnet.id ) ) ) ) &&
        all( as.numeric( subnet.id ) >= 1 ) &&
        all( as.numeric( subnet.id ) == as.integer( subnet.id ) )
    ) { # Convert to integer directly.
      subnet.id <- as.integer( subnet.id )
    } else { # Otherwise, use factor:
      subnet.if <- as.integer( factor( subnet.id ) )
    }

    #subnet.val <- c("1"="#EEE","2"="#FFF")[(as.numeric( subnet.id ) %% 2) + 1]
    #subnet.val <- table_row_colors[(as.numeric( factor(subnet.id) ) %% length(table_row_colors)) ]

    subnet.val <- table_row_colors[(( subnet.id  - 1) %% length(table_row_colors)) + 1]

    DT::formatStyle( .dt,
                     'subnet',
                     target = 'row',
                     backgroundColor = DT::styleEqual( subnet.id, subnet.val )
    )
  } else {
    .dt
  }
}




