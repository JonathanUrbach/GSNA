



#' yassifyPathways
#'
#' @description Takes a data.frame and outputs an attractively formatted HTML table widget for reports via the
#' using the DT and data.table package. Optionally, the user can specify, via the \code{n} parameter, the number
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
#'
#' # The sample data object Bai_CiKrt_DN.cerno contains MSigDB
#' # systematic names as gene set identifiers in its ID column
#' # that we can map to URLs on MSigDB's website using the
#' # 'systematicName' URL parameter:
#' msig_url <- "http://www.gsea-msigdb.org/gsea/msigdb/geneset_page.jsp"
#' id2url <- with( Bai_CiKrt_DN.cerno,
#'                 structure(paste0( msig_url, "?systematicName=", ID),
#'                           names = ID
#'                          )
#'               )
#'
#' # NOTE: In GSEA data sets against MSigDB,
#' # MSigDB STANDARD_NAMES (e.g. "GO_RESPONSE_TO_GLUCAGON")
#' # are often present in the pathways data instead of
#' # systematic name identifiers. They can be linked to URLs
#' # using the 'geneSetName' parameter, as follows:
#' #  sn2url <-
#' #   with( Bai_CiKrt_DN.gsea,
#' #       structure( paste0(msig_url, "?geneSetName=", STANDARD_NAME),
#' #                  names = STANDARD_NAME
#' #                )
#' #       )
#'
#' # The named vector id2url now contains URLs for MSigDB
#' # gene sets, names with the gene set ID. By passing a
#' # list containing the id2url named as the column we
#' # wish to map to a URL, we can have yassifyPathways
#' # generate an HTML table with links for the gene set IDs.
#' yassifyPathways( Bai_CiKrt_DN.cerno,
#'                  n = 200,
#'                  url_map_list = list(ID = id2url) )
#' # Here the 'n = 200' argument tells the function to
#' # generate an HTML table with just the first 200 results,
#' # and the 'url_map_list = list(ID=id2url)' tells the
#' # function to link the ID column of Bai_CiKrt_DN.cerno
#' # to the mapped URLs in the 'id2url' vector. In this case
#' # the entire ID field is mapped, but if we want to map
#' # in a word-based fashion, for example when a column
#' # may contain multiple IDs per row (eg "M40804, M40775" ),
#' # then the 'url_map_by_words_list = list(ID = id2url)'
#' # argument works:
#' yassifyPathways( Bai_CiKrt_DN.cerno,
#'                  n = 200,
#'                  url_map_by_words_list = list(ID = id2url) )
#' # The url_map_list_by_words argument will work in mos
#' # cases where url_map_list does, so may be fine to use
#' # generally, but it is less efficient and my sometimes be
#' # slower.
#'
#' @importFrom utils head
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




