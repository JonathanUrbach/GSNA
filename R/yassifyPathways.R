



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
#' @param valign (optional) Specifies whether the vertical alignment of text in the table cells should be \code{"top"},
#' \code{"middle"}, or \code{"bottom"}. Defaults to \code{"top"}.
#' @param halign (optional) Specifies whether the horizontal alignment of text in the table cells should be \code{"left"},
#' \code{"center"}, or \code{"right"}. Defaults to \code{"left"}.
#' @param color_by (optional) Specifies the name of a field in a dataframe by which to color rows. (default "subnet").
#'
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
#'
yassifyPathways <- function (pathways,
                             n = NULL,
                             url_map_list = list(),
                             url_map_by_words_list = list(),
                             min_decimal = 5e-04,
                             quiet = TRUE,
                             table_row_colors = c(`1` = "#EEF",
                                                  `2` = "#FFD"),
                             valign = "top",
                             halign = "left",
                             color_by = "subnet",
                             ...)
{
  if (is.null(n)) {
    n <- nrow(pathways)
  }
  small_numeric_cols <- character()
  other_numeric_cols <- colnames(pathways)[sapply(X = colnames(pathways),
                                                  FUN = function(x) class(pathways[[x]]) == "numeric" &&
                                                    !x %in% small_numeric_cols)]
  for (column in other_numeric_cols) {
    pathways[[column]] <- sapply(X = pathways[[column]],
                                 FUN = function(x) {
                                   if (is.na(x))
                                     return(NA)
                                   if (abs(x) > 1e+06 || abs(x) < min_decimal)
                                     return(format(x, scientific = TRUE, digits = 5))
                                   return(format(x, scientific = FALSE, digits = 5))
                                 })
  }
  for (column in names(url_map_list)) {
    if (!is.null(pathways[[column]])) {
      pathways[[column]] <- sapply(X = pathways[[column]],
                                   FUN = function(x) {
                                     if (is.na(x))
                                       return("")
                                     if (!x %in% names(url_map_list[[column]])) {
                                       if (!quiet)
                                         warning("Term '", x, "' not found.\n")
                                       return(x)
                                     }
                                     url <- try(url_map_list[[column]][[x]])
                                     if (is.na(url))
                                       return(x)
                                     if ("try-error" %in% class(url)) {
                                       return(x)
                                     }
                                     paste0("<a href=\"", url, "\" target=\"_blank\">",
                                            x, "</a>")
                                   })
    }
  }
  for (column in names(url_map_by_words_list)) {
    if (!is.null(pathways[[column]])) {
      .map <- as.list(url_map_by_words_list[[column]])
      .tokenized_column <- .yass_tokenizer( pathways[[column]] )

      .urlized_col <- lapply( X = .tokenized_column,
                              FUN = function( x ){
                                sapply(X = x, FUN = function(.x){
                                  ifelse( test = .x %in% names(.map),
                                          yes = paste0("<a href=\"", .map[.x], "\" target=\"_blank\">", .x, "</a>"),
                                          no = .x )
                                } )
                              } )


      pathways[[column]] <- sapply( X = .urlized_col, FUN = function( x ) paste0(x, collapse = "") )
    }
  }


  pathways <- utils::head(pathways, n)
  .dt <- DT::datatable(pathways, escape = FALSE, rownames = FALSE,
                       ...)
  if (!is.null(valign))
    .dt <- DT::formatStyle(table = .dt, columns = 0:ncol(pathways),
                           `vertical-align` = valign)
  if (!is.null(halign))
    .dt <- DT::formatStyle(table = .dt, columns = 0:ncol(pathways),
                           `text-align` = halign)
  if (color_by %in% colnames(pathways) && !is.null(table_row_colors) &&
      length(table_row_colors) > 0) {
    subnet.id <- unique(pathways[[color_by]])
    if (!any(suppressWarnings(is.na(as.numeric(subnet.id)))) &&
        all(as.numeric(subnet.id) >= 1) && all(as.numeric(subnet.id) ==
                                               as.integer(subnet.id))) {
      subnet.id <- as.integer(subnet.id)
    }
    else {
      subnet.if <- as.integer(factor(subnet.id))
    }
    subnet.val <- table_row_colors[((1:length(subnet.id) - 1)%%length(table_row_colors)) + 1]
    DT::formatStyle(.dt, color_by, target = "row", backgroundColor = DT::styleEqual(subnet.id,
                                                                                    subnet.val))
  }
  else {
    .dt
  }
}


.yass_tokenizer <- function(x){
  .outz <- list()
  for( .x in x ){
    .tokenz <- c()
    while( !is.na(.x) ){
      if( !is.na( (.cptr <- stringr::str_match( string = .x, pattern = "^(\\w+|\\W+)(.+)?" ) )[1,2] ) ){ # Letter, digit, underscore
        .tokenz <- c( .tokenz, .cptr[1,2] )
        .x <- .cptr[1,3]
      }
    }
    .outz[[length(.outz)+1]] <- .tokenz
  }
  .outz
}






