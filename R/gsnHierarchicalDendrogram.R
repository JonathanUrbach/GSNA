
#' gsnHierarchicalDendrogram
#'
#' @description Generate a dendrogram plot of a hierarchical clustered set of GSNA distances.
#'
#' @param object An object of the class \code{GSNData}
#' @param distance (optional) A character vector of length one to indicate the desired distance metric to be used for
#' generating a hierarchical dendrogram, e.g. 'lf', 'jaccard', 'stlf', etc. Defaults to the value of objects
#' \code{default_distance}.
#' @param subnet_colors (optional) A character vector of color codes matching the desired colors for subnets. If null
#' then the colors are set automatically.
#' @param file (optional) A file for outputting an SVG format file.
#' @param width (optional) Number expressing output image width in inches, defaults to 7.
#' @param height (optional) Number expressing output image height in inches, defaults to 0.16 times the number
#' of gene sets.
#' @param mai (optional) Margin size of the dendrogram in inches. It's set to allow plenty of space for the
#' labels on the right side. (default \code{c(0,0,0,5.6)})
#' @param cex (optional) Font magnification parameter, passed to \code{stats:::plot.dendrogram()}
#' @param subnetColorsFunction Function for generating subnet colors. This defaults to
#' \code{gsnDendroSubnetColors_dark()}.
#' @param id_col (optional) Character vector of length 1 indicating the name of the column to be used
#' as an ID key in the pathways dataframe (or modules data if that is used, see below). This column should contain
#' the same values as the names of the gene sets. This defaults to the value of the pathways id_col field.
#' @param pathways_title_col (optional) Character vector of length 1 indicating the name of the column in the pathways or
#' modules data.frame to be used as a Title or descriptor in the plot. If not set the function looks for the
#' following names: "Title", "Name", "NAME", "STANDARD_NAME", and takes the first that it finds. If set to NA,
#' the title part of the label is suppressed.
#' @param substitute_id_col (optional) Character vector of length 1 indicating a column used to substitute an alternative
#' ID for the labeling gene sets in data set. If set to \code{NA}, the ID in the plot is disabled.
#' @param modules (optional) Either a class \code{tmod} object containing \code{MODULES} annotation, or a
#' data.frame also containing such data. This is to be used when a pathways data set is not available or
#' insufficient for including the proper labels in plot.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' gsnHierarchicalDendrogram( object  = analysis.GSN, pathways_title_col = NA )
#' }
#'
#' @seealso \code{\link{gsnPareNetGenericHierarchic}}, \code{\link{gsnPlotNetwork}}
#'
#' @importFrom grDevices svg dev.off
#' @importFrom graphics par
#' @importFrom dendextend labels<-
#'
gsnHierarchicalDendrogram <- function( object,
                                       distance = NULL,
                                       subnet_colors = NULL,
                                       file = NULL,
                                       width = 7,
                                       height = NULL,
                                       mai = c(0,0,0,5.6),
                                       cex = 0.7,
                                       subnetColorsFunction = gsnDendroSubnetColors_dark,
                                       id_col = NULL,
                                       pathways_title_col = c("Title", "Name", "NAME", "STANDARD_NAME" ),
                                       substitute_id_col = NULL,
                                       modules = NULL
){
  stopifnot( class( object ) == "GSNData" )

  if( is.null( distance ) ) distance <- object$default_distance
  if( is.null( distance ) ) stop( 'Need distance argument.' )

  if( !is.null( modules ) ){
    if( class(modules) == 'tmod' ){
      pathways_dat <- modules$MODULES
    } else {
      pathways_dat <- modules
    }
    if( ! is.null( id_col ) )
      rownames(pathways_dat) <- pathways_dat[[id_col]]
    if( is.null( pathways_title_col ) ) pathways_title_col <- 'Title'
  } else {
    if( is.null( object$pathways ) || is.null( object$pathways$data ) )
      stop( "Pathways data is missing." )
    pathways_dat <- object$pathways$data
    if( is.null( id_col ) ) id_col <- object$pathways$id_col
    if( is.null( id_col ) ) stop( "Need id_col." )
    rownames( pathways_dat ) <- pathways_dat[[id_col]]
  }

  # Scan for pathways_title_column.
  if( ! is.null(pathways_title_col) && ! is.na(pathways_title_col) ){
    pathways_title_col <- pathways_title_col[pathways_title_col %in% colnames( pathways_dat )][[1]]
    if(length(pathways_title_col) == 0)
      stop("Cannot find pathways Title column.\nSet correct pathways column with pathways_title_column='NAME'",
           " or opt out with pathways_title_column=NULL.\n")
  }

  GSN.dend <- stats::as.dendrogram( object$distances[[distance]]$hclust )
  subnets.lf <- with( object$distances[[distance]]$vertex_subnets, structure( subnet, names = vertex ) )

  if( is.null( height ) ){
    height <- length(object$distances[[distance]]$vertices) * 0.16
  }

  # If subnet_colors is null, generate new set of colors:
  if( is.null(subnet_colors) ){
    subnet_colors <- subnetColorsFunction( subnets = object$distances[[distance]]$subnets )
  }

  # If it's too short, add black for unspecified subnet.
  if( length( unique( subnets.lf ) ) > length( subnet_colors ) ){
    subnet_colors <- structure( c( subnet_colors, rep( "#000000", length( unique( subnets.lf ) ) - length( subnet_colors ) ) ),
                                names = unique( subnets.lf ) )
  }
  # Get vertex names first... because we're going to change them.
  vertex.names <- stats:::labels.dendrogram(GSN.dend)

  if( is.null(substitute_id_col) ) substitute_id_col <- id_col

  dendextend::labels_colors(GSN.dend) <- subnet_colors[subnets.lf[vertex.names]]

  if( !is.na( substitute_id_col ) && !is.na( pathways_title_col ) ){
    #dendextend::labels(GSN.dend) <- with(pathways_dat[vertex.names,],
    #                                      sprintf("%7s %10s",
    #                                              string = get(substitute_id_col),
    #                                              get(pathways_title_col) ) )
    GSN.dend <- dendextend::`labels<-`( GSN.dend,
                                        with(pathways_dat[vertex.names,],
                                             sprintf("%7s %10s",
                                             string = get(substitute_id_col),
                                             get(pathways_title_col) ) ) )
  } else if ( !is.na( substitute_id_col ) ){
    #dendextend::labels(GSN.dend) <- sprintf("%10s", pathways_dat[vertex.names, substitute_id_col] )
    GSN.dend <- dendextend::`labels<-`(GSN.dend,
                                       sprintf("%10s", pathways_dat[vertex.names, substitute_id_col] ) )
  } else if ( !is.na( pathways_title_col ) ){
    #dendextend::labels(GSN.dend) <- sprintf("%20s", pathways_dat[vertex.names,pathways_title_col] )
    GSN.dend <- dendextend::`labels<-`(GSN.dend,
                                       sprintf("%20s", pathways_dat[vertex.names,pathways_title_col] ) )
  } else # dendextend::labels(GSN.dend) <- ""
    GSN.dend <- dendextend::`labels<-`(GSN.dend, "" )

  if( !is.null( file ) )  grDevices::svg( filename = file, width = width, height = height )
  #par(mar = mar, cex = cex)
  #par(mar = c(0,0,0,40), cex = cex )
  graphics::par(mai = mai, cex = cex )
  stats:::plot.dendrogram(GSN.dend, horiz = TRUE)
  if( !is.null( file ) ) grDevices::dev.off() -> uninteresting_return_value
}

