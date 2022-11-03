
#' gsnPlotNetwork
#'
#' @description Function for plotting the networks within GSNData objects.
#'
#' @param object A GSNData object containing a pared distance matrix with \code{edges}.
#' @param pathways.data (optional) data.frame containing associated pathways data. This defaults to whatever pathways
#' data has already been imported into this GSNData object in \code{object$pathways$data}.
#' @param distance (optional) The name of a distance metric used, defaults to whatever \code{default_distance} is.
#' @param id_col (optional) This is the name of the column in the pathways data.frame that corresponds to the names of
#' gene sets. The default value is specified by \code{object$pathways$id_col}. (See details.)
#' @param substitute_id_col (optional) This is the name of the column that is to be substituted for the \code{id_col}
#' column when labeling network vertices. (See details.)
#' @param stat_col (optional) This is the name of the column in the pathways data.frame that contains a significance
#' value for coloring network vertices. The default value is specified by \code{object$pathways$stat_col}.
#'
#' @param stat_col_2 (optional)
#'
#' @param sig_order (optional) This indicates the behavior of \code{stat_col}, whether low values (\code{'loToHi'}) or
#' high values (\code{'hiToLo'}) are most significant. The default value is specified in \code{object$pathways$sig_order}.
#'
#' @param sig_order_2 (optional)
#'
#' @param optimal_extreme (optional) This indicates the behavior of the statistic used to generate the distance metric,
#' specifically whether low values (\code{'min'}) or high values \code{'max'} are to be regarded as close. This is used
#' for scaling the width and the color of the edges connecting vertices.
#' @param transform_function (optional) This is a function to transform the values in \code{stat_col} so that they are
#' amenable to color-scaling. For *p*-values, a log transformation is often useful, but can produce negative infinities
#' if the transformation is applied to zero. By default the function is the \code{nzLog10} (non-zero log10) function,
#' provided by this package, which adds a small pseudocount to p-values when log10 transforming values equal to zero. If
#' values in \code{stat_col} are less than zero, then log10 transformation is inappropriate and will introduce NAs, and
#' therefore some other method should be used.
#' @param pathways_title_col (optional) Indicates a column to be used as the 'Title' column for network vertices.
#' (See details.)
#' @param edge_colors (optional) A vector of colors included to generate a scale represent the numerical value of the
#' edge distances. By default, the colors are arranged as a rainbow with black and purple representing the greatest
#' distannces, and orange and red the nearest distances.
#'
#' @param vertex_colors (optional)
#'
#' @param vertex_colors.1 (optional)
#'
#' @param vertex_colors.2 (optional)
#'
#' @param filename (optional) An output file name for the plot. If 'out_format' is not set (see below), the output
#' file type will be determined by the file suffix, which can be \code{'.svg'}, \code{'.pdf'}, or \code{'.png'}. If
#' the \code{out_format} cannot be determined from the file name, than it may be manually set with \code{out_format}.
#' If the output file type cannot be determined from the \code{filename} or \code{out_format} arguments, an error will
#' be thrown.
#' @param out_format (optional) Output filetype when \code{filename} is specified.
#' @param width (optional) Sets the width of the output canvas in inches. Defaults to the width of the present
#' graphical device.
#' @param height (optiona) Sets the height of the output canvas in inches. Defailts to the height of the present
#' graphical device.
#' @param vertex.shape (optional) Shape of the vertex, passed to \code{igraph::plot.igraph}. By default, the value is
#' \code{'circle'}.
#' @param vertex.size (optional) Size of vertices, passed to \code{igraph::plot.igraph}. By default, the value is
#' NULL, and the function attempts to pick a reasonable value based on the canvas size and the number of gene sets.
#' @param vertex.label.cex (optional) Size of vertex labels, passed to \code{igraph::plot.igraph}. As with vertex.size,
#' by default, the value is NULL, and the function attempts to pick a reasonable value based on the canvas size and
#' the number of gene sets.
#' @param max_edge_width (optional) Size of vertex labels, passed to \code{igraph::plot.igraph}. By default, the value
#' is NULL, and the function attempts to pick a reasonable value based on the canvas size and the number of gene sets.
#' @param edge_arrow_size (optional) Size of vertex labels, passed to \code{igraph::plot.igraph}. By default, the value
#' is NULL, and the function attempts to pick a reasonable value based on the canvas size and the number of gene sets.
#' @param seed (optional) This is a seed that the function uses to generate a plot layout. By default it is 29189892,
#' and this results in a repeatable behavior for plots. However, to randomize the plot layout behavior, this value may
#' be set to NULL, or if some other repeatable layout is desired, another seed may be used.
#' @param layout (optional) Either a function that generates a layout or a numerical matrix containing a vertex layout
#' with two columns corresponding to *x* and *y* coordinates. This argument is passed to the \code{igraph} plot method
#' that is subsequently called by \code{gsnPlotNetwork()} (see \code{.plot}, below). The default \code{layout} is
#' the anonymous function \code{function(x){igraph::layout_with_fr(x, grid = "nogrid" )}}, which calls
#' \code{igraph::layout_with_fr()} (implementing Fruchterman-Reingold layout) with the \code{grid="nogrid"} option,
#' enabling proper layout of networks with >= 1000 gene set vertices. Other useful layouts for \code{igraph} networks
#' include \code{igraph::layout_with_fr} (default Fruchterman-Reingold), \code{igraph::layout_with_dh} (implementing
#' Davidson-Harel layout), \code{igraph::layout_as_tree}, \code{igraph::layout_nicely}, and others.
#' For more details about layouts, see \code{\link[igraph]{igraph.plotting}}.
#' @param .plot (optional) A plot function used to render the internally generated \code{igraph} object. By default
#' \code{igraph::plot.igraph} is used, but for interactive plotting, \code{igraph::tkplot} may be used. For more
#' details about plotting, see \code{\link[igraph]{igraph.plotting}}.
#'
#' @return An \code{igraph} network object is returned, invisibly.
#'
#' @details This function is primarily for taking \code{GSNData} object containing a distance matrix, an associated
#' \code{edges} edge-list and pathways data, and generating and rendering a corresponding \code{igraph} object. The
#' function attempts to plot the corresponding network with vertices labeled with a gene set \code{ID} and corresponding
#' \code{Title}, and colored according to the significance values represented in \code{stat_col} using \code{sig_order}
#' as an indicator of whether high or low values are more significant. Edges are scaled by the value of the value of the
#' distance statistic in the pared distance matrix.
#'
#' When the parameters \code{vertex.shape}, \code{vertex.size}, \code{vertex.label.cex}, \code{max_edge_width}, and
#' \code{edge_arrow_size} are not specified, the function attempts to pick reasonable values. These parameters are
#' assembled into a list and attached to the returned \code{igraph} object as an attribute named \code{GSNA_plot_params}.
#' To optimize plots, the user can examine these parameters by calling the following on the output of the function:
#'
#' \code{attr( x = nw.igraph, which = "GSNA_plot_params" )}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' gsnPlotNetwork( object = analysis.GSN )
#' }
#'
#' @seealso \code{\link{plot.GSNData}}, \code{\link{gsnToIgraph}}, \code{\link[igraph]{plot.igraph}}
#'

#' @importFrom grDevices dev.size svg pdf png
#'
gsnPlotNetwork <- function( object,
                            pathways.data = NULL,
                            distance = NULL,
                            id_col = NULL,
                            substitute_id_col = NULL,
                            stat_col = NULL,
                            stat_col_2 = NULL,  # NA suppresses 2-color plotting behavior
                            sig_order = NULL,
                            sig_order_2 = NULL,
                            optimal_extreme = NULL,
                            transform_function = nzLog10,
                            pathways_title_col = 'Title',
                            edge_colors = c("black", "purple", "blue", "green","yellow4", "orange","red"),
                            vertex_colors = c("white","yellow","red"),
                            vertex_colors.1 = c("white", "red" ),
                            vertex_colors.2 = c("white", "blue" ),
                            filename = NULL,
                            out_format = NULL,
                            width = NULL,
                            height = NULL,
                            vertex.shape = "circle",
                            vertex.size = NULL,
                            vertex.label.cex = NULL,
                            max_edge_width = NULL,
                            edge_arrow_size = NULL,
                            seed = 29189892,
                            layout = function(x){igraph::layout_with_fr(x, grid = "nogrid" )},
                            .plot = igraph::plot.igraph
){
  stopifnot( class( object ) == "GSNData" )
  if( is.null(distance) ) distance <- object$default_distance
  if( is.null(pathways.data) ) pathways.data <- object$pathways$data
  if( !is.null(pathways.data) ){
    if( is.null(id_col) ) id_col <- object$pathways$id_col
    if( is.null(stat_col) ) stat_col <- object$pathways$stat_col
    if( is.null(sig_order) ) sig_order <- object$pathways$sig_order
    if( is.null(id_col) ) stop( "id_col is not defined" )
    if( is.null(stat_col) ) stop( "stat_col is not defined" )
    if( is.null(sig_order) ) stop( "sig_order is not defined" )
    # adding stat_col_2 and sig_order_2 for two-color networks:
    if( is.null(stat_col_2) ){
      stat_col_2 <- object$pathways$stat_col_2
    } else if( is.na(stat_col_2) ){
      stat_col_2 <- NULL;
    }
    if( is.null(sig_order_2) ) sig_order_2 <- object$pathways$sig_order_2
    if( is.null(sig_order_2) ) sig_order_2 <- object$pathways$sig_order
    #
    rownames(pathways.data) <- pathways.data[[id_col]]
  }
  if( is.null( optimal_extreme ) ) optimal_extreme <- object$distances[[distance]]$pared_optimal_extreme
  if( is.null( optimal_extreme ) ) optimal_extreme <- object$distances[[distance]]$optimal_extreme
  if( is.null( optimal_extreme ) ) stop( "optimal_extreme is not defined" )

  if( is.null( width ) ) width <- grDevices::dev.size("in")[1]
  if( is.null( height ) ) height <- grDevices::dev.size("in")[2]

  sigNet <- gsnToIgraph( object, distance )

  vertex_count <- length(igraph::V( sigNet ) )

  # Weirdly, vertex sizes seem to be callibrated relative to canvas size
  if( is.null( vertex.size ) ) vertex.size <- round( 100 / sqrt( vertex_count ), digits = 1 )
  # Vertex labels and edge width are callibrated on an absolute scale.
  if( is.null( vertex.label.cex ) ) vertex.label.cex <- round( 0.27 * min( width, height ) /sqrt( vertex_count ), digits = 3 )
  if( is.null( max_edge_width ) ) max_edge_width <- 10 *  min( width, height ) /sqrt( vertex_count )

  # Node characteristics in this implementation are dependent on pathways.data, whereas edge characterisics are
  # dependent on the distance matrix.
  if( ! is.null( pathways.data ) ){

    if( !is.null( stat_col_2 ) ){
      pathways.data$color <-
        myColorF_2color( numbers.1 = c(loToHi=-1, hiToLo = 1)[[as.character(sig_order)]] *
                           transform_function(pathways.data[[stat_col]]),
                         numbers.2 = c(loToHi=-1, hiToLo = 1)[[as.character(sig_order_2)]] *
                           transform_function(pathways.data[[stat_col_2]]),
                         colors.1 = vertex_colors.1,
                         colors.2 = vertex_colors.2,
                         n = 100 )
    } else {
      pathways.data$color <- myColorF( numbers = c(loToHi=-1, hiToLo = 1)[[as.character(sig_order)]] * transform_function(pathways.data[[stat_col]]),
                                       n = 100,
                                       colors = vertex_colors )
    }

    igraph::vertex_attr(sigNet, "color") <- pathways.data[igraph::V(sigNet)$name,"color"]

    titles_v <- c( "Title", "Name", "NAME", "ID" )
    if( is.null( pathways_title_col ) )
      pathways_title_col <- titles_v[titles_v %in% colnames(pathways.data)][1]

    if( !is.na( pathways_title_col ) && !is.null( pathways_title_col ) ){
      id <- igraph::V(sigNet)$name
      if( ! is.null( substitute_id_col ) )
        id <- pathways.data[igraph::V(sigNet)$name, substitute_id_col ]

      igraph::V(sigNet)$label <- paste0( id,
                                         "\n",
                                         gsub( x = pathways.data[igraph::V(sigNet)$name, pathways_title_col ],
                                               pattern = '(.{1,15})(\\s)',
                                               replacement = '\\1\n' ) )# Adds converts '\s' to '\n' after up to 1
    } else if( ! is.null( substitute_id_col ) ){
      id <- pathways.data[igraph::V(sigNet)$name, substitute_id_col ]
      igraph::V(sigNet)$label <- id
    }
  }

  paredDistRange <- structure(  range( object$distances[[distance]]$pared, na.rm = TRUE ), names = c("min","max" ) )

  if( optimal_extreme == "min" ){
    paredDistRange <- structure(rev( paredDistRange ), names = c("min","max" ) )
  }
  scale_fun <- function(x){ (x - paredDistRange[['min']]) / ( paredDistRange[['max']] - paredDistRange[['min']] ) }
  convert_fun <- function(x){ scale_fun(object$distances[[distance]]$pared[x[1], x[2]] ) }

  igraph::E(sigNet)$width <-
    1+as.integer( apply( X = read.table( text = attr( igraph::E(sigNet),"vnames" ), sep = "|"),
                         MARGIN = 1,
                         FUN = convert_fun ) * max_edge_width )

  # The arrow.size argument doesn't seem to work properly for igraph, so we're making it configurable, but not
  # used by default.
  if( !is.null(  edge_arrow_size ) ) igraph::E(sigNet)$arrow.size <- edge_arrow_size

  igraph::E(sigNet)$color <-
    myColorF( numbers = 1+as.integer(apply(X = read.table(text = attr( igraph::E(sigNet),"vnames" ), sep = "|"),
                                           MARGIN = 1,
                                           FUN = convert_fun ) * (length( edge_colors ) - 1 ) ),
              n = length( edge_colors ),
              colors = edge_colors )

  igraph::V(sigNet)$shape <- vertex.shape
  igraph::V(sigNet)$size <- vertex.size
  igraph::V(sigNet)$label.cex <- vertex.label.cex

  if( is.null(out_format) ){
    if( ! is.null( filename ) ){
      if( stringr::str_detect( string =  filename, pattern = stringr::regex( "\\.svg$", ignore_case = TRUE) ) ){
        out_format <- "svg"
      } else if( stringr::str_detect( string =  filename, pattern = stringr::regex( "\\.pdf$", ignore_case = TRUE) ) ){
        out_format <- "pdf"
      } else if( stringr::str_detect( string =  filename, pattern = stringr::regex( "\\.png$", ignore_case = TRUE) ) ){
        out_format <- "png"
      } else {
        stop( "Need to specify output type." )
      }
    } else {
      out_format <- 'plot'
    }
  } else if ( out_format %in% c("svg", "pdf", "png") & is.null(filename) ){
    stop( "filename argument needed for svg, pdf, or png" )
  }

  # Default is 'plot'
  do_nothing <- function(file = NULL, width = width, height = height){}
  out_fun <- do_nothing
  close_fun <- do_nothing

  if( out_format == "svg" ){
    out_fun <- grDevices::svg
    close_fun <- grDevices::dev.off
  } else if( out_format == "pdf" ){
    out_fun <- grDevices::pdf
    close_fun <- grDevices::dev.off
  }  else if( out_format == "png" ){
    out_fun <- grDevices::png
    close_fun <- grDevices::dev.off
  }

  # Store plotting parameters as GSNA_plot_params attribute.
  attr( x = sigNet, which = "GSNA_plot_params" ) <- list(width = width,
                                                         height = height,
                                                         vertex.size = vertex.size,
                                                         vertex.label.cex = vertex.label.cex,
                                                         vertex.shape =  vertex.shape,
                                                         seed = seed,
                                                         max_edge_width = max_edge_width
  )

  {
    out_fun( file = filename, width = width, height = height  )
    par( mar = c(0.5, 0.5, 0.5, 0.5) )
    if( !is.null( seed ) ){
      withr::with_seed( seed = seed, code = .plot(sigNet, layout = layout ) )
    } else {
      .plot(sigNet, layout = layout )
    }
    close_fun() -> out
  }
  invisible( sigNet )
}



# gsnPlotNetwork <- function( object,
#                             pathways.data = NULL,
#                             distance = NULL,
#                             id_col = NULL,
#                             substitute_id_col = NULL,
#                             stat_col = NULL,
#                             sig_order = NULL,
#                             optimal_extreme = NULL,
#                             transform_function = nzLog10,
#                             pathways_title_col = 'Title',
#                             edge_colors = c("#000000FF", "purple", "blue", "green","yellow4", "orange","red"),
#                             filename = NULL,
#                             out_format = NULL,
#                             width = NULL,
#                             height = NULL,
#                             vertex.shape = "circle",
#                             vertex.size = NULL,
#                             vertex.label.cex = NULL,
#                             max_edge_width = NULL,
#                             edge_arrow_size = NULL,
#                             seed = 29189892,
#                             layout = function(x){igraph::layout_with_fr(x, grid = "nogrid" )},
#                             .plot = igraph::plot.igraph
# ){
#   stopifnot( class( object ) == "GSNData" )
#   if( is.null(distance) ) distance <- object$default_distance
#   if( is.null(pathways.data) ) pathways.data <- object$pathways$data
#   if( !is.null(pathways.data) ){
#     if( is.null(id_col) ) id_col <- object$pathways$id_col
#     if( is.null(stat_col) ) stat_col <- object$pathways$stat_col
#     if( is.null(sig_order) ) sig_order <- object$pathways$sig_order
#     if( is.null(id_col) ) stop( "id_col is not defined" )
#     if( is.null(stat_col) ) stop( "stat_col is not defined" )
#     if( is.null(sig_order) ) stop( "sig_order is not defined" )
#     rownames(pathways.data) <- pathways.data[[id_col]]
#   }
#   if( is.null( optimal_extreme ) ) optimal_extreme <- object$distances[[distance]]$pared_optimal_extreme
#   if( is.null( optimal_extreme ) ) optimal_extreme <- object$distances[[distance]]$optimal_extreme
#   if( is.null( optimal_extreme ) ) stop( "optimal_extreme is not defined" )
#
#   if( is.null( width ) ) width <- dev.size("in")[1]
#   if( is.null( height ) ) height <- dev.size("in")[2]
#
#   sigNet <- gsnToIgraph( object, distance )
#
#   vertex_count <- length(igraph::V( sigNet ) )
#
#   # Weirdly, vertex sizes seem to be callibrated relative to canvas size
#   if( is.null( vertex.size ) ) vertex.size <- round( 100 / sqrt( vertex_count ), digits = 1 )
#   # Vertex labels and edge width are callibrated on an absolute scale.
#   if( is.null( vertex.label.cex ) ) vertex.label.cex <- round( 0.27 * min( width, height ) /sqrt( vertex_count ), digits = 3 )
#   if( is.null( max_edge_width ) ) max_edge_width <- 10 *  min( width, height ) /sqrt( vertex_count )
#
#   # Node characteristics in this implementation are dependent on pathways.data, whereas edge characterisics are
#   # dependent on the distance matrix.
#   if( ! is.null( pathways.data ) ){
#     pathways.data$color <- myColorF( c(loToHi=-1, hiToLo = 1)[[as.character(sig_order)]] * transform_function(pathways.data[[stat_col]]) )
#     igraph::vertex_attr(sigNet, "color") <- pathways.data[igraph::V(sigNet)$name,"color"]
#
#     titles_v <- c( "Title", "Name", "NAME", "ID" )
#     if( is.null( pathways_title_col ) )
#       pathways_title_col <- titles_v[titles_v %in% colnames(pathways.data)][1]
#
#     if( !is.na( pathways_title_col ) && !is.null( pathways_title_col ) ){
#       id <- igraph::V(sigNet)$name
#       if( ! is.null( substitute_id_col ) )
#         id <- pathways.data[igraph::V(sigNet)$name, substitute_id_col ]
#
#       igraph::V(sigNet)$label <- paste0( id,
#                                          "\n",
#                                          gsub( x = pathways.data[igraph::V(sigNet)$name, pathways_title_col ],
#                                                pattern = '(.{1,15})(\\s)',
#                                                replacement = '\\1\n' ) )# Adds converts '\s' to '\n' after up to 1
#     } else if( ! is.null( substitute_id_col ) ){
#       id <- pathways.data[igraph::V(sigNet)$name, substitute_id_col ]
#       igraph::V(sigNet)$label <- id
#     }
#   }
#
#   paredDistRange <- structure(  range( object$distances[[distance]]$pared, na.rm = TRUE ), names = c("min","max" ) )
#
#   if( optimal_extreme == "min" ){
#     paredDistRange <- structure(rev( paredDistRange ), names = c("min","max" ) )
#   }
#   scale_fun <- function(x){ (x - paredDistRange[['min']]) / ( paredDistRange[['max']] - paredDistRange[['min']] ) }
#   convert_fun <- function(x){ scale_fun(object$distances[[distance]]$pared[x[1], x[2]] ) }
#
#   igraph::E(sigNet)$width <-
#     1+as.integer( apply( X = read.table( text = attr( igraph::E(sigNet),"vnames" ), sep = "|"),
#                          MARGIN = 1,
#                          FUN = convert_fun ) * max_edge_width )
#
#   # The arrow.size argument doesn't seem to work properly for igraph, so we're making it configurable, but not
#   # used by default.
#   if( !is.null(  edge_arrow_size ) ) igraph::E(sigNet)$arrow.size <- edge_arrow_size
#
#   igraph::E(sigNet)$color <-
#     myColorF( numbers = 1+as.integer(apply(X = read.table(text = attr( igraph::E(sigNet),"vnames" ), sep = "|"),
#                                            MARGIN = 1,
#                                            FUN = convert_fun ) * (length( edge_colors ) - 1 ) ),
#               n = length( edge_colors ),
#               colors = edge_colors )
#
#   igraph::V(sigNet)$shape <- vertex.shape
#   igraph::V(sigNet)$size <- vertex.size
#   igraph::V(sigNet)$label.cex <- vertex.label.cex
#
#   if( is.null(out_format) ){
#     if( ! is.null( filename ) ){
#       if( stringr::str_detect( string =  filename, pattern = stringr::regex( "\\.svg$", ignore_case = TRUE) ) ){
#         out_format <- "svg"
#       } else if( stringr::str_detect( string =  filename, pattern = stringr::regex( "\\.pdf$", ignore_case = TRUE) ) ){
#         out_format <- "pdf"
#       } else if( stringr::str_detect( string =  filename, pattern = stringr::regex( "\\.png$", ignore_case = TRUE) ) ){
#         out_format <- "png"
#       } else {
#         stop( "Need to specify output type." )
#       }
#     } else {
#       out_format <- 'plot'
#     }
#   } else if ( out_format %in% c("svg", "pdf", "png") & is.null(filename) ){
#     stop( "filename argument needed for svg, pdf, or png" )
#   }
#
#   # Default is 'plot'
#   do_nothing <- function(file = NULL, width = width, height = height){}
#   out_fun <- do_nothing
#   close_fun <- do_nothing
#
#   if( out_format == "svg" ){
#     out_fun <- svg
#     close_fun <- dev.off
#   } else if( out_format == "pdf" ){
#     out_fun <- pdf
#     close_fun <- dev.off
#   }  else if( out_format == "png" ){
#     out_fun <- png
#     close_fun <- dev.off
#   }
#
#   # Store plotting parameters as GSNA_plot_params attribute.
#   attr( x = sigNet, which = "GSNA_plot_params" ) <- list(width = width,
#                                                          height = height,
#                                                          vertex.size = vertex.size,
#                                                          vertex.label.cex = vertex.label.cex,
#                                                          vertex.shape =  vertex.shape,
#                                                          seed = seed,
#                                                          max_edge_width = max_edge_width
#                                                         )
#
#   {
#     out_fun( file = filename, width = width, height = height  )
#     par( mar = c(0.5, 0.5, 0.5, 0.5) )
#     if( !is.null( seed ) ){
#       withr::with_seed( seed = seed, code = .plot(sigNet, layout = layout ) )
#     } else {
#       .plot(sigNet, layout = layout )
#     }
#     close_fun() -> out
#   }
#   invisible( sigNet )
# }

#' @importFrom grDevices col2rgb
#'
color2IntV <- function( color ){
  as.vector(grDevices::col2rgb(color))
}


intV2Color <- function( rgb_v ){
  rgb_v[is.na(rgb_v ) | rgb_v > 255] <- 255
  rgb_v[rgb_v < 0] <- 0
  paste0( "#", paste0( sprintf( "%02X", round(rgb_v) ), collapse = "" ) )
}


twoColorArray <- function( numbers.1.scale,
                           numbers.2.scale,
                           colors.1 = c("#FFFFFF", "#FF0000"),
                           colors.2 = c("#FFFFFF", "#0000FF"),
                           n = 100,
                           combine_method = "mean" #"default"
){
  if( length( numbers.1.scale ) != length( numbers.2.scale ) ){
    stop( "numbers.1.scale and numbers.2.scale have different numbers of elements." )
  }
  c1min <- color2IntV( colors.1[1] )
  c1max <- color2IntV( colors.1[2] )
  c2min <- color2IntV( colors.2[1] )
  c2max <- color2IntV( colors.2[2] )

  c1.fun <- function( x, channel ){
    round((x / n)  * (c1max[channel] - c1min[channel]) + c1min[channel])}
  c2.fun <- function( x, channel ){
    round((x / n)  * (c2max[channel] - c2min[channel]) + c2min[channel])}

  c1.mat <- matrix( data = NA, nrow = length( numbers.1.scale ), ncol = 3 )
  c2.mat <- matrix( data = NA, nrow = length( numbers.2.scale ), ncol = 3 )

  for( i in 1:length(numbers.1.scale) ){
    for( c in 1:3 ){
      c1.mat[i,c] <- c1.fun( x = numbers.1.scale[[i]], channel = c )
      c2.mat[i,c] <- c2.fun( x = numbers.2.scale[[i]], channel = c )
    }
  }
  if( combine_method == "standard" ){
    c12.mat <- 255 - ((255 - c1.mat) + (255 - c2.mat))
  } else if( combine_method == "mean" ){
    c12.mat <- (c1.mat + c2.mat) / 2
  } else {
    #c12.mat <- round(c1.mat * c2.mat / 255)
    c12.mat <- round(255 * sqrt(c1.mat/255) * sqrt(c2.mat/255))
  }
  apply(X = c12.mat, MARGIN = 1, FUN = function(x){intV2Color( unlist(x) )} )
}

myColorF_2color <- function(numbers.1,
                            numbers.2,
                            n=100,
                            colors.1 =  c( "#FFFFFF", "#FF0000" ),
                            colors.2 =  c( "#FFFFFF", "#0000FF" )){
  numbers.1.scale <- round(n*(numbers.1 - min(numbers.1, na.rm = TRUE))/(max(numbers.1, na.rm = TRUE)-min(numbers.1, na.rm = TRUE)))
  numbers.2.scale <- round(n*(numbers.2 - min(numbers.2, na.rm = TRUE))/(max(numbers.2, na.rm = TRUE)-min(numbers.2, na.rm = TRUE)))
  twoColorArray( numbers.1.scale, numbers.2.scale, colors.1, colors.2, n )
}
