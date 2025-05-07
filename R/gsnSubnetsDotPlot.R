
#' gsnSubnetsDotPlot
#'
#' @description This function generates a **ggplot2** or (if the \code{interactive=TRUE} argument is specified)
#' plotly dotplot representation of gene set clusters/subnets. Gene sets are represented on the y-axis,
#' whereas the x-axis position, color, and size of dots represent user-chosen statistics for each gene set.
#'
#' NOTE: Unlike the \code{gsnPlotNetwork()} and \code{genHierarchicalDendrogram()} methods,
#' \code{gsnSubnetsDotPlot()} does not currently have direct support for output to a file. For outputting to
#' a file, use the **ggplot2** \code{ggsave()} function for **ggplot** objects, or for **plotly** objects,
#' use the \code{saveWidget()} function from the **htmlwidget** package.
#'
#' @param object A GSNData object.
#' @param pathways.data A pathways data.frame. If not specified, this defaults to the pathways data.frame that
#' was imported prior to subnet assignment.
#' @param distance A specifier for distance metric. If not specified, this defaults to the return value of
#' \code{gsn_default_distance(object)}.
#' @param id_col Specifies the id column of the pathways data.frame. (default: value of
#' \code{object$pathways$id_col})
#' @param stat_col Specifies the id column of the pathways data.frame to be used for plotting the x axis.
#' (This is only used in very particular circumstances, and can generally be ignored.
#' default: value of \code{object$pathways$id_col})
#' @param sig_order Specifies the sig_order of the stat_col column of the pathways data.frame to be used for
#' plotting the x axis. (default: value of \code{object$pathways$sig_order})
#' @param n_col  Specifies the name of the pathways data.frame column to be used for plotting the dot sizes.
#' Generally, this corresponds to gene set sizes, but other fields may be used for plotting.
#  (default: value of \code{object$pathways$n_col})
#' @param subnet_col This is the name of the column to be used to group subnets. (default \code{'subnet'})
#' @param title_col This is the name of the column to be used to identify gene sets. (default \code{'ID'})
#' @param color_col This is the name of the column to be used for the color aesthetic. (default NULL)
#' @param color_sig_order This is the name of the column to be used to indicate the significance order of
#' for the color aesthetic. (default \code{'hiToLo'})
#' @param group_by This is the name of the column to be used to label subnets, indicated on the y-axis.
#' (default \code{'subnet'})
#' @param summarize Logical value indicating whether the function should use the \code{termSummary} feature
#' to generate descriptive names for subnets. This is currently an experimental feature. (default \code{FALSE})
#' @param preappend_subnet_in_summary Logical value indicating whether the function should preappend subnet
#' numbers to the subnet descriptors used for the y-axis. (default \code{TRUE})
#' @param summary_annotation_name A name for the the summarized annotation column, if one is used.
#' (default: \code{'Summary Annotation'})
#' @param x_transform The transform used for the x_axis. This would normally be either \code{'identity'} or
#' \code{'log10'}, but can take other values. (See the \code{transform} argument of **ggplot2's**
#' \code{\link[ggplot2]{scale_x_continuous}()} function.
#' @param axis_font_size Specifies the size of the axis tick fonts in points. If set as NULL, then the
#' function attempts to pick an appropriate size based on the number of characters in the longest y
#' label, and the number of y values. (default 8)
#' @param axis_label_font_size Specifies the size of the axis label fonts in points (default 2 x axis_font_size).
#' @param x_axis_font_size Specifies the size of the x axis tick fonts in points (default axis_font_size).
#' @param y_axis_font_size Specifies the size of the y axis tick fonts in points (default axis_font_size).
#' @param x_axis_label_font_size Specifies the size of the x axis label fonts in points (default axis_label_font_size).
#' @param y_axis_label_font_size Specifies the size of the y axis label fonts in points (default axis_label_font_size).
#' @param colors Specifies a palette of colors to use. If NULL, the function attempts to select a color palette based
#' on whether the values in \code{color_col} are positive (white to red), negative (red to white), or include both
#' positive and negative values (red to white to blue). If a 3 color palette is specified, the \code{midpoint}
#' argument can be used to specify a midpoint value for a color scale. (see \code{midpoint} below.)
#' @param midpoint A value to be used to specify the middle value of a color scale. If the \code{midpoint} argument
#' is specified, and the \code{colors} argument contains exactly 3 colors, then the second one is used for the
#' middle color. This is useful for specifying a zero-value (or mid value) color.
#' @param label_width_chars Numeric value indicating the length of a y-axis gene set label to wrap. If \code{NULL}.
#' no wrapping is performed. (default NULL)
#' @param col_widths This function uses either the **patchwork** package or the **plotly** \code{subplot()} function
#' to create a plot from multiple subplots, such that -Inf or Inf can be represented as broken x-axis values. The
#' \code{col_widths} argument specifies the relative sizes of these subplots.
#' @param interactive A single logical value specifying that either a ggplot object should be returned
#' (FALSE, non-interactive) or alternatively an interactive **plotly** object should be returned (TRUE).
#' (default FALSE).
#' @param width This specifies the width of the plotting area in inches. This is mainly to enable automatic
#' calculation of the font size, and does not determine the size of images generated by this function.
#' (default, value of \code{grDevices::dev.size()[1]})
#' @param height This specifies the height of the plotting area in inches. This is mainly to enable automatic
#' calculation of the font size, and does not determine the size of images generated by this function.
#' (default, value of \code{grDevices::dev.size()[2]})
#'
#' @returns This function returns either a **ggplot2** \code{ggplot} object (default) or a \code{plotly/htmlwidget}
#' if the \code{interactive=TRUE} argument is specified.
#'
#' @export
#'
#' @importFrom ggplot2 aes element_blank geom_point ggplot scale_fill_gradient2 scale_fill_gradientn scale_size_continuous scale_x_continuous scale_y_discrete theme unit ylab
#' @importFrom plotly ggplotly subplot
#' @importFrom rlang sym
#' @importFrom scales as.transform
#'
# @examples
gsnSubnetsDotPlot <- function(
    object,
    pathways.data = NULL,
    distance = NULL,
    id_col = NULL,
    stat_col = NULL,
    sig_order = NULL,
    n_col = NULL,
    subnet_col = 'subnet',
    title_col = "ID",
    color_col = NULL,
    color_sig_order = NULL,
    group_by = "subnet",
    summarize = FALSE, # Can be FALSE, TRUE (uses id_col) or the name of a column.
    preappend_subnet_in_summary = TRUE,
    summary_annotation_name = 'Summary Annotation',
    x_transform = "identity",
    axis_font_size = 8,
    axis_label_font_size = NULL,
    x_axis_font_size = NULL,
    y_axis_font_size = NULL,
    x_axis_label_font_size = NULL,
    y_axis_label_font_size = NULL,
    #colors = c("white", "yellow2", "orange", "red"),
    colors = NULL,
    midpoint = NULL,
    label_width_chars = NULL,
    col_widths = c(neg_inf = 1, fin = 10, pos_inf = 1),
    interactive = FALSE,
    width = NULL,
    height = NULL
){
  stopifnot( "GSNData" %in% class( object ) )
  if( is.null( distance ) ) distance <- object$default_distance
  if( is.null( pathways.data ) ) pathways.data <- object$pathways$data

  if( is.null( distance ) ) stop( 'distance not defined' )
  if( is.null( pathways.data ) ) stop( 'pathways.data needed' )
  if( is.null( object$distances[[distance]]$vertex_subnets ) ) stop( 'No vertex_subnets data found. Did you call \'gsnAssignSubnets()\'?' )

  if( is.null( id_col ) ) id_col <- object$pathways$id_col
  if( is.null( id_col ) ) stop( 'id_col is NULL. Can\'t continue.' )

  if( is.null( title_col ) ) title_col <- id_col

  if( is.null( stat_col ) ) stat_col <- object$pathways$stat_col
  if( is.null( stat_col ) ) warning( 'stat_col is NULL. Cannot order subnets.' )

  if( is.null( sig_order ) ) sig_order <- object$pathways$sig_order
  if( is.null( sig_order ) ) warning( 'sig_order is NULL. Cannot order subnets.' )

  if( is.null( n_col ) ) n_col <- object$pathways$n_col

  # If color_col is the same as stat_col, use sig_order for color_sig_order
  if( is.null(color_sig_order) && ( ! is.null(color_col) ) && color_col == object$pathways$stat_col )
    color_sig_order <- object$pathways$sig_order
  if( is.null( color_sig_order ) ) color_sig_order <- "hiToLo" # For p-values, use "loToHi"

  # Device size
  ppi <- grDevices::dev.size( units = "px" )[1] / grDevices::dev.size( units = "in" )[1]
  if( is.null( height ) ) {
    height <- grDevices::dev.size( units = "in" )[2]
    height.px <- grDevices::dev.size( units = "px" )[2]
  } else {
    height.px <- ppi * height
  }

  if( is.null( width ) ) {
    width <- grDevices::dev.size( units = "in" )[1]
    width.px <- grDevices::dev.size( units = "px" )[1]
  }else {
    width.px <- ppi * width
  }

  # Get subnet data
  .subnets <- gsnMergePathways( object = object,
                                pathways.data = pathways.data,
                                distance = distance,
                                id_col = id_col,
                                stat_col = stat_col,
                                sig_order = sig_order
  )

  # If colors is not defined, look at data to pick a good palette
  if( !is.null( color_col ) ){
    color_col.range <- range( .subnets[[color_col]] )
    if( is.null( colors ) ){
      if( color_col.range[1] < 0 & color_col.range[2] > 0 ) {
        colors = c( "blue", "white", "red" )
        if( is.null( midpoint ) ) midpoint = 0
      } else if( color_col.range[2] > 0 ){
        colors = c("white", "yellow2", "orange", "red")
        if( color_sig_order == "loToHi" ) colors <- rev( colors )
      } else if( color_col.range[1] < 0 ){
        colors = c("red", "orange", "yellow2", "white")
        if( color_sig_order == "hiToLo" ) colors <- rev( colors )
      }
    }
  }

  if( summarize ){
    .summary_x_subnet <- termSummary( terms = .subnets[[title_col]], group = .subnets[[subnet_col]], priority_factor = 1.4 )
    if( preappend_subnet_in_summary ){
      .summary_x_subnet <- structure( paste0( names(.summary_x_subnet), " ", .summary_x_subnet ),
                                      names = names( .summary_x_subnet ) )
      summary_annotation_name <- paste( "Subnet / ", summary_annotation_name )
    }
    .subnets[[summary_annotation_name]] <- .summary_x_subnet[.subnets[[subnet_col]]]
    group_by <- summary_annotation_name
  }

  # We can add code about ordering the subnets in the list.
  .subnets.levels <- rev( unique(as.character(.subnets[[group_by]])) )
  .subnets[[group_by]] <- factor( .subnets[[group_by]], levels = .subnets.levels )

  if( !is.null( n_col ) ) .subnets <- .subnets[order(- .subnets[[n_col]]),]

  .aes <- list( y = rlang::sym( group_by ),
                x = rlang::sym( stat_col ),
                text = rlang::sym( "ID" )
  )
  if( ! is.null( n_col ) ) .aes$size <- rlang::sym(n_col)
  if( ! is.null( color_col ) ) .aes$fill <- rlang::sym(color_col)
  if( ! is.null( title_col ) ) .aes$text <- rlang::sym(title_col)

  # The "x_transform" object can be a character vector corresponding to the name of
  # a transform or an actual transform object. Either way, as.transform will convert
  # it into a transform object, from which we can extract the transform function.
  if( is.null( x_transform ) ) x_transform <- "identity"
  .x_transform <- scales::as.transform( x_transform )
  .x_transform_fun <- .x_transform$transform
  .stat_data.orig <- .subnets[[stat_col]]
  .stat_data.transformed <- .x_transform_fun( .stat_data.orig )

  .plots.l <- list()

  x_scale <- function( ... ){ ggplot2::scale_x_continuous( transform = x_transform ) }

  # We may be able to handle infinite values in interactive plots by using plotly subplots, but
  # currently haven't figured out how to do that gracefully.

  .widths <- numeric()

  # Negative infinity subset
  if( any( .neg_inf.stat <- .stat_data.transformed == -Inf ) ){
    .subnets.neg_inf <- .subnets[.neg_inf.stat,]
    .subnets.neg_inf[[stat_col]] <- as.character( .stat_data.orig[.neg_inf.stat] )
    .subnets.neg_inf[[stat_col]][ .subnets.neg_inf[[stat_col]] == "-Inf" ]  <- "-\u221e"

    .plots.l[['neg_inf']] <- ggplot2::ggplot( data = .subnets.neg_inf,
                                              mapping = do.call( what = ggplot2::aes, args = .aes )) +
      ggplot2::geom_point(shape = 21)
    .widths <- c( .widths, col_widths[['neg_inf']] )
  }

  # Finite subset
  if( any( !is.infinite( .stat_data.transformed ) ) ){
    .subnets.fin <- .subnets[!is.infinite( .stat_data.transformed ),]

    .plots.l[['fin']] <- ggplot2::ggplot( data = .subnets.fin,
                                          mapping = do.call( what = ggplot2::aes, args = .aes )) +
      ggplot2::geom_point(shape = 21) + x_scale()
    .widths <- c( .widths, col_widths[['fin']] )
  }


  # Positive infinity subset
  if( any( .pos_inf.stat <- .stat_data.transformed == Inf ) ){
    .subnets.pos_inf <- .subnets[.pos_inf.stat,]
    .subnets.pos_inf[[stat_col]] <- as.character( .stat_data.orig[.pos_inf.stat] )
    .subnets.pos_inf[[stat_col]][ .subnets.pos_inf[[stat_col]] == "Inf" ]  <- "\u221e"

    .plots.l[['pos_inf']] <- ggplot2::ggplot( data = .subnets.pos_inf,
                                              mapping = do.call( what = ggplot2::aes, args = .aes )) +
      ggplot2::geom_point(shape = 21)
    .widths <- c( .widths, col_widths[['pos_inf']] )
  }

  .widths <- .widths / sum( .widths )

  # y-axis labels
  if( !is.null( label_width_chars ) ){
    if( interactive ){
      .y_scale <- ggplot2::scale_y_discrete( name = group_by,
                                             labels = function( x ){
                                               gsub( pattern = "\\n",
                                                     replacement = "<br>",
                                                     stringr::str_wrap( x, width = label_width_chars ) )
                                             },
                                             drop = FALSE )
    } else {
      .y_scale <- ggplot2::scale_y_discrete( name = group_by,
                                             labels = function( x ){
                                               stringr::str_wrap( x, width = label_width_chars )
                                             },
                                             drop = FALSE )
    }
  } else {
    .y_scale <-  ggplot2::scale_y_discrete( name = group_by, drop = FALSE )
  }
  .plots.l <- lapply(.plots.l, function(x) x + .y_scale)

  # Colors
  if( !is.null(color_col) && !is.null(colors) ){
    .color_col_range <- range(.subnets[[color_col]])
    if( length(colors) == 3 ){
      .color_scale <- ggplot2::scale_fill_gradient2( name = color_col,
                                                     low = colors[1],
                                                     mid = colors[2],
                                                     high = colors[3],
                                                     midpoint = midpoint,
                                                     limits = .color_col_range )
    } else {
      .color_scale <- ggplot2::scale_fill_gradientn( name = color_col, colours = colors, limits = .color_col_range )
    }
    .plots.l <- lapply(.plots.l, function(x) x + .color_scale)
  }

  # Size
  if( !is.null(n_col) ){
    .n_col_range <- range(.subnets[[n_col]])
    .size_scale <- ggplot2::scale_size_continuous( name = n_col, limits = .n_col_range )
    .plots.l <- lapply(.plots.l, function(x) x + .size_scale)
  }

  .plots.l <- .plots.l[!is.null(.plots.l)]

  if( length( .plots.l ) > 1 && 'fin' %in% names( .plots.l ) ){
    for( .name in names( .plots.l ) ){
      if( .name != 'fin' ) .plots.l[[.name]] <- .plots.l[[.name]] + ggplot2::theme( axis.title.x=ggplot2::element_blank() )
    }
  }

  # Y-axis font size
  # # Character Length
  .labels <- .subnets[!duplicated(.subnets$subnet), group_by]
  if( ! is.null( label_width_chars ) ){
    .labels <- stringr::str_wrap( .labels, width = label_width_chars )
  }
  .label_max_width.chars <- max( nchar( unlist( stringr::str_split( string = .labels, pattern = "\n" ) ) ) )
  #.label_lines <- length( unlist( stringr::str_split( string = .labels, pattern = "\n" ) ) )
  .label_lines <- length( .labels ) * max( sapply( stringr::str_split( string = .labels, pattern = "\n" ), length ) )

  if( is.null( axis_font_size ) )
    axis_font_size <- min( height.px / .label_lines, 0.4 * width.px / .label_max_width.chars )
  if( is.null( x_axis_font_size ) )
    x_axis_font_size <- axis_font_size
  if( is.null( y_axis_font_size ) )
    y_axis_font_size <- axis_font_size

  if( is.null( axis_label_font_size ) ){
    axis_label_font_size <- 2 * axis_font_size
  }

  if( is.null( x_axis_label_font_size ) ) x_axis_label_font_size <- x_axis_font_size * 2
  if( is.null( y_axis_label_font_size ) ) y_axis_label_font_size <- y_axis_font_size * 2

  # Interactive
  if( interactive ){
    .yaxes <- structure( lapply(  X = .plots.l, FUN = function(x){list(tickfont = list(size = y_axis_font_size),
                                                                       titlefont = list(size = y_axis_label_font_size),
                                                                       automargin = TRUE)} ),
                         names = paste0("yaxis", sapply( 1:length( .plots.l ), function(x)ifelse(x == 1, "", as.character(x)) ) ) )
    .xaxes <- structure( lapply(  X = .plots.l, FUN = function(x){list(tickfont = list(size = x_axis_font_size),
                                                                       titlefont = list(size = x_axis_label_font_size),
                                                                       automargin = TRUE)} ),
                         names = paste0("xaxis", sapply( 1:length( .plots.l ), function(x)ifelse(x == 1, "", as.character(x)) ) ) )
    .margins <- list( l = 25, r = 25, b = 100, t = 100, pad = 4)
    .layout_args <- c( autosize = TRUE,
                       .yaxes,
                       .xaxes
    )
    # align = 'left'
    # width = width.px,
    # height = height.px,
    # margins = c( l = 25, r = 25, b = 100, t = 100, pad = 4) )

    if( length( .plots.l ) > 1 ){
      .pl <- plotly::subplot(.plots.l,
                             nrows = 1,
                             shareY = TRUE,
                             widths = .widths,
                             margin = 0.01
      )
    } else {
      .pl <- plotly::ggplotly( .plots.l[[1]],
                               width = width.px,
                               heights = height.px )
    }
    .pl <- do.call( what = function(...) plotly::layout(  p = .pl, ... ), .layout_args )
    # This remove problematic formats from the plotly objects
    .pl$x$layout$margin <- list(t = 30, r = 7, b = 30, l = 7)
    .pl$width <- NULL
    .pl$height <- NULL
    .pl$x$layout$width <- NULL
    .pl$x$layout$height <- NULL
    return( .pl )
  }

  # Non-interactive
  p <- NULL
  for( .p in .plots.l ){
    if( ! is.null( .p ) )
      if( is.null( p ) ) {
        p <- .p
      } else {
        # p <- p | ( .p + ggplot2::theme(
        #   axis.text.y=ggplot2::element_blank(),  #remove y axis labels
        #   axis.ticks.y=ggplot2::element_blank()  #remove y axis ticks
        # ) )
        p <- p | .p
      }
  }

  if( sum( ! sapply( .plots.l, is.null ) ) > 1 ){
    p <- p + patchwork::plot_layout( axes = "collect_y",
                                                       axis_titles = "collect_y",
                                                       guides = "collect",
                                                       ncol = length(.plots.l) + 1,
                                                       widths = .widths )
    #) #& ggplot2::theme( plot.margin = ggplot2::unit( x = c( 12,0.5,12,0.5 ), units = "points") )
  }

  p
}



