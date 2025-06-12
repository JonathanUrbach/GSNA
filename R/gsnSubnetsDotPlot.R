
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
#' @param pathways.data An (optional) pathways data.frame. If not specified, this defaults to the pathways
#' data.frame that was imported prior to subnet assignment.
#' @param merged.pathways.data This is an (optional) data.frame, containing a merged pathways data set,
#' including a subnet or other cluster grouping column. This is the same data typically obtained via the
#' \code{\link{gsnMergePathways}()} function, which is otherwise called if this argument is not specified.
#' (default: NULL)
#' @param distance A specifier for distance metric (optional). If not specified, this defaults to the return
#' value of \code{gsn_default_distance(object)}.
#' @param id_col (optional) Specifies the id column of the pathways data.frame. (default: value of
#' \code{object$pathways$id_col})
#' @param stat_col (optional)  Specifies the id column of the pathways data.frame to be used for plotting
#' the x axis. (This is only used in very particular circumstances, and can generally be ignored.
#' default: value of \code{object$pathways$id_col})
#' @param sig_order (optional) Specifies the sig_order of the stat_col column of the pathways data.frame to
#' be used for plotting the x axis. (default: value of \code{object$pathways$sig_order})
#' @param n_col (optional) Specifies the name of the pathways data.frame column to be used for plotting the
#' dot sizes. Generally, this corresponds to gene set sizes, but other fields may be used for plotting.
#  (default: value of \code{object$pathways$n_col})
#' @param subnet_col (optional)  This is the name of the column to be used to group subnets.
#' (default \code{'subnet'})
#' @param title_col (optional) This is the name of the column to be used to identify gene sets.
#' (default \code{id_col})
#' @param color_col (optional) This is the name of the column to be used for the color aesthetic.
#' (default NULL)
#' @param color_sig_order (optional) This is the name of the column to be used to indicate the significance
#' order of for the color aesthetic. (default \code{'hiToLo'})
#' @param color_transform (optional) The user can provide the name of a function to transform the data
#' such that log or other scales can be used. This takes the same arguments as
#' \code{ggplot2::\link[ggplot2]{scale_fill_continuous}()} (default "identity")
#' @param na.color (optional) This specifies the color used for NA and infinite values. Currently, this
#' value is not included in the legend.
#' @param group_by (optional) This is the name of the column to be used to label subnets, indicated on the
#' y-axis. (default \code{'subnet'})
#' @param summarize (optional) Logical value indicating whether the function should use the \code{termSummary}
#' feature to generate descriptive names for subnets. This is currently an experimental feature.
#' (default \code{FALSE})
#' @param preappend_subnet_in_summary (optional) Logical value indicating whether the function should preappend
#' subnet numbers to the subnet descriptors used for the y-axis. (default \code{TRUE})
#' @param summary_annotation_name (optional)nA name for the the summarized annotation column, if one is used.
#' (default: \code{'Summary Annotation'})
#' @param max_subnets (optional) Specifies the number of top subnets to plot. If \code{NULL}, then all subnets
#' are plotted. (default: NULL)
#' @param subset (optional) Specifies an expression or function to serve as a logical filter giving the user
#' very fine control over which for subnets and gene sets are plotted. When passed as an expression, it is
#' identical to the \code{subset} argument of the \code{base::\link[base]{subset.data.frame}()} function to
#' which it is passed. Note, if a function, it should take no arguments. (default: NULL)
#' @param order_by (optional) This is an expression or function to be used to order subnets in the plot. The
#' expression or function is evaluated within the environment of the data.frame returned by gsnMergePathways(),
#' using \code{group_by} as a grouping variable, such that the user can specify something like
#' \code{order_by = min( p.value )} or \code{order_by = function() min( p.value )}. Note, if a function, it
#' should take no arguments. (default: NULL)
#' @param x_transform (optional) The transform used for the x_axis. This would normally be either \code{'identity'}
#' or \code{'log10'}, but can take other values. (See the \code{transform} argument of **ggplot2's**
#' \code{\link[ggplot2]{scale_x_continuous}()} function.
#' @param axis_font_size (optional)  Specifies the size of the axis tick fonts in points. If set as NULL, then the
#' function attempts to pick an appropriate size based on the number of characters in the longest y
#' label, and the number of y values. (default 8)
#' @param axis_label_font_size (optional) Specifies the size of the axis label fonts in points
#' (default 2 x axis_font_size).
#' @param x_axis_font_size (optional) Specifies the size of the x axis tick fonts in points (default axis_font_size).
#' @param y_axis_font_size (optional) Specifies the size of the y axis tick fonts in points (default axis_font_size).
#' @param x_axis_label_font_size (optional) Specifies the size of the x axis label fonts in points
#' (default axis_label_font_size).
#' @param y_axis_label_font_size (optional) Specifies the size of the y axis label fonts in points
#' (default axis_label_font_size).
#' @param colors (optional) Specifies a palette of colors to use. If NULL, the function attempts to select a color
#' palette based on whether the values in \code{color_col} are positive (white to red), negative (red to white), or
#' include both positive and negative values (red to white to blue). If a 3 color palette is specified, the
#' \code{midpoint} argument can be used to specify a midpoint value for a color scale. (see \code{midpoint} below.)
#' @param midpoint (optional) A value to be used to specify the middle value of a color scale. If the \code{midpoint}
#' argument is specified, and the \code{colors} argument contains exactly 3 colors, then the second one is used for
#' the middle color. This is useful for specifying a zero-value (or mid value) color.
#' @param label_width_chars (optional) Numeric value indicating the length of a y-axis gene set label to wrap. If
#' \code{NULL} no wrapping is performed. (default NULL)
#' @param col_widths (optional) This function uses either the **patchwork** package or the **plotly** \code{subplot()}
#' function to create a plot from multiple subplots, such that -Inf or Inf can be represented as broken x-axis values.
#' The \code{col_widths} argument specifies the relative sizes of these subplots.
#' @param interactive (optional) A single logical value specifying that either a ggplot object should be returned
#' (FALSE, non-interactive) or alternatively an interactive **plotly** object should be returned (TRUE).
#' (default FALSE).
#' @param width (optional) This specifies the width of the plotting area in inches. This is mainly to enable
#' automatic calculation of the font size, and does not determine the size of images generated by this function.
#' (default, value of \code{grDevices::dev.size()[1]})
#' @param height (optional) This specifies the height of the plotting area in inches. This is mainly to enable
#' automatic calculation of the font size, and does not determine the size of images generated by this function.
#' (default, value of \code{grDevices::dev.size()[2]})
#' @param return_subnets (optional) Logical value specifying that instead of a plot object, the function should
#' return the data.frame containing the data used to generate the plot object. (default: FALSE)
#'
#' @details Under the hood, this function calls the \code{gsnMergePathways()} function to generate a subnets
#' report with the \code{id_reassign = FALSE} argument, and plots the results from the data.frame that is
#' returned. The \code{subset} or \code{max_subnets} arguments, are applied to this data.frame, giving the
#' user fine control over which subsets and gene sets are plotted. The subset argument is passed to the
#' \code{base::\link[base]{subset.data.frame}()} function.
#'
#' Plotting is done via **ggplot2**. If there are infinities in x-axis values, for example if
#' \code{x_transform = "log10"} and the specified \code{stat_col} is a *p*-value of 0 which would be a
#' negative infinity on a log10 scale, infinite values are separated out and plotted as a separate
#' **ggplot** object and then combined with the finite values and presented as a broken x-axis. If the
#' \code{interactive = TRUE} argument is specified, the initial \code{ggplot} objects are converted to
#' interactive \code{plotly (htmlwidget)} objects via **plotly**'s ggplotly or subplot functions.
#' If \code{interactive = FALSE} is specified, then the plot is returned as is, or if there are infinities
#' in the x-axis, combined using patchwork and then returned.
#'
#' @returns This function returns a **ggplot2** \code{ggplot} object (default) or a \code{plotly/htmlwidget}
#' if the \code{interactive=TRUE} argument is specified. If \code{return_subnets=TRUE} is specified, a
#' data.frame is returned instead of a plot object, and the \code{interactive} argument is ignored.
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
    object = NULL,
    pathways.data = NULL,
    merged.pathways.data = NULL,
    distance = NULL,
    id_col = NULL,
    stat_col = NULL,
    sig_order = NULL,
    n_col = NULL,
    subnet_col = 'subnet',
    title_col = id_col,
    color_col = NULL,
    color_sig_order = NULL,
    color_transform = "identity",
    na.color = "grey40",
    group_by = "subnet",
    summarize = FALSE, # Can be FALSE, TRUE (uses id_col) or the name of a column.
    preappend_subnet_in_summary = TRUE,
    summary_annotation_name = 'Summary Annotation',
    max_subnets = NULL,
    subset = NULL,
    order_by = NULL,
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
    height = NULL,
    return_subnets = FALSE
){
  if( is.null( merged.pathways.data ) ){
    if( is.null( object ) ){ stop( "Either the object argument or the merged.pathways.data must be defined." ) }
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


    # Get subnet data
    if( ! is.null( merged.pathways.data ) ){
      .subnets <- merged.pathways.data
    } else {
      .subnets <- gsnMergePathways( object = object,
                                    pathways.data = pathways.data,
                                    distance = distance,
                                    id_col = id_col,
                                    stat_col = stat_col,
                                    sig_order = sig_order,
                                    id_reassign = FALSE
      )
    }
  } else {
    if( is.null( id_col ) ) stop( 'id_col is NULL. Can\'t continue.' )
    if( is.null( stat_col ) ) stop( 'stat_col is NULL. Can\'t continue.' )
    if( is.null( title_col ) ) title_col <- id_col
    if( is.null( color_sig_order ) ) color_sig_order <- "hiToLo" # For p-values, use "loToHi"
    .subnets <- merged.pathways.data
  }

  # Limit the results if `max_subnets` specified.
  if( !is.null( max_subnets ) ){
    .subnets <- base::subset.data.frame( .subnets, as.numeric( as.character( .subnets$subnet ) ) <= max_subnets )
  }

  # Check if subset can be evaluated directly. If not an expression, call, name, or NULL, substitute:
  if( ( 'try-error' %in% class( try( subset.class <- class( subset ), silent = TRUE ) ) ) ||
      ! any( c( "expression", "call", "name", "function", "NULL" ) %in% subset.class )){
    subset <- substitute( subset )
  }
  # Use the subset function to have fine control over which subnets and gene signatures are plotted.
  if( !is.null( subset ) ){
    if( any( c( "expression", "call", "name" ) %in% class( subset ) ) ){
      .subnets <- base::subset.data.frame( .subnets, eval(subset) )
    } else if( "function" %in% class( subset ) ){
      .subnets <- base::subset.data.frame( .subnets, eval(str2expression(deparse(subset)))() )
    }  else {stop( "Don't know how to handle subset argument of class ", class( subset ), "." )}
  }



  # The "x_transform" object can be a character vector corresponding to the name of
  # a transform or an actual transform object. Either way, as.transform will convert
  # it into a transform object, from which we can extract the transform function.
  if( is.null( x_transform ) ) x_transform <- "identity"
  .x_transform <- scales::as.transform( x_transform )
  .x_transform_fun <- .x_transform$transform
  .stat_data.orig <- .subnets[[stat_col]]
  .stat_data.transformed <- .x_transform_fun( .stat_data.orig )

  # If a color transforms generates an infinity, it's merely plotted as gray, so we
  # don't really need to change anything.
  if( is.null( color_transform ) ) color_transform <- "identity"
  .color_transform <- scales::as.transform( color_transform )
  .color_transform_fun <- .color_transform$transform
  .color_data.orig <- .subnets[[color_col]]
  .color_data.transformed <- .color_transform_fun( .color_data.orig )

  # If colors is not defined, look at data to pick a good palette
  if( !is.null( color_col ) ){
    # If there are infinite (&infin; or -&infin;) values in stat_col and stat_col and color_col are
    # the same, then the function will crash. The solution is to add an additional column to the
    # .subnets data.frame and use that for the color_col
    if( stat_col == color_col && any( is.infinite(.stat_data.transformed) ) ){
      color_col <- paste0( " ", color_col, " " )
      .subnets[[color_col]] <- .subnets[[stat_col]]
    }

    # Now, if colors is not set, pick appropriate values.
    if( is.numeric( .subnets[[color_col]] ) ){
      #color_col.range <- range( .subnets[[color_col]] )
      color_col.range <- range( .color_data.orig[!is.infinite(.color_data.transformed)], na.rm = TRUE )
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


  # Check if order_by can be evaluated directly. If not an expression, call, name, or NULL, substitute:
  if( ( 'try-error' %in% class( try( order_by.class <- class( order_by ), silent = TRUE ) ) ) ||
      ! any( c( "expression", "call", "name", "function", "NULL" ) %in% order_by.class )){
    order_by <- substitute( order_by )
  }

  # We can add code about ordering the subnets in the list.
  if( !is.null( order_by ) ){
    if( "function" %in% class( order_by ) ){
      .order_stat <- sapply( X = unique( .subnets[[group_by]] ),
                             FUN = function( sn ){
                               .pw.sn <- subset( .subnets, get( group_by ) == sn )
                               with( .pw.sn, unique( eval(str2expression(deparse(order_by)))() ) )
                             } )
    } else if( any(c("expression", "call", "name") %in% class( order_by )) ){
      .order_stat <- sapply( X = unique( .subnets[[group_by]] ),
                             FUN = function( sn ){
                               .pw.sn <- subset( .subnets, get( group_by ) == sn )
                               with( .pw.sn, unique( eval(order_by) ) )
                             } )
    } else {stop( "Don't know how to handle order_by argument of class ", class( order_by ), "." )}

    # Check that order_by function returns only a single value is returned per group_by value.
    if( any( sapply( .order_stat, length ) > 1 ) ){
      warning("The order_by ", class(order_by)," returns more than one unique value per group_by value (",
              group_by ,"). Ordering by mean value per group.")
      .order_stat <- sapply( .order_stat, mean )
    }
    .order_stat <- sort( .order_stat )
    #.subnets[[group_by]] <- factor( .subnets[[group_by]], levels = names(.order_stat) )
    .subnets.levels <- names(.order_stat)
  } else {
    # We can add code about ordering the subnets in the list.
    .subnets.levels <- rev( unique(as.character(.subnets[[group_by]])) )
  }
  #.subnets.levels <- rev( unique(as.character(.subnets[[group_by]])) )
  .subnets[[group_by]] <- factor( .subnets[[group_by]], levels = .subnets.levels )

  # Here we're reordering the subnets, so nothing beyond this point that assumes the original order is valid.
  if( !is.null( n_col ) ) .subnets <- .subnets[order(- .subnets[[n_col]]),]

  .aes <- list( y = rlang::sym( group_by ),
                x = rlang::sym( stat_col ),
                text = rlang::sym( id_col )
  )
  if( ! is.null( n_col ) ) .aes$size <- rlang::sym(n_col)
  if( ! is.null( color_col ) ) .aes$fill <- rlang::sym(color_col)
  if( ! is.null( title_col ) ) .aes$text <- rlang::sym(title_col)

  # We're recalculating .stat_data.transformed and .color_data.transformed
  .stat_data.orig <- .subnets[[stat_col]]
  .stat_data.transformed <- .x_transform_fun( .stat_data.orig )
  .color_data.orig <- .subnets[[color_col]]
  .color_data.transformed <- .color_transform_fun( .color_data.orig )


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


  # Set up the plots:
  .plots.l <- list()
  .widths <- numeric()

  #x_scale <- function( ... ){ ggplot2::scale_x_continuous( transform = x_transform ) }

  # Negative infinity subset
  if( any( .neg_inf.stat <- .stat_data.transformed == -Inf ) ){
    .subnets.neg_inf <- .subnets[.neg_inf.stat,]
    .subnets.neg_inf[[stat_col]] <- as.character( .subnets.neg_inf[[stat_col]] )
    .subnets.neg_inf[[stat_col]][ .subnets.neg_inf[[stat_col]] == -Inf ]  <- "-\u221e"
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
      ggplot2::geom_point(shape = 21) +
      ggplot2::scale_x_continuous( transform = x_transform )
    .widths <- c( .widths, col_widths[['fin']] )
  }


  # Positive infinity subset
  if( any( .pos_inf.stat <- .stat_data.transformed == Inf ) ){
    .subnets.pos_inf <- .subnets[.pos_inf.stat,]
    .subnets.pos_inf[[stat_col]] <- as.character( .subnets.pos_inf[[stat_col]] )
    .subnets.pos_inf[[stat_col]][ .subnets.pos_inf[[stat_col]] == Inf ]  <- "\u221e"

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
    if( class( .subnets[[color_col]] ) %in% c( "factor", "character" ) ){
      .color_scale <- ggplot2::scale_fill_manual(colors,
                                                 transform = color_transform,
                                                 limits = range( .color_data.orig[! is.infinite( .color_data.transformed )],
                                                                 na.rm = TRUE ),
                                                 na.value = na.color
      )
    }

    .color_col_range <- range(.subnets[[color_col]])
    if( length(colors) == 3 ){
      .color_scale <- ggplot2::scale_fill_gradient2( name = color_col,
                                                     low = colors[1],
                                                     mid = colors[2],
                                                     high = colors[3],
                                                     midpoint = midpoint,
                                                     #limits = .color_col_range,
                                                     transform = color_transform,
                                                     limits = range( .color_data.orig[! is.infinite( .color_data.transformed )],
                                                                     na.rm = TRUE ),
                                                     na.value = na.color
      )
    } else {
      .color_scale <- ggplot2::scale_fill_gradientn( name = color_col,
                                                     colours = colors,
                                                     #limits = .color_col_range,
                                                     transform = color_transform,
                                                     limits = range( .color_data.orig[! is.infinite( .color_data.transformed )],
                                                                     na.rm = TRUE ),
                                                     na.value = na.color
      )
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
      if( .name != 'fin' ) .plots.l[[.name]] <- .plots.l[[.name]] + ggplot2::xlab(NULL)
    }
  }

  # Y-axis font size
  # # Character Length
  .labels <- .subnets[!duplicated(.subnets$subnet), group_by]
  if( ! is.null( label_width_chars ) ){
    .labels <- stringr::str_wrap( .labels, width = label_width_chars )
  }
  .label_max_width.chars <- max( nchar( unlist( stringr::str_split( string = .labels, pattern = "\n" ) ) ) )
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

  if( return_subnets ){
    return( .subnets )
  }

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

