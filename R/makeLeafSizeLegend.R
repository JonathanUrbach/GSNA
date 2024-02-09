#' makeLeafSizeLegend
#'
#' @description Internal GSNA package function to generate a leaf size legend for GSNA network plots generated
#' via \code{gsnHierarchicalDendrogram()}.
#'
#' @param numbers A vector containing numerical values to be mapped to a range of leaf sizes. The only really needs
#' to be a minimum and a maximum value to establish a set of scale values.
#'
#' @param sizeEncode.fun The function used by \code{gsnPlotNetwork()} to convert the value in \code{n_col} (usually
#' representing gene set sizes) into leaf sizes in cex units.
#'
#' @param pch (optional) A pch symbol used for representing leaves in the legend, generally the same as is used in the
#' dendrogram itself. (default: 16 (a closed circle))
#'
#' @param cin.pch (optional) A numeric vector of length 2 representing the width and height in inches of the pch used
#' in the dendrogram when \code{cex = 1}. This is used for calculating the height and width of lines of the legend,
#' since not all pch characters are the same size and they tend to be smaller than the corresponding character sizes
#' in inches. The default should be a good approximation for most pch characters. (default: c(0.125,0.125))
#'
#' @param .plt.leg Required plot area where the legend is drawn, specified in the manner of graphics::par('plt') as a vector of
#' four values in figure units. This is generally determined before rendering by calling \code{makeNodeSizeLegend()}
#' and the other legend plot functions with a provisional value for .plt.leg that specifies the maximal available
#' region for plotting the legend and the arguments \code{render.bool = FALSE}, \code{optimize.legend.size = TRUE} and
#' \code{h.adjust} specified as \code{"left", "right", or "center"} prior to rendering. The function returns a list of
#' graphical parameters including an optimized \code{.plt.leg} (see value) and the different values for this returned
#' by the various legend plot functions can be reconciled prior to calling this function a second time with
#' \code{render.bool = TRUE} to actually render the legend.
#'
#' @param log_scale (optional) Logical value indicating whether the size values should be incremented in a linear or
#' logarithmic scale. If not specified, then this will be decided based on the range of minimum to maximum values
#' specified in the \code{numbers} argument.
#'
#' @param cex.ticks (optional) The font size used for tick labels. (default: graphics::par('cex'))
#'
#' @param leaf.col (optional) The color of the leaf symbols used in the legend, or for open symbols,
#' \code{pch \eqn{\in} c(21, 22, 23, 24, 25)}, the background fill color. (default: "#999999")
#'
#' @param leaf_border_color (optional) For open symbols, \code{pch \eqn{\in} c(21, 22, 23, 24, 25)}, the color of the symbol
#' border. (default: "#666666")
#'
#' @param legend.lab (optional) A title for the legend. (default: NULL)
#'
#' @param legend.lab.cex (optional) The font size of the legend labels in cex units. (default: \code{cex.ticks * 1.1})
#'
#' @param legend.fg (optional) Legend foreground, used for text and border color. (default: graphics::par('fg'))
#'
#' @param legend.bg (optional) Legend background. Doesn't currently do anything. (default: graphics::par('bg'))
#'
#' @param font_face (optional) The font family used for text in the legend. (default: graphics::par( "family" ))
#'
#' @param .fin (optional) (optional) The width and height of the current figure in inches. It is advisable to allow
#' this to be automatically determined. (default: graphics::par('fin'))
#'
#' @param order_high_to_low (optional) If TRUE, tells the function to order the vertices in the legend with the largest
#' first, and the smallest last. Otherwise, vertices are ordered from lowest to highest. (default FALSE)
#'
#' @param optimize.legend.size (optional) Tells the function to optimize the legend dimensions and graphical parameters.
#' (default: FALSE)
#'
#' @param y.compression.factor (optional) A compression factor allowing lines of the legend to be squeezed together
#' if less than one or stretched apart if greater than one. When less than 1, this can result in vertices overlapping.
#' (default: 1)
#'
#' @param v.adjust (optional) This argument, which is passed to the internal function \code{adjust_plt()} controls
#' how the legend's vertical position and height are optimized. The values \code{"top"}, \code{"bottom"}, and
#' \code{"center"} indicate that the legend-plotting region will be flush with the top, or bottom of the printing
#' area, or otherwise centered within the available legend plotting. If this argument is set to \code{NULL} no
#' vertical adjustment is performed. (default: "top")
#'
#' @param h.adjust (optional) This argument, which is passed to the function \code{adjust_plt()} controls
#' how the legend's horizontal position and width are optimized. The values \code{"left"}, \code{"right"}, and
#' \code{"center"} indicate that the legend-plotting region will be flush with the left, or right of the printing
#' area, or otherwise centered within the available legend plotting. If this argument is set to \code{NULL} no
#' horizontal adjustment is performed. (default: "left")
#'
#' @param draw.legend.box.bool (optional) This argument is a logical, telling the function to draw a box around the
#' legend. (default: FALSE)
#'
#' @param bottom_legend_margin  (optional) Number of lines added to the bottom to create a bottom legend margin.
#' (default: FALSE)
#'
#' @param restore.params.bool (optional) Logical telling the function to restore any graphical parameters that may have
#' been changed to their values when the function was called. (restore.params.bool: TRUE)
#' @param render.bool (optional) Logical telling the function to actually render the legend as opposed to merely
#' calculating graphical parameters. (default: TRUE)
#'
#' @return  The function returns a list with a set of graphic parameters, including the optimized value \code{.plt.leg} if
#' \code{optimize.legend.size = TRUE}.
#'
#' @details
#'
#' examples:
#' \code{
#'      legend.dat <- makeLeafSizeLegend( numbers = gs_numbers,
#'               sizeEncode.fun = sizeEncode.fun,
#'               .plt.leg = c( legend_left_x.fig,
#'                             legend_left_x.fig + legend_x_size.fig,
#'                             0,
#'                             legend_top_y.fig
#'               ),
#'               legend.lab = n_col,
#'               pch = leaves_pch,
#'               cex = cex,
#'               leaf_border_color = leaf_border_color,
#'               leaf.col = legend.leaf.col,
#'               legend.fg = legend.fg,
#'               legend.bg = legend.bg,
#'               h.adjust = 'left',
#'               v.adjust = 'top',
#'               render.bool = FALSE,
#'               optimize.legend.size = TRUE )
#' }
#'
#' @importFrom grDevices axisTicks
#'
#' @importFrom graphics box par plot.window
#'
#' @noRd
#' @keywords internal
makeLeafSizeLegend <-
  function(numbers,
           sizeEncode.fun,
           pch = 16,
           cin.pch = c(0.125,0.125),      # cin (w & h) for the set pch symbol for cex = 1. Default should work for most pch values.

           .plt.leg,

           log_scale = NULL,

           cex.ticks = graphics::par( "cex" ),
           leaf.col = "#999999",          # Symbol col
           leaf_border_color = "#666666", # Symbol border color for symbols that support it c(21:25).

           legend.lab = NULL,
           legend.lab.cex = NULL,

           legend.fg = graphics::par("fg"),
           legend.bg = graphics::par('bg'),

           font_face = graphics::par( "family" ),

           .fin = graphics::par( "fin" ), # These are best to leave alone

           order_high_to_low = FALSE,
           optimize.legend.size = FALSE,

           y.compression.factor = 1,
           v.adjust = "top",
           h.adjust = "left",

           draw.legend.box.bool = FALSE,

           bottom_legend_margin = 0.25, # Number of lines added to the bottom to create a bottom legend margin.

           restore.params.bool = TRUE,
           render.bool = TRUE
           #,
           #debug = FALSE

  ){
    # Backup par, so that original settings are restored on exit:
    .par.orig <- par( no.readonly = TRUE )
    on.exit( add = TRUE, expr = par(.par.orig) )

    # We're going to use the coordinate system set up by igraph::plot.igraph because the size of vertices is dependent on the coordinate sytem.
    # Input data used to determine tick_values
    numbers.range <- range( numbers, na.rm = TRUE )
    if( is.null( legend.lab.cex ) ) legend.lab.cex <- cex.ticks * 1.1

    if( length( legend.lab ) > 1 ){
      legend.lab <- legend.lab[1]
      warning( "lengend.lab only supports up to a single value. Additional data ignored." )
    }

    if( is.null( log_scale ) ) log_scale = sizeEncode.fun( return_log_scale_bool = TRUE )

    # Calculate tic locations for tick_values
    if( log_scale ){
      tick_values <- axisTicks( usr = log10( numbers.range ), log = TRUE )
    } else {
      tick_values <- axisTicks( usr = numbers.range, log = FALSE )
    }

    # Order ticks appropriately
    if( ! order_high_to_low ) tick_values <- rev( tick_values )

    # How many lines needed for legend:
    lines_needed <- length( tick_values ) + length( legend.lab ) # Number of ticks + legend title

    x.dim.avail.fu <- .plt.leg[2] - .plt.leg[1]
    x.dim.avail.in <- .fin[1] * (.plt.leg[2] - .plt.leg[1])

    # This is tick.width in cex units
    tick.cex <- sizeEncode.fun( x = tick_values )

    tick.width.in <- tick.cex * cin.pch[1]
    tick.width.fu <- tick.width.in / .fin[1]

    tick.height.in <- tick.cex * cin.pch[2]
    tick.height.fu <- tick.height.in / .fin[2]

    tick.label.width.fu <- getStringWidthsFigure( strings = as.character( tick_values ), cex = cex.ticks, font_face = font_face, method = "auto" )

    margin.char.width.fu <- getStringWidthsFigure( strings = " ", cex = cex.ticks, font_face = font_face, method = "auto" )

    legend.header.width.fu <- if( is.null( legend.lab ) ) 0 else getStringWidthsFigure( strings = legend.lab, cex = legend.lab.cex, font_face = font_face, method = "auto" )

    max_tick.width.fu <- max( tick.width.fu, na.rm = TRUE )

    max_tick.label.width.fu <- max( tick.label.width.fu, na.rm = TRUE )

    max_tick_plus_label.width.fu <- max_tick.width.fu + margin.char.width.fu + max_tick.label.width.fu

    # Adding 2 extra margin chars to improve the layout.
    x.dim.needed.fu <- max( c( max_tick_plus_label.width.fu + 4 * margin.char.width.fu,
                               legend.header.width.fu + 4 * margin.char.width.fu ), na.rm = TRUE )

    if( x.dim.needed.fu > x.dim.avail.fu ) warning( "Node size legend may have insufficient width to plot." )

    x.dim.actual.fu <- min( x.dim.avail.fu, x.dim.needed.fu, na.rm = TRUE )

    y.dim.avail.fu <- .plt.leg[4] - .plt.leg[3]

    tick.label.height.fu <- getStringHeightsFigure( strings = as.character( tick_values ),
                                                    cex = cex.ticks,
                                                    font_face = font_face,
                                                    method = "auto" )

    legend.header.height.fu <- ifelse( is.null( legend.lab ),
                                       0,
                                       getStringHeightsFigure( strings = legend.lab,
                                                               cex = legend.lab.cex,
                                                               font_face = font_face,
                                                               method = "auto" ) )

    margin.char.height.fu <- getStringHeightsFigure( strings = " ",
                                                     cex = cex.ticks,
                                                     font_face = font_face,
                                                     method = "auto" )

    y.line.height.fu <- max( c( pmax( tick.label.height.fu,
                                      tick.height.fu,
                                      na.rm = TRUE ), legend.header.height.fu), na.rm = TRUE  )

    y.dim.needed.fu <- y.line.height.fu * ( lines_needed + bottom_legend_margin ) * y.compression.factor

    if( y.dim.avail.fu < y.dim.needed.fu ) warning( "Node size legend may have insufficient height to plot." )

    y.dim.actual.fu <- min( y.dim.avail.fu, y.dim.needed.fu, na.rm = TRUE )

    if( optimize.legend.size ){
      .plt.adj <- adjust_plt( .plt = .plt.leg,
                              y.dim.actual.fu = y.dim.actual.fu,
                              x.dim.actual.fu = x.dim.actual.fu,
                              v.adjust = v.adjust,
                              h.adjust = h.adjust,
                              v.strict = TRUE,
                              h.strict = TRUE )
    } else {
      .plt.adj <- .plt.leg
    }


    # Plot the legend, if render.bool == TRUE
    if( render.bool ){
      x1 <- 0.5 - ( 0.5 * max_tick_plus_label.width.fu  / x.dim.avail.fu )
      x2 <- x1 + ( 0.5 * max_tick.width.fu + margin.char.width.fu ) / x.dim.avail.fu

      ticks_needed <- length(tick_values)
      ys <- seq( from = 0.5 + bottom_legend_margin, lines_needed - 0.5, length.out = lines_needed )

      vertices <- data.frame( tick_val = tick_values,
                              tick.cex = tick.cex,
                              x1 = rep( x = x1, ticks_needed ),
                              x2 = rep( x = x2, ticks_needed ),
                              y = ys[1:ticks_needed],
                              cex = rep( x = cex.ticks, ticks_needed )
      )

      if( !is.null( legend.lab ) ){
        legend.header.x2 <- 0.5 - ( 0.5 * legend.header.width.fu / x.dim.avail.fu )
        legend.header.y <- ys[lines_needed]
        vertices <-
          rbind( vertices,
                 data.frame( tick_val = legend.lab,
                             tick.cex = NA,
                             x1 = x1,
                             x2 = legend.header.x2,
                             y = legend.header.y,
                             cex = legend.lab.cex
                 )
          )
      }

      # First back up original graphical parameters # Probably no longer necessary due to on.exit call.
      #.plt.orig <- graphics::par( 'plt' )
      .usr.orig <- graphics::par( 'usr' )
      #.xpd.orig <- graphics::par( 'xpd' )

      graphics::par( "plt" = .plt.adj, xpd = TRUE, new = TRUE ) # This will be restored by on.exit call.

      graphics::plot.window( xlim = c(0,1), ylim = c( 0, lines_needed ), xaxs = "i", yaxs = "i" )

      if( draw.legend.box.bool ){
        graphics::box( which = "plot", fg = legend.fg, bg = legend.bg )
      }

      with( subset( vertices, !is.na( tick.cex ) ),
            if( pch %in% c( 21, 22, 23, 24, 25 )){
              with( vertices, plot.xy( xy = xy.coords( x1, y, "", "" ),
                                       type = NULL,
                                       pch = pch,
                                       cex = tick.cex,
                                       xlab = "",
                                       ylab = "",
                                       col = leaf_border_color,
                                       bg = leaf.col ))
            } else {
              with( vertices, plot.xy( xy.coords( x1, y, "", "" ),
                                       type = NULL,
                                       pch = pch,
                                       cex = tick.cex,
                                       xlab = "",
                                       ylab = "",
                                       col = leaf.col ))
            }
      )

      with( subset( x = vertices, !is.na( tick_val ) ),
            text( x = x2,
                  y = y,
                  labels = as.character( tick_val ),
                  adj = 0,
                  pos = 4,
                  family = font_face,
                  cex = cex.ticks,
                  col = legend.fg )
      )

      if( restore.params.bool ){
        #graphics::par( "plt" = .plt.orig, xpd = .xpd.orig, new = TRUE )
        graphics::plot.window( xlim = .usr.orig[1:2], ylim = .usr.orig[3:4], xaxs = "i", yaxs = "i" )
      }
    }
    invisible(list( .plt.adj = .plt.adj,
                    cex = min(cex.ticks, legend.lab.cex, na.rm = TRUE),
                    cex.ticks = cex.ticks,
                    cex.lab = legend.lab.cex ))
  } # makeLeafSizeLegend
