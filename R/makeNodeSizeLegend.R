
invisible( utils::globalVariables( c("tick_val", "size.usr") ) )

#' get_usr_x_coords_per_inch
#'
#' @description Internal GSNA package function to return the ratio of user x coordinates per inch. This number is used
#' in the \code{makeNodeSizeLegend()} functions.
#'
#' @return The numerical value corresponding to \code{par('usr')[2] - par('usr')[1] ) / par('pin')[1]}
#'
#' @details
#'
#' examples:
#' \code{
#'    uxcpi <- get_usr_x_coords_per_inch()
#' }
#'
#' @noRd
#' @keywords internal
get_usr_x_coords_per_inch <- function(){
  ( par('usr')[2] - par('usr')[1] ) / par('pin')[1]
}


#' makeNodeSizeLegend
#'
#' @description Internal GSNA package function to generate a vertex size legend for GSNA network plots generated
#' via \code{gsnPlotNetwork()} which uses \code{\link[igraph]{plot.igraph}()} from the igraph package to generate plots.
#'
#' @param numbers A vector containing numerical values to be mapped to a range of vertex sizes. The only really needs
#' to be a minimum and a maximum value to establish a set of scale values.
#'
#' @param sizeEncode.fun The function used by \code{gsnPlotNetwork()} to convert the value in \code{n_col} (usually
#' representing gene set sizes) into vertex sizes within the \code{igraph::plot.igraph()} function. For the current
#' implementation of the \code{igraph} package, vertex sizes are user x coordinates * 200.
#'
#' @param usr_x_coords_per_inch The ratio of horizontal (x) user coordinates per inch at the time when (or immediately
#' after) \code{plot.igraph()} is called, equal to \code{( par('usr')[2] - par('usr')[1] ) / par('pin')[1]}. This ratio
#' is essential for correctly sizing the vertices in the legend. If not specified when the function is called,
#' \code{get_usr_x_coords_per_inch} will be called to obtain this value, but this is only valid if \code{plot.window()}
#' has not been called to create a new user coordinate system.
#'
#' @param log_scale (optional) Logical value indicating whether the size values should be incremented in a linear or
#' logarithmic scale. If not specified, then this will be decided based on the range of minimum to maximum values
#' specified in the \code{numbers} argument.
#'
#' @param cex.ticks (optional) The font size used for tick labels. (default: par('cex'))
#'
#' @param legend.lab (optional) The label for the legend, generally the name of the pathways data field used for scaling
#' the vertex sizes.
#'
#' @param legend.lab.cex (optional) The font size of the legend labels in cex units. (default: \code{cex.ticks * 1.1})
#'
#' @param legend.fg (optional) The foreground color of the legend, used for font, tick and border color. (default: par("fg"))
#'
#' @param legend.bg (optional) The background color of the legend. Doesn't currently do anything.  (default: par("bg"))
#'
#' @param legend.vertex.fg (optional) The foreground color of the legend vertices, used to set the border color of vertices
#' in the legend. This should generally be the same as the value of \code{vertex.frame.color} assigned when generating the
#' igraph network. (default: par("fg"))
#'
#' @param legend.vertex.bg (optional) The background color of the legend vertices, used to set the fill color of vertices
#' in the legend. (default: "#DDDDDD")
#'
#' @param font_face (optional) The font family used for text in the legend. (default: par( "family" ))
#'
#' @param .plt.leg Required plot area where the legend is drawn, specified in the manner of par('plt') as a vector of
#' four values in figure units. This is generally determined before rendering by calling \code{makeNodeSizeLegend()}
#' and the other legend plot functions with a provisional value for .plt.leg that specifies the maximal available
#' region for plotting the legend and the arguments \code{render.bool = FALSE}, \code{optimize.legend.size = TRUE} and
#' \code{h.adjust} specified as \code{"left", "right", or "center"} prior to rendering. The function returns a list of
#' graphical parameters including an optimized \code{.plt.leg} (see value) and the different values for this returned
#' by the various legend plot functions can be reconciled prior to calling this function a second time with
#' \code{render.bool = TRUE} to actually render the legend.
#'
#' @param .fin (optional) The width and height of the current figure in inches. It is advisable to allow this to be
#' automatically determined. (default: par('fin'))
#'
#' @param .pin (optional) The width and height of the current plot region in inches. It is advisable to allow this
#' to be automatically determined. (default: par('pin'))
#'
#' @param .usr (optional) The range of user coordinates corresponding to the plot region. It is advisable to allow
#' this to be automatically determined. (default: par('usr'))
#'
#' @param order_high_to_low (optional) If TRUE, tells the function to order the vertices in the legend with the largest
#' first, and the smallest last. Otherwise, vertices are ordered from lowest to highest. (default FALSE)
#'
#' @param optimize.legend.size (optional) Tells the function to optimize the legend dimensions and graphical
#' parameters. (default: FALSE)
#'
#' @param y.compression.factor (optional) A compression factor allowing lines of the legend to be squeezed together
#' if less than one or stretched apart if greater than one. When less than 1, this can result in vertices
#' overlapping. (default: 1)
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
#' @param bottom_legend_margin (optional) Number of lines added to the bottom to create a bottom legend margin.
#' (default: FALSE)
#'
#' @param restore.params.bool (optional) Logical telling the function to restore any graphical parameters that may have
#' been changed to their values when the function was called. (restore.params.bool: TRUE)
#'
#' @param render.bool (optional) Logical telling the function to actually render the legend as opposed to merely
#' calculating graphical parameters. (default: TRUE)
#'
#' @return The function returns a list with a set of graphic parameters, including the optimized value \code{.plt.leg} if
#' \code{optimize.legend.size = TRUE}.
#'
#' @details
#'
#' examples
#'
#' \code{
#'      uxcpi <- get_usr_x_coords_per_inch()
#'
#'      .plt.leg <- c( legend_left_x.fig,
#'                     legend_left_x.fig + legend_x_size.fig,
#'                     legend_bottom_y.fig,
#'                     legend_top_y.fig )
#'
#'      legend.list <- makeNodeSizeLegend( numbers = c(10, 240),
#'                                         sizeEncode.fun = sizeEncode.fun,
#'                                         .plt.leg = .plt.leg,
#'                                         legend.lab = "Gene Set Size",
#'                                         legend.lab.cex = 1,
#'                                         legend.fg = "black",
#'                                         legend.vertex.fg = "black",
#'                                         legend.vertex.bg = "#CCCCCC",
#'                                         usr_x_coords_per_inch = uxcpi,
#'                                         draw.legend.box.bool = TRUE,
#'                                         h.adjust = "left",
#'                                         render.bool = TRUE,
#'                                         optimize.legend.size = TRUE  )
#' }
#'
#' @noRd
#' @keywords internal
makeNodeSizeLegend <-
  function(numbers,
           sizeEncode.fun,
           usr_x_coords_per_inch = NULL, # NEW, should be calculated immediately after calling plot.igraph.
           log_scale = NULL,
           cex.ticks = par( "cex" ),
           legend.lab = NULL,
           legend.lab.cex = NULL,

           legend.fg = par("fg"),
           legend.bg = par('bg'),

           legend.vertex.fg = par('fg'),
           legend.vertex.bg = "#DDDDDD",

           font_face = par( "family" ),

           .plt.leg,            # edges of legend region as a fraction of figure

           .fin = par( "fin" ), # These are best to leave alone
           .pin = par( "pin" ), # These are best to leave alone
           .usr = par( "usr" ), # These are best to leave alone

           order_high_to_low = FALSE,
           optimize.legend.size = FALSE,

           y.compression.factor = 1,
           v.adjust = "top",
           h.adjust = "center",

           draw.legend.box.bool = FALSE,

           bottom_legend_margin = 0.25, # Number of lines added to the bottom to create a bottom legend margin.

           restore.params.bool = TRUE,
           render.bool = TRUE
  ){
    # Backup par, so that original settings are restored on exit:
    .par.orig <- par( no.readonly = TRUE )
    on.exit( add = TRUE, expr = par(.par.orig) )

    if( is.null( usr_x_coords_per_inch ) ){
      warning( "usr_x_coords_per_inch = NULL. Guessing.\n",
               "For best results, obtain by calling get_usr_x_coords_per_inch() immediately after plot.igraph.\n" )
      usr_x_coords_per_inch <- get_usr_x_coords_per_inch()
    }

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

    # Some conversions: User Coords per inch (x&y)
    x_usr_per_in <- ( .usr[2] - .usr[1] ) / .pin[1]
    y_usr_per_in <- ( .usr[4] - .usr[3] ) / .pin[2]

    # Some conversions: User Coords per figure unit (".fu", x&y)
    x_usr_per_fu <- (.usr[2] - .usr[1]) * .fin[1] / .pin[1]
    y_usr_per_fu <- (.usr[4] - .usr[3]) * .fin[2] / .pin[2]

    figureXsize2figureYsize <- function( x ) x * .fin[1] / .fin[2]

    if( ! order_high_to_low ) tick_values <- rev( tick_values )

    # How many lines needed for legend:
    lines_needed <- length( tick_values ) + length( legend.lab ) # Number of ticks + legend title

    x.dim.avail.fu <- .plt.leg[2] - .plt.leg[1]
    x.dim.avail.in <- .fin[1] * (.plt.leg[2] - .plt.leg[1])

    # This is tick.radius in igraph units
    tick.radius <- sizeEncode.fun( x = tick_values )

    tick.radius.usr <- tick.radius / 200
    tick.radius.fu <- tick.radius / 200 / x_usr_per_fu

    tick.label.width.fu <- getStringWidthsFigure( strings = as.character( tick_values ), cex = cex.ticks, font_face = font_face, method = "auto" )

    margin.char.width.fu <- getStringWidthsFigure( strings = " ", cex = cex.ticks, font_face = font_face, method = "auto" )

    legend.header.width.fu <- if( is.null( legend.lab ) ) 0 else getStringWidthsFigure( strings = legend.lab, cex = legend.lab.cex, font_face = font_face, method = "auto" )

    max_tick.radius.fu <- max( tick.radius.fu, na.rm = TRUE )
    max_tick.radius.usr <- max( tick.radius.usr, na.rm = TRUE )
    max_tick.label.width.fu <- max( tick.label.width.fu, na.rm = TRUE )
    #max_tick_plus_label.width.fu <- 2 * max_tick.radius.fu + max_tick.label.width.fu + 3 * margin.char.width.fu
    max_tick_plus_label.width.fu <- 2 * max_tick.radius.fu + max_tick.label.width.fu + margin.char.width.fu

    # Adding 2 extra margin chars to improve the layout.
    x.dim.needed.fu <- max( c( max_tick_plus_label.width.fu + 4 * margin.char.width.fu,
                               legend.header.width.fu + 4 * margin.char.width.fu ), na.rm = TRUE )
    #x.dim.needed.fu <- max( c( max_tick_plus_label.width.fu + 2 * margin.char.width.fu,
    #                           legend.header.width.fu + 2 * margin.char.width.fu ), na.rm = TRUE )

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
                                      2 * figureXsize2figureYsize( tick.radius.fu ),
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

    # Plot the legend, if render == TRUE
    if( render.bool ){
      # Map the vertex/label positions in the legend.
      x1 <- 0.5 - ( 0.5 * ( max_tick_plus_label.width.fu + 2 * margin.char.width.fu ) / x.dim.avail.fu ) + ( max_tick.radius.fu / x.dim.avail.fu )
      x2 <- x1 + ( max_tick.radius.fu +  margin.char.width.fu ) / x.dim.avail.fu
      x0 <- .plt.adj[1]
      y0 <- .plt.adj[3]

      ticks_needed <- length(tick_values)
      ys <- seq( from = 0.5 + bottom_legend_margin, lines_needed - 0.5, length.out = lines_needed )

      vertices <- data.frame( tick_val = tick_values,
                              size.usr = tick.radius.usr,
                              x1 = rep( x = x1, ticks_needed ),
                              x2 = rep( x = x2, ticks_needed ),
                              y = ys[1:ticks_needed],
                              cex = rep( x = cex.ticks, ticks_needed )
      )

      if( !is.null( legend.lab ) ){
        #legend.header.x2 <-  0.5 - ( 0.5 * ( legend.header.width.fu + 2 * margin.char.width.fu )/ x.dim.actual.fu )
        legend.header.x2 <-  0.5 - ( 0.5 * ( legend.header.width.fu )/ x.dim.avail.fu )
        legend.header.y <- ys[lines_needed]
        vertices <-
          rbind( vertices,
                 data.frame( tick_val = legend.lab,
                             size.usr = NA,
                             x1 = x1,
                             x2 = legend.header.x2,
                             y = legend.header.y,
                             cex = legend.lab.cex
                 )
          )
      }

      # Before changing parameters for rendering legend, first back up original graphical parameters
      #.plt.orig <- par( 'plt' )
      .usr.orig <- par( 'usr' )
      #.xpd.orig <- par( 'xpd' )

      par( plt = .plt.adj, xpd = TRUE, new = TRUE ) # This will be restored by an on.exit call.

      plot.window( xlim = c(0,1), ylim = c( 0, lines_needed ), xaxs = "i", yaxs = "i" )

      if( draw.legend.box.bool ){
        graphics::box( which = "plot", col = legend.fg, bg = legend.bg )
      }

      # Note: for inches = a number, the largest dimension is scaled to the size indicated. In the case of
      # circles, the largest dimension is the diameter, but the size argument given is 'radii', so the
      # inches argument needs to be \code{inches = 2 * max_tick.radius.usr / usr_x_coords_per_inch}.
      with( subset( vertices, !is.na( size.usr) ),
            graphics::symbols( x = x1,
                               y = y,
                               circles = size.usr,
                               bg = legend.vertex.bg,
                               fg = legend.vertex.fg,
                               inches = max_tick.radius.usr / usr_x_coords_per_inch,
                               add = TRUE  )
      )

      with( subset( x = vertices, !is.na( tick_val ) ),
            graphics::text( x = x2,
                            y = y,
                            labels = as.character( tick_val ),
                            adj = 0,
                            pos = 4,
                            family = font_face,
                            cex = cex.ticks,
                            col = legend.fg )
      )

      if( restore.params.bool ){
        #par( "plt" = .plt.orig, xpd = .xpd.orig, new = TRUE )
        plot.window( xlim = .usr.orig[1:2], ylim = .usr.orig[3:4], xaxs = "i", yaxs = "i" )
      }
    }
    invisible(list( .plt.adj = .plt.adj,
                    usr_x_coords_per_inch = usr_x_coords_per_inch,
                    cex.ticks = cex.ticks,
                    cex.lab = legend.lab.cex
    ))
  } # makeNodeSizeLegend
