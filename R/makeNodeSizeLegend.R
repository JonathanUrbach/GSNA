get_usr_x_coords_per_inch <- function() ( par('usr')[2] - par('usr')[1] ) / par('pin')[1]


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
           #,
           #debug = FALSE

  ){
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
      .plt.orig <- par( 'plt' )
      .usr.orig <- par( 'usr' )
      .xpd.orig <- par( 'xpd' )

      par( "plt" = .plt.adj, xpd = TRUE, new = TRUE )

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
        par( "plt" = .plt.orig, xpd = .xpd.orig, new = TRUE )
        plot.window( xlim = .usr.orig[1:2], ylim = .usr.orig[3:4], xaxs = "i", yaxs = "i" )
      }
    }
    invisible(list( .plt.adj = .plt.adj,
                    usr_x_coords_per_inch = usr_x_coords_per_inch,
                    #cex = min(cex.axis, cex.lab, na.rm = TRUE),
                    cex.ticks = cex.ticks,
                    cex.lab = legend.lab.cex
    ))
  } # makeNodeSizeLegend
