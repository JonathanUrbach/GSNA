#' renderCircularDendrogram
#'
#' @description Internal plot function for circular dendrogram, called by gsnHierarchicalDendrogram()
#'
#' @param dendro An object of class dendrogram.
#' @param leaf.names A vector of the names of leaves.
#' @param subnets.lf A vector containing subnets keyed by the names of associated gene sets.
#' @param label_names A vector containing label names for each leaf.
#' @param labels_colors A vector containing the color associated with each label. This is used for
#' coloring labels by cluster.
#' @param labels.cex The cex font size for leaf labels.
#' @param cex General cex font size.
#' @param .plt.plot plt value for the plot itself, describing the plotting region, in the same format
#' as par(.plt').
#' @param clear Boolean indicating whether circlize::circos.clear() should be called, resetting circular
#' plot parameters.
#' @param family The font family used.
#' @param width Plot width in inches.
#' @param height Plot height in inches.
#' @param .mai.plot Size of plot margins, in same format as par('mai').
#' @param .mai.circos Circular coordinate margins of plot, corresponding to circle.margin in 'circlize'
#' package. (see 'circlize' documentation).
#' @param max_frxn_label_width maximal label width as a fraction of radius.
#' @param frxn_dendro_height circularized dendrogram height as a fraction of radius.
#' @param label_margin_chars number of characters with which to pad the label margin.
#' @param canvas.xlim x limit for the canvas (cartesian coordinates).
#' @param canvas.ylim y limit for the canvas (cartesian coordinates).
#' @param xlim_ylim_ratio Ratio of width / height.
#'
#' @importFrom graphics strwidth
#'
#' @noRd
#' @keywords internal
renderCircularDendrogram <- function( dendro,
                                      leaf.names, # Names of leaves in proper order. May not be the same as label_names, which is obtained from dendro.
                                      subnets.lf = NULL,
                                      label_names = NULL,
                                      labels_colors = NULL,
                                      labels.cex = NULL,

                                      cex = par('cex'),
                                      .plt.plot = NULL,

                                      clear = TRUE,
                                      family = par('family'),

                                      width = par('fin')[1],
                                      height = par('fin')[2],

                                      #.mai.plot = c(0.82, 0.42, 0.42, 2.42),
                                      .mai.plot = par('mai'),
                                      .mai.circos = c( 0, 0, 0, 0 ), # Region within .mai.plot/.plt.plot

                                      max_frxn_label_width = 0.58, # Maximal label width as a fraction of radius.
                                      frxn_dendro_height = 0.3,
                                      label_margin_chars = 2,
                                      canvas.xlim = NULL,
                                      canvas.ylim = NULL,
                                      xlim_ylim_ratio = NULL
                                     ){
  # Backup par, so that original settings are restored on exit:
  .par.orig <- par( no.readonly = TRUE )
  on.exit( add = TRUE, expr = par(.par.orig) )

  if( is.null(label_names) ) label_names <- labels(dendro) # Labels on the dendrogram. May not be the same as "leaf.names"
  label_cols <- dendextend::labels_colors(dendro)
  if( is.null( labels.cex ) )
    labels.cex <- sapply( X = dendextend::get_leaves_nodePar( dendro ), FUN = function(x) ifelse( !is.null( x$lab.cex ), x$lab.cex, NULL ))
  .n <- length( label_names )


  # Plot Geometry
  if( is.null( xlim_ylim_ratio ) ) xlim_ylim_ratio <- width / height

  if( is.null( canvas.xlim ) || is.null( canvas.ylim ) ){

    if( is.null( .plt.plot ) ){
      .plt.plot <- c( .mai.plot[2]/width,
                      ( width - .mai.plot[4] )/width,
                      .mai.plot[1]/height,
                      ( height - .mai.plot[3] )/height )

      width.avail <- width - .mai.plot[2] - .mai.plot[4]
      height.avail <- height - .mai.plot[3] - .mai.plot[1]

      # Adjust to left and top
      if( width.avail > height.avail ){
        .plt.plot[2] <- .plt.plot[1] + height.avail / width
      } else {
        .plt.plot[3] <- .plt.plot[4] - width.avail / height
      }
    }

    canvas.xlim <- c( -1, 1 )
    canvas.ylim <- c( -1, 1 )
  }

  # Brackets coordinates. This is the same code as is in gsnHierarchicalDendrogram. x and y coords are swapped when plotting with circlize.
  if( !is.null( subnets.lf ) )
    brackets.coords <- get_brackets_coords( subnets.lf = subnets.lf, leaf.names = leaf.names, labels_colors = label_cols )
  available_radius <- min( ( width - .mai.plot[2] - .mai.plot[4] ) / xlim_ylim_ratio, height - .mai.plot[1] - .mai.plot[3] ) / 2

  # Plot
  {
    par( plt = .plt.plot ) # This has been backed up already and will be restored by on.exit call.
    circlize::circos.par(cell.padding = c(0, 0, 0, 0),
                         circle.margin = .mai.circos + 0.0001, # circle.margin can only be positive.
                         canvas.xlim = canvas.xlim,
                         canvas.ylim = canvas.ylim,
                         points.overflow.warning = FALSE)
    circlize::circos.initialize("a", xlim = c(0, .n)) # only one sector

    dend_height = attr(dendro, "height")

    size_labels_width.in <- max( graphics::strwidth( s = label_names , units = "inches", cex = labels.cex, family = family ), na.rm = TRUE )
    space_width.in <- max( graphics::strwidth( s = " " , units = "inches", cex = labels.cex, family = family ), na.rm = TRUE )
    space_width.radii <- space_width.in / available_radius

    label_scale_factor <- 1
    #max_frxn_label_width <- 0.58 # Maximal label width as a fraction of radius.
    if( size_labels_width.in / available_radius > max_frxn_label_width ) {
      label_scale_factor <- max_frxn_label_width * available_radius / ( size_labels_width.in + label_margin_chars * space_width.in )
      warning( "Font cex is too large. Scaling label text x ", label_scale_factor, "." )
    }

    # Track heights. For 3 tracks, the max total track height is 0.92
    track_height_0 <- if( is.null( subnets.lf ) ) 0 else 0.10
    track_height_1 <- max_frxn_label_width
    track_height_2 <- 0.92 - track_height_1 - track_height_0

    x_adj <- - 0.5  # We're adjusting the x coordinate so that it is aligns with the labels.

    if( !is.null( subnets.lf ) ){
      circlize::circos.track(ylim = c(0, 1),
                             bg.border = NA,
                             track.height = track_height_0,
                             panel.fun = function(x, y) {
                               for(i in seq_len(nrow(brackets.coords))) {
                                 circlize::circos.text(x = brackets.coords[i,'y3'] + x_adj,
                                                       y = brackets.coords[i,'x3'],
                                                       labels = brackets.coords[i,'cluster'],
                                                       adj = c(0, 0.5),
                                                       facing = "clockwise",
                                                       niceFacing = TRUE,
                                                       col = brackets.coords[i,'bracket_color'],
                                                       cex = cex )
                                 circlize::circos.segments( x0 = brackets.coords[i,'y1'] + 0.05 + x_adj,
                                                            y0 = 0.20,
                                                            x1 = brackets.coords[i,'y2'] - 0.05 + x_adj,
                                                            y1 = 0.20,
                                                            col = brackets.coords[i,'bracket_color'],
                                                            lwd = 2 )
                                 circlize::circos.segments( x0 = brackets.coords[i,'y1'] + 0.05 + x_adj,
                                                            y0 = 0.00,
                                                            x1 = brackets.coords[i,'y1'] + 0.05 + x_adj,
                                                            y1 = 0.20,
                                                            col = brackets.coords[i,'bracket_color'],
                                                            lwd = 2 )
                                 circlize::circos.segments( x0 = brackets.coords[i,'y2'] - 0.05 + x_adj,
                                                            y0 = 0.00,
                                                            x1 = brackets.coords[i,'y2'] - 0.05 + x_adj,
                                                            y1 = 0.20,
                                                            col = brackets.coords[i,'bracket_color'],
                                                            lwd = 2 )
                               }
                             })
    }
    circlize::circos.track(ylim = c(0, dend_height),
                           bg.border = NA,
                           track.height = track_height_1,
                           panel.fun = function(x, y) {
                             for(i in seq_len(.n)) {
                               circlize::circos.text(x = i - 0.5,
                                                     y = 0 + space_width.radii,
                                                     labels = label_names[i],
                                                     adj = c(0, 0.5),
                                                     facing = "clockwise",
                                                     niceFacing = TRUE,
                                                     col = label_cols[i],
                                                     cex = label_scale_factor * ifelse( length( labels.cex ) == 1, labels.cex, labels.cex[i] ) )
                             }
                           })

    circlize::circos.track(ylim = c(0, dend_height * 1.05),
                           bg.border = NA,
                           track.height = track_height_2,
                           panel.fun = function(x, y) {
                             circlize::circos.dendrogram(dendro)
                           })

    if( clear ) circlize::circos.clear()
  }

  attr( x = dendro, which = ".plt.plot" ) <- .plt.plot
  invisible( dendro )
}
