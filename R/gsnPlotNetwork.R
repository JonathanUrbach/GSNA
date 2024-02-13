
#' gsnPlotNetwork
#'
#' @description Function for plotting the networks within GSNData objects.
#'
#' @param object A GSNData object containing a pared distance matrix with \code{edges}. NOTE: when calling as
#' \code{plot.GSNData}, use the argument \code{x} instead.
#' @param pathways_dat (optional) data.frame containing associated pathways data. This defaults to whatever pathways
#' data has already been imported into this GSNData object in \code{object$pathways$data}.
#' @param distance (optional) The name of a distance metric used, defaults to whatever \code{default_distance} is.
#' @param id_col (optional) This is the name of the column in the pathways data.frame that corresponds to the names of
#' gene sets. The default value is specified by \code{object$pathways$id_col}. (See details.)
#' @param substitute_id_col (optional) This is the name of the column that is to be substituted for the \code{id_col}
#' column when labeling network vertices. (See details.)
#' @param stat_col (optional) This is the name of the column in the pathways data.frame that contains a significance
#' value for coloring network vertices. The default value is specified by \code{object$pathways$stat_col}.
#'
#' @param stat_col_2 (optional) This is the name of an optional second column in the pathways data.frame that
#' contains a significance value for coloring network vertices in a 2-color network. The default value is specified
#' by \code{object$pathways$stat_col_2}. When specified, a 2-color network is generated. To force a 2-color network
#' to plot as a standard 1-color network using \code{stat_col} alone, use \code{stat_col_2 = NA}.
#'
#' @param sig_order (optional) This indicates the behavior of \code{stat_col}, whether low values (\code{'loToHi'}) or
#' high values (\code{'hiToLo'}) are most significant. The default value is specified in \code{object$pathways$sig_order}.
#'
#' @param sig_order_2 (optional) This indicates the behavior of \code{stat_col_2}, whether low values (\code{'loToHi'}) or
#' high values (\code{'hiToLo'}) are most significant. The default value is specified in \code{object$pathways$sig_order_2}.
#'
#' @param n_col (optional) This is the name of the column in the pathways data.frame that contains a value for gene set
#' size, or any other value intended to be the bases of leaf scaling. When specified, leaf sizes will be scaled by this
#' value, either as a function argument, or in the \code{object$pathways$n_col} field. An \code{NA} value can be used
#' to override the the value in \code{object$pathways$n_col} and suppress leaf scaling when \code{n_col} has been
#' set in the object. (default is the value in \code{object$pathways$n_col}).
#'
#' @param optimal_extreme (optional) This indicates the behavior of the statistic used to generate the distance metric,
#' specifically whether low values (\code{'min'}) or high values \code{'max'} are to be regarded as close. This is used
#' for scaling the width and the color of the edges connecting vertices. See \code{scale.edges.by.distance}, below:
#' (default: object$distances[[distance]]$pared_optimal_extreme or if that's NULL,
#' object$distances[[distance]]$optimal_extreme)
#'
#' @param transform_function (optional) This is a function to transform the values in \code{stat_col} so that they
#' are suitable for amenable to color-scaling. For *p*-values, a log transformation is often useful, but can produce
#' negative infinities if the transformation is applied to zero. By default the function is the \code{nzLog10}
#' (non-zero log10) function, provided by this package, which adds a small pseudocount to p-values when log10 transforming
#' values equal to zero. If values in \code{stat_col} are less than zero, then log10 transformation is inappropriate and
#' will introduce NAs, and therefore some other method should be used. (default: \code{nzLog10})
#'
#' @param pathways_title_col (optional) Indicates a column to be used as the 'Title' column for network vertices.
#' If unset, the function attempts to search for a title column from the following values: c("Title", "Name", "NAME",
#' "STANDARD_NAME" ) (See details.)
#'
#' @param edge_colors (optional) A vector of colors included to generate a scale represent the numerical value of the
#' edge distances. By default, the colors are arranged as a rainbow with black and purple representing the greatest
#' distances, and orange and red the nearest distances. This feature (and argument) will likely be deprecated in
#' future versions. (default: edge_colors = c("black", "purple", "blue", "green","yellow4", "orange","red"))
#'
#' @param vertex_colors (optional) This is the standard set of colors used for a standard single color network.
#' By default, c("white","yellow","red") is used, coloring low values white, high values red, and intermediate values
#' yellow if \code{sig_order} is "loToHi" and vice versa if sig_order is "hiToLo".
#'
#' @param vertex_colors.1 (optional) This is the range of colors used for a 2-color network corresponding to
#' values of \code{stat_col}. Up to 2 colors can be used, and should correspond to a color contrasting with
#' \code{vertex_colors.2}. The default is c("white","red"), coloring high values red and low values white if
#' \code{sig_order} is \code{"loToHi"} and vice versa if \code{sig_order} is \code{"hiToLo"}.
#'
#' @param vertex_colors.2 (optional) This is the range of colors used for a 2-color network corresponding to
#' values of \code{stat_col_2}. Up to 2 colors can be used, and should correspond to a color contrasting with
#' \code{vertex_colors.2}. The default is \code{c("white","blue")}, coloring high values blue and low values white
#' if \code{sig_order_2} is \code{"loToHi"} and vice versa if \code{sig_order} is \code{"hiToLo"}.
#'
#' @param combine_method (optional) For dual channel plots this is a string used to indicate how colors are combined to
#' generate a two dimensional color scale. Options are "scaled_geomean" (same as "default"), "standard" (same as "euclidean" ),
#' "negative_euclidean", "mean", and "additive". See details.
#'
#' @param na.color (optional) This color is assigned to vertices for which there is an \code{NA} value. (default: "#CCCCCC")
#'
#' @param filename (optional) An output file name for the plot. If 'out_format' is not set (see below), the output
#' file type will be determined by the file suffix, which can be \code{'.svg'}, \code{'.pdf'}, or \code{'.png'}. If
#' the \code{out_format} cannot be determined from the file name, than it may be manually set with \code{out_format}.
#' If the output file type cannot be determined from the \code{filename} or \code{out_format} arguments, an error will
#' be thrown.
#'
#' @param out_format (optional) Output filetype when \code{filename} is specified, either \code{'svg'}, \code{'png'},
#' \code{'pdf'}, or \code{'plot'} (default if filename is not specified). For more information, see Details.
#'
#' @param width (optional) Sets the width of the output canvas in inches. Defaults to the width of the present
#' graphical device.
#'
#' @param height (optional) Sets the height of the output canvas in inches. Defaults to the height of the present
#' graphical device.
#'
#' @param vertex.shape (optional) Shape of the vertex, passed to \code{igraph::plot.igraph}. By default, the value is
#' \code{'circle'}.
#'
#' @param vertex.size (optional) Size of vertices, passed to \code{igraph::plot.igraph}. By default, the value is
#' NULL, and the function attempts to pick a reasonable value based on the canvas size and the number of gene sets.
#'
#' @param vertex.size.range (optional) The range of vertex sizes used in plots, from low to high. This is used when
#' \code{n_col} is specified and vertex sizes are intended to be scaled. If this is not specified, then the function
#' attempts to select appropriate values based on size of the figure being generated.
#'
#' @param vertex.label.cex (optional) Size of vertex labels, passed to \code{igraph::plot.igraph}. As with vertex.size,
#' by default, the value is NULL, and the function attempts to pick a reasonable value based on the canvas size and
#' the number of gene sets.
#'
#' @param vertex.label.col (optional) Color of vertex labels, passed to \code{igraph::plot.igraph}. If not specified,
#' the function attempts to pick a contrasting color for vertex label text using the \code{contrasting_color.fun}
#' argument. (default: NULL)
#'
#' @param vertex.frame.color (optional) Color of the vertex border. (default par('fg'))
#'
#' @param contrasting_color.fun (optional) A function to pick a color for vertex labels that contrasts with the vertex
#' fill color. If unspecified, the function attempts to pick a suitable function for generating suitable set of contrasting
#' colors, based on the \code{contrasting_color()} function. (default: For single channel plots using color scales defined
#' with \code{vertex_colors}, or dual channel color scales defined with \code{vertex_colors.1}, or \code{vertex_colors.2}
#' using yellow or orange, \code{contrasting_color(type="binary")} is used, and otherwise
#' \code{contrasting_color(type="blackyellow")} is used.)
#'
#' @param scale_labels_by_vertex (optional) Logical that tells the function to scale the text in vertex labels by the size
#' of the vertex. (default: TRUE)
#'
#' @param max_edge_width (optional) Size of vertex labels, passed to \code{igraph::plot.igraph}. By default, the value
#' is NULL, and the function attempts to pick a reasonable value based on the canvas size and the number of gene sets.
#'
#' @param scale.edges.by.distance (optional) A logical telling the function to scale edges between vertices on the basis
#' of distance. NOTE: If \code{optimal_extreme == "max"}, then smaller numbers are treated as more distant, and conversely
#' if \code{optimal_extreme == "min"}, larger numbers are treated as more distant. (default: FALSE)
#'
#' @param color.edges.by.distance (optional)  A logical telling the function to color edges between vertices on the basis
#' of distance. This functionality will likely be deprecated. (default: FALSE)
#'
#' @param edge_arrow_size (optional) Size of vertex labels, passed to \code{igraph::plot.igraph}. By default, the value
#' is NULL, and the function attempts to pick a reasonable value based on the canvas size and the number of gene sets.
#'
#' @param seed (optional) This is a seed that the function uses to generate a plot layout. By default it is 29189892,
#' and this results in a repeatable behavior for plots. However, to randomize the plot layout behavior, this value may
#' be set to NULL, or if some other repeatable layout is desired, another seed may be used.
#'
#' @param layout (optional) Either a function that generates a layout or a numerical matrix containing a vertex layout
#' with two columns corresponding to *x* and *y* coordinates. This argument is passed to the \code{igraph} plot method
#' that is subsequently called by \code{gsnPlotNetwork()} (see \code{.plot}, below). The default \code{layout} is
#' the anonymous function \code{function(x){igraph::layout_with_fr(x, grid = "nogrid" )}}, which calls
#' \code{igraph::layout_with_fr()} (implementing Fruchterman-Reingold layout) with the \code{grid="nogrid"} option,
#' enabling proper layout of networks with >= 1000 gene set vertices. Other useful layouts for \code{igraph} networks
#' include \code{igraph::layout_with_fr} (default Fruchterman-Reingold), \code{igraph::layout_with_dh} (implementing
#' Davidson-Harel layout), \code{igraph::layout_as_tree}, \code{igraph::layout_nicely}, and others.
#' For more details about layouts, see \code{\link[igraph]{igraph.plotting}}.
#'
#' @param .plot (optional) A plot function used to render the internally generated \code{igraph} object. By default
#' \code{igraph::plot.igraph} is used, but for interactive plotting, \code{igraph::tkplot} may be used. For more
#' details about plotting, see \code{\link[igraph]{igraph.plotting}}.
#'
#' @param show.legend (optional) A logical value telling the function whether or not to show legends. Legends for vertex
#' size and node color are currently supported. (default: TRUE)
#'
#' @param legend.lab.cex (optional) The font size of legend label text as cex units. If not specified, the function will
#' attempt to pick an appropriate value based on the figure layout.
#'
#' @param legend.axis.cex (optional) The font size of legend axis text as cex units. If not specified, the function will
#' attempt to pick an appropriate value based on the figure layout.
#'
#' @param legend.fg (optional) The foreground color of the legend that controls the color of text, axes, axis labels,
#' ticks, and legend border. (default: par('fg'))
#'
#' @param legend.bg (optional) The background color of the legend. This argument doesn't currently work, and may be
#' removed in the future. (default: "#CCCCCC" )
#'
#' @param legend.vertex.fg (optional) The border color of vertices for vertex size legends. This argument allows the
#' legend vertex frame color to be set separately from vertex.frame.color. (default: vertex.frame.color)
#'
#' @param legend.vertex.bg (optional) The fill color of vertices for vertex size legends. (default: "#DDDDDD")
#'
#' @param font_face (optional) The font face used for the figure. (default: par("family"))
#'
#' @param main (optional) The plot title. (default: NULL)
#'
#' @param cex.main (optional) The font size in cex units of the main title. (default: 1.5 * par( 'cex' ))
#'
#' @param mar.main (optional) The number of lines set aside for a main title when main is used. (default: 3.2)
#'
#' @param lines.main (optional) The distance of the main title in lines from the top of the plot. (default: 0.9)
#'
#' @param .mar.plot (optional) The margins of the plot itself. If unspecified, the function will attempt to reserve
#' enough room to the right of the plot for the legend or legends.
#'
#' @param draw.legend.box.bool (option) Logical indicating whether bounding boxes should be drawn for the legends.
#'
#' @param legend.free.cex.bool (optional) Logical allowing independent optimized sizing of legend label font sizes if TRUE.
#' (default: FALSE)
#'
#' @param legend_x_size.in (optional) The width of the legend in inches. If not set, the function attempts to choose an
#' appropriate value. (default: \code{min(2,max(width*2/5,width-height))})
#'
#' @param colors.n (optional) The number of colors in for each channel in 1 or 2 channel plots. For single channel plots
#' the number of colors is simply equal to this number. For dual channel plots the total number of colors in the legend
#' is equal to the square of this number. (default: 100)
#'
#' @param new (optional) Logical telling the function (if true) that a new plot should be added to an existing device (if
#' TRUE) or that the current device should be cleared and written over (if FALSE). (default: FALSE)
#'
#' @param legend_spacing.x.in (optional) Space between plot and legend in inches. This can be used to adjust the horizontal
#' position and move the legend closer to or farther away from the plot region. Since the network plot may not fill the
#' entire plotting region, it may be useful to use negative values to move the legends closer to the plot.
#' (default: 2 character widths)
#'
#' @param legend_spacing.y.in (optional) Space between legends in inches. (default: 1 character height)
#'
#' @param resolution Image resolution in pixels per inch, only for bitmap image output formats (currently
#' png only). (default: 72)
#'
#' @param DO_BROWSER (option) Logical indicating whether browser() should be run for this function. (For debugging
#' purposes, will probably remove.)
#'
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
#'
#' \donttest{
#' # This example can take >10 seconds to run on some platforms,
#' # so we won't test it here.
#'
#' library(GSNA)
#'
#' # In this example, we generate a gene set network from CERNO example
#' # data. We begin by subsetting the CERNO data for significant results:
#' sig_pathways.cerno <- subset( Bai_CiHep_DN.cerno, adj.P.Val <= 0.05 )
#'
#' # Now create a gene set collection containing just the gene sets
#' # with significant CERNO results, by subsetting Bai_gsc.tmod using
#' # the gene set IDs as keys:
#' sig_pathways.tmod <- Bai_gsc.tmod[sig_pathways.cerno$ID]
#'
#' # And obtain a background gene set from differential expression data:
#' background_genes <- toupper( rownames( Bai_CiHep_v_Fib2.de ) )
#'
#' # Build a gene set network:
#' sig_pathways.GSN <-
#'    buildGeneSetNetworkJaccard(geneSetCollection = sig_pathways.tmod,
#'                               ref.background = background_genes )
#'
#' # Now import the CERNO data:
#' sig_pathways.GSN <- gsnImportCERNO( sig_pathways.GSN,
#'                                     pathways_data = sig_pathways.cerno )
#'
#' # Now we can pare the network and assign subnets:
#' sig_pathways.GSN <- gsnPareNetGenericHierarchic( object = sig_pathways.GSN )
#' sig_pathways.GSN <- gsnAssignSubnets( sig_pathways.GSN )
#'
#' # Now, produce a network plot:
#' gsnPlotNetwork( object  = sig_pathways.GSN )
#' }
#'
#' @seealso
#'  \code{\link{plot.GSNData}}
#'  \code{\link{gsnToIgraph}}
#'  \code{\link[igraph]{plot.igraph}}
#'
#' @importFrom grDevices dev.size svg pdf png
#'
gsnPlotNetwork <- function( object,
                            pathways_dat = NULL,
                            distance = NULL,
                            id_col = NULL,
                            substitute_id_col = NULL,
                            stat_col = NULL,
                            stat_col_2 = NULL,  # NA suppresses 2-color plotting behavior
                            sig_order = NULL,
                            sig_order_2 = NULL,
                            n_col = NULL,
                            optimal_extreme = NULL,
                            transform_function = nzLog10,
                            pathways_title_col = c("Title", "Name", "NAME", "STANDARD_NAME" ),
                            edge_colors = c("black", "purple", "blue", "green","yellow4", "orange","red"),
                            vertex_colors = c("white","yellow","red"),
                            vertex_colors.1 = c("white", "red" ),
                            vertex_colors.2 = c("white", "blue" ),
                            combine_method = "scaled_geomean", # Same as "default"
                            na.color = "#CCCCCC",
                            filename = NULL,
                            out_format = NULL,
                            width = NULL,
                            height = NULL,
                            vertex.shape = "circle",
                            vertex.size = NULL,
                            vertex.size.range = NULL, #c( 0.8, 10 ),
                            vertex.label.cex = NULL,
                            vertex.label.col = NULL,
                            vertex.frame.color = par('fg'),
                            contrasting_color.fun = NULL,
                            scale_labels_by_vertex = TRUE,
                            max_edge_width = NULL,
                            scale.edges.by.distance = FALSE,
                            color.edges.by.distance = FALSE,
                            edge_arrow_size = NULL,
                            seed = 29189892,
                            layout = function(x){igraph::layout_with_fr(x, grid = "nogrid" )},
                            .plot = igraph::plot.igraph,
                            show.legend = TRUE,
                            legend.lab.cex = NULL,
                            legend.axis.cex = NULL,
                            legend.fg = par('fg'),
                            legend.bg = "#DDDDDD",
                            legend.vertex.fg = NULL,
                            legend.vertex.bg = "#DDDDDD",
                            font_face = par("family"),
                            main = NULL,
                            cex.main = NULL,
                            mar.main = 3.2,   # NEW reserve this many lines for main
                            lines.main = 0.9, # Position Main this many lines from plot
                            .mar.plot = NULL,
                            draw.legend.box.bool = FALSE,
                            legend.free.cex.bool = FALSE,
                            legend_x_size.in = NULL, #
                            colors.n = 100,
                            new = FALSE,
                            legend_spacing.x.in = 2 * par('cin')[1],
                            legend_spacing.y.in = par('cin')[2],
                            resolution = 72, # pixels per inch
                            DO_BROWSER = FALSE

){
  if(DO_BROWSER) browser()
  stopifnot( "GSNData" %in% class( object ) )

  # Backup par, so that original settings are restored on exit:
  .par.orig <- par( no.readonly = TRUE )
  on.exit( add = TRUE, expr = par(.par.orig) )

  # If sig_col or stat_col_2 are specified, check that sig_order / sig_order_2 is specified.
  if( ( !is.null( stat_col ) && is.null( sig_order ) ) )
    stop("If stat_col is specified, so must sig_order be. ('loToHi' or 'hiToLo')")
  if( !is.null( stat_col_2 ) )
      if( !is.na( stat_col_2 ) && is.null( sig_order_2 ) )
    stop("If stat_col_2 is specified (other than NA), so must sig_order_2 be. ('loToHi' or 'hiToLo')")

  # Set defaults from object
  if( is.null(distance) ) distance <- object$default_distance
  if( is.null(pathways_dat) ) pathways_dat <- object$pathways$data
  if( !is.null(pathways_dat) ){
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

    if( is.null(n_col) ){
      n_col <- object$pathways$n_col
    } else if( is.na( n_col ) ){
      n_col <- NULL
    }
    #
    rownames(pathways_dat) <- pathways_dat[[id_col]]

    # Scan for pathways_title_column.
    if( ! is.null(pathways_title_col) && ! all(is.na(pathways_title_col)) ){
      #pathways_title_col <- pathways_title_col[pathways_title_col %in% colnames( pathways_dat )][[1]]
      pathways_title_col <- pathways_title_col[pathways_title_col %in% colnames( pathways_dat )]
      if(length(pathways_title_col) == 0)
        stop("Cannot find pathways Title column.\nSet correct pathways column with pathways_title_col='NAME'",
             " or opt out with pathways_title_col=NULL.\n")
      pathways_title_col <- pathways_title_col[[1]]
    }
  }

  if( is.null( optimal_extreme ) ) optimal_extreme <- object$distances[[distance]]$pared_optimal_extreme
  if( is.null( optimal_extreme ) ) optimal_extreme <- object$distances[[distance]]$optimal_extreme
  if( is.null( optimal_extreme ) ) stop( "optimal_extreme is not defined" )

  if( is.null( legend.vertex.fg ) ) legend.vertex.fg <- vertex.frame.color
  if( is.null( cex.main ) ) cex.main <- 1.5 * par( 'cex' )

  # Determine plot layout.
  if( is.null( width ) ) width <- par('fin')[1]   # grDevices::dev.size("in")[1]
  if( is.null( height ) ) height <- par('fin')[2] # grDevices::dev.size("in")[2]

  # Space between plot and legend(s)
  legend_spacing.x.fig <- legend_spacing.x.in / width
  # Space between legends
  legend_spacing.y.fig <- legend_spacing.y.in / height

  # For small plots less than width 5 in, have the legend size scale downward with the
  if( show.legend & is.null( legend_x_size.in ) )
    legend_x_size.in <- min( 2, max( width * 2/5, width - height ) )

  if( is.null( .mar.plot ) ){
    .mar.plot <- c( 2.1,
                    2.1,
                    2.1 + ifelse( is.null(main), mar.main, 0 ),
                    if( show.legend ) legend_x_size.in / par('cin')[2] else 2.1 )
  }

  .mai.plot <- par('cin')[2]*.mar.plot

  # For network plot we need a square plotting area, which is constrained by the lesser of the
  # height.plot.in or width.plot.in:
  height.plot.avail.in <- height - .mai.plot[1] - .mai.plot[3]
  width.plot.avail.in <- width - .mai.plot[2] - .mai.plot[4]

  height.plot.in <- min( height.plot.avail.in, width.plot.avail.in )
  width.plot.in <- height.plot.in

  if( width.plot.in < 3 )  warning( "Figure width may be too small to plot." )

  .mai.plot <- c( height - .mai.plot[3] - height.plot.in,
                  .mai.plot[2],
                  .mai.plot[3],
                  width - .mai.plot[2] - width.plot.in )

  .plt.plot <- c( .mai.plot[2] / width,
                  ( .mai.plot[2] + width.plot.in ) / width,
                  ( height - .mai.plot[3] - height.plot.in ) / height,
                  ( height - .mai.plot[3] ) / height )

  if( height.plot.in > width.plot.in ) height.plot.in <- width.plot.in


  sigNet <- gsnToIgraph( object, distance )
  vertex.names <- igraph::V(sigNet)$name
  vertex_count <- length(igraph::V( sigNet ) )

  # Weirdly, vertex sizes seem to be callibrated relative to canvas size
  if( is.null( vertex.size ) ) vertex.size <- round( 100 / sqrt( vertex_count ), digits = 1 )
  if( is.null( vertex.size.range ) ) vertex.size.range <- round( c(50, 150) / sqrt( vertex_count ), digits = 1 )

  # Vertex labels and edge width are callibrated on an absolute scale.
  if( is.null( vertex.label.cex ) ){
    vertex.label.cex <- round( 0.27 * min( width.plot.in, height.plot.in ) / sqrt( vertex_count ), digits = 3 )
    # If we're using a range of vertex sizes without label scaling, optimize vertex.label.cex so that it fits in a the smallest vertex.
    if( !is.null( n_col ) && ! scale_labels_by_vertex ) vertex.label.cex <- vertex.label.cex * min( vertex.size.range ) / vertex.size
  }
  if( is.null( max_edge_width ) ) max_edge_width <- 10 *  min( width.plot.in, height.plot.in ) / sqrt( vertex_count )

  ####
  # Node characteristics in this implementation are dependent on pathways_dat, whereas edge characterisics are
  # dependent on the distance matrix.

  twoColorEncode.fun <- NULL
  oneColorEncode.fun <- NULL
  sizeEncode.fun <- NULL

  if( ! is.null( pathways_dat ) ){
    if( !is.null( stat_col_2 ) ){
      numbers.1 <- c(loToHi=-1, hiToLo = 1)[[as.character(sig_order)]] * transform_function(pathways_dat[[stat_col]])
      if( is.null( numbers.1 ) ) stop( "Column stat_col: '", stat_col, " contains no data." )
      numbers.2 <- c(loToHi=-1, hiToLo = 1)[[as.character(sig_order_2)]] * transform_function(pathways_dat[[stat_col_2]])
      if( is.null( numbers.2 ) ) stop( "Column stat_col_2: '", stat_col_2, " contains no data." )

      twoColorEncode.fun <- makeTwoColorEncodeFunction( numbers.1 = numbers.1,
                                                        numbers.2 = numbers.2,
                                                        colors.1 = vertex_colors.1,
                                                        colors.2 = vertex_colors.2,
                                                        combine_method = combine_method,
                                                        na.color = na.color
      )

      pathways_dat$vertex.color <- twoColorEncode.fun( numbers.1 = numbers.1, numbers.2 = numbers.2, output_as = "rgb" )
      legend.xlab <- stat_col_2
      legend.ylab <- stat_col
    } else {
      numbers <- c(loToHi=-1, hiToLo = 1)[[as.character(sig_order)]] * transform_function(pathways_dat[[stat_col]] )
      if( is.null( numbers ) ) stop( "Column stat_col: '", stat_col, " contains no data." )
      oneColorEncode.fun <- makeOneColorEncodeFunction( numbers = numbers, colors = vertex_colors, na.color = na.color )
      pathways_dat$vertex.color <- oneColorEncode.fun( numbers = numbers, output_as = "rgb" )
      legend.ylab <- stat_col
    }
    # If n_col is set, use for the vertex size scale.
    if( !is.null( n_col ) ){
      gs_numbers <- pathways_dat[[n_col]]
      if( is.null( gs_numbers ) ) stop( "Column n_col: '", n_col, " contains no data." )
      sizeEncode.fun <- make_lv_sizing_function( numbers = gs_numbers, size_range = vertex.size.range )
      pathways_dat$vertex.size <- sizeEncode.fun( x = gs_numbers )
      legend.lab.n_col <- n_col
    }
  }

  igraph::vertex_attr(sigNet, "color") <- pathways_dat[igraph::V(sigNet)$name,"vertex.color"]
  if( !is.null(vertex.frame.color) ) igraph::vertex_attr(sigNet, "frame.color") <- vertex.frame.color
  if( is.null( vertex.label.col ) ){
    # If the node color sets to be used contain yellow, do use binary black/white contrasting label colors.
    # If not, use blackyellow.
    if( is.null(contrasting_color.fun) ){
      # Default contrasting_colors are "blackyellow"
      contrasting_color.fun <- function( x ) contrasting_color( col = x, type = "blackyellow" )
      if( is.null( stat_col_2 ) ){
        if( any( c("yellow", "#FFFF00", "orange", "#FEA400" ) %in% vertex_colors ) ){
          contrasting_color.fun <- function( x ) contrasting_color( col = x, type = "binary" )
        }
      } else if( any( c("yellow", "#FFFF00", "orange", "#FEA400" ) %in% c( vertex_colors.1, vertex_colors.2 ) )  ){
        contrasting_color.fun <- function( x ) contrasting_color( col = x, type = "binary" )
      }
    }
    # Use contrasting colors:
    igraph::vertex_attr(sigNet, "label.color") <- contrasting_color.fun( igraph::vertex_attr(sigNet, "color") )
  } else {
    igraph::vertex_attr(sigNet, "label.color") <- vertex.label.col
  }

  # Get id from the vertex name. It may require reformatting:
  id <- igraph::V(sigNet)$name
  if( ! is.null( substitute_id_col ) )
    id <- pathways_dat[igraph::V(sigNet)$name, substitute_id_col ]

  # Now, get the Title column if it's not null or na
  if( !is.na( pathways_title_col ) && !is.null( pathways_title_col ) ){
    id <- igraph::V(sigNet)$name
    if( ! is.null( substitute_id_col ) )
      id <- pathways_dat[ igraph::V(sigNet)$name, substitute_id_col ]

    # igraph::V(sigNet)$label <- paste0( gsub( x = id, pattern = '(.{1,15})([\\s\\~\\:])', replacement = '\\1\n' ),
    #                                    "\n",
    #                                    gsub( x = pathways_dat[igraph::V(sigNet)$name, pathways_title_col ],
    #                                          pattern = '(.{1,15})([\\s\\~\\:])',
    #                                          replacement = '\\1\n' ) )# Adds converts '\s' to '\n' after up to 1

    igraph::V(sigNet)$label <- paste0( break_long_lines( x = id ),
                                       "\n",
                                       break_long_lines( x = pathways_dat[igraph::V(sigNet)$name, pathways_title_col ] ) )
                                       # Adds converts '\s' to '\n' after up to 1
  } else {
    igraph::V(sigNet)$label <- break_long_lines( x = id )
  }

  if( !is.null( pathways_dat[['vertex.size']] ) ){
    igraph::V(sigNet)$size <- pathways_dat[vertex.names, "vertex.size"]
  } else {
    igraph::V(sigNet)$size <- vertex.size
  }

  if( scale_labels_by_vertex ){
    igraph::V(sigNet)$label.cex <- vertex.label.cex * igraph::V(sigNet)$size / vertex.size
  } else {
    igraph::V(sigNet)$label.cex <- vertex.label.cex
  }

  igraph::V(sigNet)$shape <- vertex.shape

  if( scale.edges.by.distance || color.edges.by.distance ){
    # Edge Attributes:
    paredDistRange <- structure(  range( object$distances[[distance]]$pared, na.rm = TRUE ), names = c("min","max" ) )

    if( optimal_extreme == "min" ){
      paredDistRange <- structure(rev( paredDistRange ), names = c("min","max" ) )
    }
    # These edge width and edge color functions are the old way of doing edge sizing and coloring. We should probably
    # replace using a newer method.
    scale_fun <- function(x){ (x - paredDistRange[['min']]) / ( paredDistRange[['max']] - paredDistRange[['min']] ) }
    convert_fun <- function(x){ scale_fun(object$distances[[distance]]$pared[x[1], x[2]] ) }

    if( scale.edges.by.distance )
      igraph::E(sigNet)$width <-
      1+as.integer( apply( X = read.table( text = attr( igraph::E(sigNet),"vnames" ), sep = "|"),
                           MARGIN = 1,
                           FUN = convert_fun ) * max_edge_width )

    if( color.edges.by.distance ){
      warning( "The 'color.edges.by.distance' functionality will deprecated and removed in future versions." )
      igraph::E(sigNet)$color <-
      myColorF( numbers = 1+as.integer(apply(X = read.table(text = attr( igraph::E(sigNet),"vnames" ), sep = "|"),
                                             MARGIN = 1,
                                             FUN = convert_fun ) * (length( edge_colors ) - 1 ) ),
                n = length( edge_colors ),
                colors = edge_colors )
    }
  }

  # The arrow.size argument doesn't seem to work properly for igraph, so we're making it configurable, but not
  # used by default.
  if( !is.null( edge_arrow_size ) ) igraph::E(sigNet)$arrow.size <- edge_arrow_size

  if( is.null(out_format) ){
    if( ! is.null( filename ) ){
      if( stringr::str_detect( string =  filename, pattern = stringr::regex( "\\.svg$", ignore_case = TRUE) ) ){
        out_format <- "svg"
      } else if( stringr::str_detect( string =  filename, pattern = stringr::regex( "\\.pdf$", ignore_case = TRUE) ) ){
        out_format <- "pdf"
      } else if( stringr::str_detect( string =  filename, pattern = stringr::regex( "\\.png$", ignore_case = TRUE) ) ){
        out_format <- "png"
      } else {
        stop( "Need to specify out_format." )
      }
    } else {
      out_format <- 'plot'
    }
  } else if ( out_format %in% c("svg", "pdf", "png") & is.null(filename) ){
    stop( "filename argument needed for svg, pdf, or png" )
  }

  # Default is 'plot' to current device.
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
    #resolution <- 72 # pixels per inch
    #out_fun <- function( height, width, ... ) grDevices::png( units = "in", res = resolution, ... ) # This doesn't work right.
    out_fun <- function( height, width, ... ) grDevices::png( height = height * resolution, width = width * resolution, res = resolution, ... )
    close_fun <- grDevices::dev.off
  }

  # Open new output device if appropriate.
  out_fun( file = filename, width = width, height = height  )

  # Set plot parameters
  par( mai = .mai.plot, new = new ) # This will be restored by an on.exit call

  if( !is.null( seed ) ){
    withr::with_seed( seed = seed, code = .plot(sigNet, layout = layout, xlim = c(-1,1), ylim = c(-1,1 ), new = new ) )
  } else {
    .plot(sigNet, layout = layout, xlim = c(-1,1), ylim = c(-1,1 ), new = new)
  }
  uxcpi <- get_usr_x_coords_per_inch()


  # Set up layout for legends.
  if( show.legend ){
    plt.l <- list()
    legend.lab.cex.v <- numeric()
    legend.axis.cex.v <- numeric()
    plt.idx <- 0

    if( is.null( legend_spacing.y.fig ) ) legend_spacing.y.fig <- par('cin')[2] / height
    if( is.null( legend_spacing.x.fig ) ) legend_spacing.x.fig <- -10 * par('cin')[1] / width

    legend_left_x.fig <- .plt.plot[2] + legend_spacing.x.fig # Align right edge
    legend_top_y.fig <- .plt.plot[4]  # & top edge of plot
    max_legend_x_size.fig <- legend_x_size.in / width

    # Go through process of generating legends without rendering to determine geometries.
    # Align top legend with top and right edge of plot .plt.plot[4] and .plt.plot[2]
    if( !is.null( twoColorEncode.fun ) ){
      legend.dat <- make2ColorLegend(numbers.1 = numbers.1,
                                     numbers.2 = numbers.2,
                                     twoColorEncode.fun = twoColorEncode.fun,
                                     n = colors.n,
                                     lab.1 = legend.ylab,
                                     lab.2 = legend.xlab,
                                     .plt.leg = c( legend_left_x.fig,
                                                   legend_left_x.fig + max_legend_x_size.fig,
                                                   0,
                                                   legend_top_y.fig
                                     ),
                                     cex.lab = legend.lab.cex,
                                     cex.axis = legend.lab.cex,
                                     draw.legend.box.bool = draw.legend.box.bool,
                                     h.adjust = "left",
                                     render.bool = FALSE,
                                     legend.fg = legend.fg,
                                     legend.bg = legend.bg,
                                     optimize.legend.size = TRUE
      )
      plt.idx <- plt.idx + 1
      plt.l[[plt.idx]] <- legend.dat$.plt.adj
      legend.lab.cex.v[[plt.idx]] <- legend.dat$cex.lab
      legend.axis.cex.v[[plt.idx]] <- legend.dat$cex.axis
      legend_top_y.fig <- legend.dat$.plt.adj[3] - legend_spacing.y.fig
    } else if (! is.null( oneColorEncode.fun ) ){
      legend.dat <- make1ColorLegend( numbers = numbers,
                                      oneColorEncode.fun = oneColorEncode.fun,
                                      n = colors.n,
                                      lab = legend.ylab,
                                      .plt.leg = c( legend_left_x.fig,
                                                    legend_left_x.fig + max_legend_x_size.fig,
                                                    0,
                                                    legend_top_y.fig
                                      ),
                                      cex.lab = legend.lab.cex,
                                      draw.legend.box.bool = draw.legend.box.bool,
                                      h.adjust = "left",
                                      #v.adjust = "center",
                                      render.bool = FALSE,
                                      legend.fg = legend.fg,
                                      legend.bg = legend.bg,
                                      optimize.legend.size = TRUE
      )

      plt.idx <- plt.idx + 1
      plt.l[[plt.idx]] <- legend.dat$.plt.adj
      legend.lab.cex.v[[plt.idx]] <- legend.dat$cex.lab
      legend.axis.cex.v[[plt.idx]] <- legend.dat$cex.axis
      legend_top_y.fig <- legend.dat$.plt.adj[3] - legend_spacing.y.fig
    }
    if( !is.null(sizeEncode.fun) ){
      legend.dat <- makeNodeSizeLegend( numbers = gs_numbers,
                                        sizeEncode.fun = sizeEncode.fun,
                                        .plt.leg =  c( legend_left_x.fig,
                                                       legend_left_x.fig + max_legend_x_size.fig,
                                                       0,
                                                       legend_top_y.fig
                                        ),
                                        legend.lab = n_col,
                                        legend.lab.cex = legend.lab.cex,
                                        legend.fg = legend.fg,
                                        legend.bg = legend.bg,
                                        legend.vertex.fg = legend.vertex.fg,
                                        legend.vertex.bg = legend.vertex.bg,
                                        usr_x_coords_per_inch = uxcpi,
                                        draw.legend.box.bool = draw.legend.box.bool,
                                        h.adjust = "left",
                                        render.bool = FALSE,
                                        optimize.legend.size = TRUE  )
      plt.idx <- plt.idx + 1
      plt.l[[plt.idx]] <- legend.dat$.plt.adj
      legend.lab.cex.v[[plt.idx]] <- legend.dat$cex.lab
      legend.axis.cex.v[[plt.idx]] <- legend.dat$cex.ticks
      legend_top_y.fig <- legend.dat$.plt.adj[3] - legend_spacing.y.fig
    }

    widest_plt_idx <- which.max( sapply( X = plt.l, FUN = function( x ) x[2] - x[1] ) )
    widest_plt <- plt.l[[widest_plt_idx]]
    if( widest_plt[2] > 1 ) widest_plt[2] <- 1
    # Adjust plt.l
    plt.l <- lapply( X = plt.l, FUN = function(x){ x[2] <- widest_plt[2]; x[1] <- widest_plt[1]; x } )

    if( is.null( legend.lab.cex ) && !legend.free.cex.bool ) legend.lab.cex <- min( legend.lab.cex.v )
    if( is.null( legend.axis.cex ) && !legend.free.cex.bool ) legend.axis.cex <- min( legend.axis.cex.v )

    # Render Legend
    plt.idx <- 1

    if( !is.null( twoColorEncode.fun ) ){
      make2ColorLegend(numbers.1 = numbers.1,
                       numbers.2 = numbers.2,
                       twoColorEncode.fun = twoColorEncode.fun,
                       n = colors.n,
                       lab.1 = legend.ylab,
                       lab.2 = legend.xlab,
                       .plt.leg = plt.l[[plt.idx]],
                       cex.lab = legend.lab.cex,
                       cex.axis = legend.lab.cex,
                       draw.legend.box.bool = draw.legend.box.bool,
                       v.adjust = NULL,
                       h.adjust = NULL,
                       legend.fg = legend.fg,
                       legend.bg = legend.bg
                       ) #-> legend.dat
      plt.idx <- plt.idx + 1
    } else if (! is.null( oneColorEncode.fun ) ){
      make1ColorLegend( numbers = numbers,
                        oneColorEncode.fun = oneColorEncode.fun,
                        cex.lab = legend.lab.cex,
                        cex.axis = legend.axis.cex,
                        n = colors.n,
                        lab = legend.ylab,
                        .plt.leg = plt.l[[plt.idx]],
                        draw.legend.box.bool = draw.legend.box.bool,
                        v.adjust = "center",
                        legend.fg = legend.fg,
                        legend.bg = legend.bg
      ) #-> legend.dat
      plt.idx <- plt.idx + 1
    }

    if( !is.null(sizeEncode.fun) ){
      makeNodeSizeLegend( numbers = gs_numbers,
                          sizeEncode.fun = sizeEncode.fun,
                          .plt.leg = plt.l[[plt.idx]],
                          legend.lab = n_col,
                          legend.fg = legend.fg,
                          legend.bg = legend.bg,
                          legend.vertex.fg = legend.vertex.fg,
                          legend.vertex.bg = legend.vertex.bg,
                          usr_x_coords_per_inch = uxcpi,
                          legend.lab.cex = legend.lab.cex,
                          cex.ticks = legend.axis.cex,
                          draw.legend.box.bool = draw.legend.box.bool,
                          v.adjust = NULL,
                          h.adjust = NULL  ) #-> legend.dat
      plt.idx <- plt.idx + 1
    }
  }
  # Add Title
  if( !is.null( main ) ) title( main = main,
                                cex.main = cex.main,
                                line = lines.main )

  close_fun() -> out


  # Store plotting parameters as GSNA_plot_params attribute.
  attr( x = sigNet, which = "GSNA_plot_params" ) <- list(filename = filename,
                                                         out_format = out_format,

                                                         width = width,
                                                         height = height,
                                                         vertex.size = vertex.size,
                                                         vertex.size.range = vertex.size.range,
                                                         vertex.label.cex = vertex.label.cex,
                                                         vertex.shape =  vertex.shape,
                                                         seed = seed,
                                                         max_edge_width = max_edge_width,

                                                         transform_function = deparse(substitute(transform_function)),

                                                         edge_colors = edge_colors,
                                                         vertex_colors = vertex_colors,
                                                         vertex_colors.1 = vertex_colors.1,
                                                         vertex_colors.2 = vertex_colors.2,
                                                         combine_method = combine_method,
                                                         na.color = na.color,

                                                         vertex.label.col = vertex.label.col,
                                                         vertex.frame.color = vertex.frame.color,
                                                         contrasting_color.fun = deparse(substitute(contrasting_color.fun)),
                                                         scale_labels_by_vertex = scale_labels_by_vertex,
                                                         max_edge_width = max_edge_width,
                                                         scale.edges.by.distance = scale.edges.by.distance,
                                                         color.edges.by.distance = color.edges.by.distance,
                                                         edge_arrow_size = edge_arrow_size,

                                                         layout = deparse(substitute(layout)),
                                                         .plot = deparse(substitute(.plot)),
                                                         show.legend = show.legend,
                                                         legend.lab.cex = legend.lab.cex,
                                                         legend.axis.cex = legend.axis.cex,
                                                         legend.fg = legend.fg,
                                                         legend.bg = legend.bg,
                                                         legend.vertex.fg = legend.vertex.fg,
                                                         legend.vertex.bg = legend.vertex.bg,
                                                         font_face = font_face,
                                                         main = main,
                                                         cex.main = cex.main,
                                                         mar.main = mar.main,
                                                         lines.main = lines.main,
                                                         .mar.plot = .mar.plot,

                                                         draw.legend.box.bool = draw.legend.box.bool,
                                                         legend.free.cex.bool = legend.free.cex.bool,
                                                         legend_x_size.in = legend_x_size.in, #
                                                         colors.n = colors.n,
                                                         new = new,
                                                         legend_spacing.x.in = legend_spacing.x.in,
                                                         legend_spacing.y.in = legend_spacing.y.in,

                                                         distance = distance,
                                                         id_col = id_col,
                                                         substitute_id_col = substitute_id_col,
                                                         stat_col = stat_col,
                                                         stat_col_2 = stat_col_2,
                                                         sig_order = sig_order,
                                                         sig_order_2 = sig_order_2,
                                                         n_col = n_col,
                                                         optimal_extreme = optimal_extreme,
                                                         pathways_title_col = pathways_title_col

  )

  invisible( sigNet )
} # gsnPlotNetwork




# Break long lines (greater than 1-15 letters with a preference for longer)
break_long_lines <- function( x ){
  gsub( pattern = '(.{1,15})(\\s+)', replacement = '\\1\n',
        x = gsub( x = x, pattern = '([~:;])', replacement = '\\1 ' ) )
}

