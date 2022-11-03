

#' plot.GSNData
#'
#' @description S3 method to plot \code{GSNData} objects. It is a wrapper for gsnPlotNetwork and works very similarly.
#'
#' @inheritParams gsnPlotNetwork
#'
#' @param x A \code{GSNData} class object corresponding to the \code{object} parameter of \code{gsnPlotNetwork}.
#' @param y Currently ignored, but needed for compatibility with the \code{plot} generic method.
#' @param plot_layout_params_bool (optional) A boolean value that changes the return value of the function to a list
#' containing plotting parameters when set to \code{TRUE} (see below). The default is \code{FALSE}.
#' @param ... Additional parameters, currently ignored.
#'
#'
#' @return An \code{igraph} network object is returned, invisibly, unless the \code{plot_layout_params_bool} is set as
#' \code{TRUE} in which case, the list of plotting parameters contained in the \code{igraph} object's
#' \code{GSNA_plot_params} attribute is returned instead.
#'
#' @details This method is a wrapper for \code{gsnPlotNetwork()}. It is primarily for taking \code{GSNData} object
#' containing a distance matrix, an associated \code{edges} edge-list and pathways data, and generating and rendering a
#' corresponding \code{igraph} object. The function attempts to plot the corresponding network with vertices labeled
#' with a gene set \code{ID} and corresponding \code{Title}, and colored according to the significance values
#' represented in \code{stat_col} using \code{sig_order} as an indicator of whether high or low values are more
#' significant. Edges are scaled by the value of the value of the distance statistic in the pared distance matrix.
#'
#' When the parameters \code{vertex.shape}, \code{vertex.size}, \code{vertex.label.cex}, \code{max_edge_width}, and
#' \code{edge_arrow_size} are not specified, the function attempts to pick reasonable values. These parameters are
#' assembled into a list and attached to the returned \code{igraph} object as an attribute named \code{GSNA_plot_params}.
#' To optimize plots, the user can examine these parameters by calling the following on the output of the function:
#'
#' \code{attr( x = nw.igraph, which = "GSNA_plot_params" )}
#'
#' @examples
#' \dontrun{
#' plot( x = analysis.GSN )
#' }
#'
#' @seealso \code{\link{gsnPlotNetwork}}, \code{\link{gsnToIgraph}}, \code{\link[igraph]{plot.igraph}}
#'
#' @export
#' @exportS3Method
#'
plot.GSNData <- function( x, y = NULL,
                          object,
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
                          .plot = igraph::plot.igraph,
                          plot_layout_params_bool = FALSE,
                          ...
){
  gsnPlotNetwork( object = x,
                  pathways.data = pathways.data,
                  distance = distance,
                  id_col = id_col,
                  substitute_id_col = substitute_id_col,
                  stat_col = stat_col,
                  stat_col_2 = stat_col_2,  # NA suppresses 2-color plotting behavior
                  sig_order = sig_order,
                  sig_order_2 = sig_order_2,
                  optimal_extreme = optimal_extreme,
                  transform_function = transform_function,
                  pathways_title_col = pathways_title_col,
                  edge_colors = edge_colors,
                  vertex_colors = vertex_colors,
                  vertex_colors.1 = vertex_colors.1,
                  vertex_colors.2 = vertex_colors.2,
                  filename = filename,
                  out_format = out_format,
                  width = width,
                  height = height,
                  vertex.shape = vertex.shape,
                  vertex.size = vertex.size,
                  vertex.label.cex = vertex.label.cex,
                  max_edge_width = max_edge_width,
                  edge_arrow_size = edge_arrow_size,
                  seed = seed,
                  layout = layout,
                  .plot = .plot
                  ) -> nw
  if( plot_layout_params_bool ){
    attr( x = nw, which = "GSNA_plot_params" )
  } else {
    invisible( nw )
  }
}

