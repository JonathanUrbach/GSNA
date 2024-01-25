


#' gsnToIgraph
#'
#' @description For a \code{GSNData} object containing an edge list, generate an igraph object.
#'
#' @param object A \code{GSNData} object containing a pared distance matrix and an edge list.
#' @param distance (optional) A character vector specifying a distance to use. If no \code{distance} is
#' specified, the value of the \code{default_distance} will be used.
#'
#' @return Returns an \code{igraph} object corresponding to the edges and vertices in the \code{GSNData}
#' object's edge-list data.frame.
#'
#' @details This is used by gsnPlotNetwork to generate an \code{igraph}. Users will probably not need to call
#' gsnToIgraph, for most cases. If edges are not found, it will emit an error.
#'
#' @seealso
#'  \code{\link{gsnPlotNetwork}()}
#'  \code{\link{plot.GSNData}()}
#'
#' @examples
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
#' # Now, create an igraph version of the network:
#' sig_pathways.igraph <- gsnToIgraph( object  = sig_pathways.GSN )
#'
#' # This can be plotted via igraph::plot.igraph:
#' plot( sig_pathways.igraph )
#'
#'
#' @export
gsnToIgraph <- function( object, distance = NULL ){
  stopifnot( "GSNData" %in% class( object ) )
  if( is.null(distance) ) distance <- object$default_distance
  if( is.null(distance) ) stop( 'Need distance parameter.' )
  if( is.null(object$distances[[distance]]$edges) )
    stop("No edges found for distance '", distance, "'. Need to pare network and assign subnets.")
  with( object$distances[[distance]],
        igraph::graph_from_data_frame( d = subset(edges, !is.na(M1) & !is.na(M2))[,c("M1","M2")],
                                       directed = TRUE,
                                       vertices ))
}

