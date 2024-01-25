
#' gsnAssignSubnets
#'
#' @description Main wrapper method for assigning subnets.
#'
#' @param object An object of type \code{GSNData} containing pathways data and a pared distance matrix.
#' @param distance (optional) character vector of length 1 indicating which pared distance matrix is to be used for assigning
#' subnets. This defaults to the 'default_distance'.
#' @param scoreCol (optional) A score column used for ordering edges. See explanation below. If there are 3 or more columns
#' the last one is presumed to be the score column and used for ordering. The score is usually derived from a pathways
#' score but may also be derived the pared distance matrix.
#' @param highToLow (optional) A boolean indicating how scores are to be ordered based on significance, low to high, or
#' high to low.
#'
#' @details Calls the private \code{assignSubnets} function using scores derived from pathways data, starting with the most significant
#' edge scores in a subnet, and subsequently joining additional vertices in order of the best score.
#'
#' @return The method returns a GSNData object containing the following data for the indicated distance matrix:
#'\describe{
#'   \item{\code{edges}}{The edges data.frame, but with a subnet column added.}
#'   \item{\code{subnets}}{A list of vectors such that the names of the vectors are the names of subnets, and the contents
#'         of each vector are the gene sets making up that vector.}
#'   \item{\code{vertex_subnets}}{A data.frame containing the name of a vertex and its assigned subnet.}
#'   }
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
#' # Now we can pare the network. By default, the distances are complemented
#' # and converted into ranks for the sake of generating a network.
#' sig_pathways.GSN <- gsnPareNetGenericHierarchic( object = sig_pathways.GSN )
#'
#' # Once the network has been pared, gsnAssignSubnets() can be called:
#' sig_pathways.GSN <- gsnAssignSubnets( object = sig_pathways.GSN )
#'
#'
#' @export
#'
gsnAssignSubnets <- function( object, distance = NULL, scoreCol = NULL, highToLow = NULL ){
  stopifnot( "GSNData" %in% class( object ) )
  if( is.null( distance ) ) distance <- object$default_distance
  if( is.null( scoreCol ) ) scoreCol <- object$pathways$stat_col
  if( is.null( highToLow ) ) highToLow <- object$pathways$sig_order == 'hiToLo'

  #merge( x =  object$distances[[distance]]$edges, y = object$pathways$data, by.x = "M1", by.y = "ID"  )

  edges.df <- object$distances[[distance]]$edges
  if( is.null( edges.df ) ) stop( "No edges found. The network needs paring by running gsnPareNetGenericToNearestNNeighbors()",
                                  " or gsnPareNetGenericHierarchic()." )

  if( ! scoreCol %in% colnames( edges.df ) ){
    edges.df <- merge( x =  object$distances[[distance]]$edges,
                       y = object$pathways$data,
                       by.x = "M1",
                       by.y = object$pathways$id_col,
                       all.x = TRUE  )
  }

  subnets.l <- assignSubnets( edges.df = edges.df, scoreCol = scoreCol, highToLow = highToLow )

  if( !is.null(object$distances[[distance]]$clusters) )
    subnets.l$vertex_subnets$hcluster <- object$distances[[distance]]$clusters[subnets.l$vertex_subnets$vertex]

  object$distances[[distance]]$edges <- subnets.l$edges[,1:3]
  object$distances[[distance]]$subnets <- subnets.l$subnets
  object$distances[[distance]]$vertex_subnets <- subnets.l$vertex_subnets

  object
}

