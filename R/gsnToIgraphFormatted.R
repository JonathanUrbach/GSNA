
gsnToIgraphFormatted <- function( object, pathways.data = NULL, vertex.shape = "circle", vertex.size = 10,
                                  distance = NULL, id_col = NULL, stat_col = NULL, sig_order = NULL,
                                  optimal_extreme = NULL, transform_function = log10,
                                  pathways_title_col = 'Title',
                                  max_edge_width = 16,
                                  edge_colors = c("#000000FF", "purple", "blue", "green","yellow4", "orange","red")
){
  stopifnot( "GSNData" %in% class( object ) )
  if( is.null(distance) ) distance <- object$default_distance
  if( is.null(pathways.data) ) pathways.data <- object$pathways$data
  if( !is.null(pathways.data) ){
    if( is.null(id_col) ) id_col <- object$pathways$id_col
    if( is.null(stat_col) ) stat_col <- object$pathways$stat_col
    if( is.null(sig_order) ) sig_order <- object$pathways$sig_order
    if( is.null(id_col) ) stop( "id_col is not defined" )
    if( is.null(stat_col) ) stop( "stat_col is not defined" )
    if( is.null(sig_order) ) stop( "sig_order is not defined" )
  }
  if( is.null( optimal_extreme ) ) optimal_extreme <- object$distances[[distance]]$optimal_extreme
  if( is.null( optimal_extreme ) ) stop( "optimal_extreme is not defined" )

  sigNet <- gsnToIgraph( object, distance )

  # Node characteristics in this implementation are dependent on pathways.data, whereas edge characterisics are
  # dependent on the distance matrix.
  if( ! is.null( pathways.data ) ){
    #pathways.data$color <- myColorF( -log10(pathways.data$adj.P.Val) ) # Original implementation
    pathways.data$color <- myColorF( c(loToHi=-1, hiToLo = 1)[[as.character(sig_order)]] * transform_function(pathways.data[[stat_col]]) )
    igraph::vertex_attr(sigNet, "color") <- pathways.data[igraph::V(sigNet)$name,"color"]


    if( is.null( pathways_title_col ) ) {
      if( is.na( pathways_title_col <- c( "Title", "Name", "NAME", "ID" )[c( "Title", "Name", "NAME", "ID" ) %in% colnames(pathways.data)][1] ) )
        pathways_title_col <- NULL
    }

    if( !is.null( pathways_title_col ) )
      igraph::V(sigNet)$label <- paste0( igraph::V(sigNet)$name,
                                         "\n",
                                         gsub( x = pathways.data[igraph::V(sigNet)$name, pathways_title_col ],
                                               pattern = '(.{1,15})(\\s)',
                                               replacement = '\\1\n' ) )# Adds converts '\s' to '\n' after up to 1
  }

  paredDistRange <- structure(  range( object$distances[[distance]]$pared, na.rm = TRUE ), names = c("min","max" ) )

  if( optimal_extreme == "max" ){
    paredDistRange <- structure(rev( paredDistRange ), names = c("min","max" ) )
  }
  scale_fun <- function(x){ (x - paredDistRange[['min']]) / ( paredDistRange[['max']] - paredDistRange[['min']] ) }
  convert_fun <- function(x){ scale_fun(object$distances[[distance]]$pared[x[1], x[2]] ) }

  igraph::E(sigNet)$width <-
    1+as.integer( apply( X = read.table( text = attr( igraph::E(sigNet),"vnames" ), sep = "|"),
                         MARGIN = 1,
                         FUN = convert_fun ) * max_edge_width )

  igraph::E(sigNet)$color <-
    myColorF( numbers = 1+as.integer(apply(X = read.table(text = attr( igraph::E(sigNet),"vnames" ), sep = "|"),
                                           MARGIN = 1,
                                           FUN = convert_fun ) * (length( edge_colors ) - 1 ) ),
              n = length( edge_colors ),
              colors = edge_colors )


  igraph::V(sigNet)$shape <- vertex.shape
  igraph::V(sigNet)$size <- vertex.size

  sigNet
}

