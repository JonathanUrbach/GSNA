
## Returns coordinates for use with the horizontal dendrogram. Also used for circular dendrogram with swapped and slightly adjusted x and y coords.
get_brackets_coords <- function(subnets.lf,   # A vector with keys that are leaf.names (gene sets) and values that are subnets (as characters)
                                leaf.names,   # A vector containing leaf.names in the order that they would appear in the tree.
                                labels_colors # A vector of the colors of leaves in the proper order
){
  #browser()
  if( !is.null( subnets.lf ) ){
    last_subnet <- NULL
    next_y1 <- -0.5
    j <- 0
    brackets.coords <- list( y1 = numeric(), y2 = numeric(), y3 = numeric(), x1 = numeric(), x2 = numeric(), x3 = numeric(), cluster = character(), bracket_color = character() )
    #for( vertex.name in leaf.names ){
    for( i in 1:length( leaf.names ) ){
      next_y1 <- next_y1 + 1
      leaf.name <- leaf.names[[i]]
      if( is.null( last_subnet ) || last_subnet != subnets.lf[[leaf.name]] ){
        j <- j + 1
        last_subnet <- subnets.lf[[leaf.name]]
        brackets.coords$y1[[j]] <- next_y1
        brackets.coords$y2[[j]] <- next_y1 + 1
        brackets.coords$y3[[j]] <- ( brackets.coords$y1[[j]] + brackets.coords$y2[[j]] ) / 2
        brackets.coords$x1[[j]] <- 0
        brackets.coords$x2[[j]] <- 0.25
        brackets.coords$x3[[j]] <- 0.65
        brackets.coords$cluster[[j]] <- subnets.lf[[leaf.name]]
        brackets.coords$bracket_color[[j]] <- labels_colors[[i]]
      } else{
        brackets.coords$y2[[j]] <- brackets.coords$y2[[j]] + 1
        brackets.coords$y3[[j]] <- ( brackets.coords$y1[[j]] + brackets.coords$y2[[j]] ) / 2
      }
    }
    brackets.coords <- as.data.frame( brackets.coords, stringsAsFactors = FALSE )
  }
}

