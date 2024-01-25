# Code taken from:
# https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette


#' gsnDendroSubnetColors, gsnDendroSubnetColors_dark
#'
#' @description Given a list of vectors of gene set IDs corresponding to subnets, returns a vector of colors.
#' with each color corresponding to a subnet (see details).
#'
#' @param subnets A list of vectors containing, as elements, vectors corresponding to subnets and containing
#' gene set IDs as subnet members. List element names are the names of the subnets. This corresponds to the set
#' of subnets stored in the \code{$distances[[distance]]$subnets} field of a pared \code{GSNData} object.
#'
#' @details Given a list of vectors in which each vector contains a set of gene set IDs corresponding to a
#' subnet, with list element names being the subnet names, this function generates a vector of colors in
#' which subnets with a single member are colored black and subnets with multiple members are given associated
#' distinct colors. In the returned vector of names, the names are the subnets and the elements are the associated
#' colors. This function is primarily for generating colors for hierarchical dendrograms.
#'
#' The \code{gsnDendroSubnetColors()} and \code{gsnDendroSubnetColors_dark()} do approximately the same thing,
#' but \code{gsnDendroSubnetColors_dark()} returns a darker palette of colors.
#'
#' @return A vector of colors with names corresponding to subnet names.
#'
#' @examples
#'
#' library( GSNA )
#'
#' # After gsnAssignSubnets() has been called, a list containing subnet
#' # assignments is stored in GSNData objects at
#' # object$distances[[distance]]$subnets
#'
#' # It has this structure:
#' subnets <- list( `1` = c( "M30131", "M40742", "M29968", "M29984", "M29922",
#'                           "M30190", "M40775", "M30171", "M30154", "M30186" ),
#'                  `2` = c( "M40770" ),
#'                  `3` = c( "M30055", "M30117" ),
#'                  `4` = c( "M40804" ),
#'                  `5` = c( "M40776" ),
#'                  `6` = c( "M40846" ) )
#'
#' # Based on this, singltons, subnets/clusters with single membership
#' # are assigned black, and subnets with multiple members are assigned
#' # colors using a color wheel:
#' colors_v <- gsnDendroSubnetColors( subnets )
#'
#' # gsnDendroSubnetColors_dark does the same thing as
#' # gsnDendroSubnetColors, but picks darker colors.
#' dark_colors_v <- gsnDendroSubnetColors_dark( subnets )
#'
#' @importFrom grDevices hcl
#' @export
gsnDendroSubnetColors <- function( subnets ){
  counts.subnet <- sapply(subnets, length)
  colors.subnet <- c()
  # Colors of single membership clusters are black
  subnets.singlet <- names(counts.subnet[counts.subnet == 1])
  # Multiplet colors are determined by color wheel:
  subnets.multiplet <- names(counts.subnet[counts.subnet != 1])
  colors.subnet[subnets.singlet] <- "#000000"

  color.count <- length( subnets.multiplet )
  colz <- seq( 15, 375, length = color.count + 1 )
  colors.subnet[subnets.multiplet] <- grDevices::hcl(h = colz, l = 65, c = 100)[1:color.count]
  colors.subnet
}




#' @rdname gsnDendroSubnetColors
#'
#' @importFrom grDevices hcl
#' @export
gsnDendroSubnetColors_dark <- function( subnets ){
  #counts.subnet <- sapply(R1015R1200.FILT.best.LNvPB.Lo2Hi.GO_BP.CERNO.GSN.JH$subnets, length)
  counts.subnet <- sapply(subnets, length)
  colors.subnet <- c()
  # Colors of single membership clusters are black
  subnets.singlet <- names(counts.subnet[counts.subnet == 1])
  # Multiplet colors are determined by color wheel:
  subnets.multiplet <- names(counts.subnet[counts.subnet != 1])
  colors.subnet[subnets.singlet] <- "#000000"

  color.count <- length( subnets.multiplet )
  colz <- seq( 15, 375, length = color.count + 1 )
  colors.subnet[subnets.multiplet] <- grDevices::hcl(h = colz, l = 35, c = 100)[1:color.count]
  colors.subnet
}

