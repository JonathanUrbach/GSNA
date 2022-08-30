# Code taken from:
# https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette


#' gsnDendroSubnetColors
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
#' @export
#'
#' @examples
#' \dontrun{
#'   analysis.subnets <- analysis.GSN$distances$jaccard$subnets
#'   colors_v <- gsnDendroSubnetColors( analysis.subnets )
#' }
#'
#' @importFrom grDevices hcl
#'
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

