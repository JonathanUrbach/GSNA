
#' @rdname gsnDendroSubnetColors
#'
#' @importFrom grDevices hcl
#'
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
