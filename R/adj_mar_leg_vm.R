# # This utility function scales the virtual legend margin for figures with minor dimension smaller than threshold inches (defaults to 5 inches).
# adj_mar_leg_vm <- function( .mar.leg.vm, width = par('fin')[1], height = par('fin')[2], threshold = 5 ){
#   min_hw <- min( width, height * 2 )
#   if( min_hw < threshold ){
#     .mar.leg.vm <- .mar.leg.vm * min_hw / threshold
#   }
#   .mar.leg.vm
# }


# This utility function scales the virtual legend margin for figures width smaller than threshold inches (defaults to 5 inches).
# or height < 2.5 inches.
adj_mar_leg_vm <- function( .mar.leg.vm, width = par('fin')[1], height = par('fin')[2], width.threshold = 5, height.threshold = 2.5 ){
  scale_ratio <- min( width/width.threshold, height/height.threshold )
  if( scale_ratio < 1 ){
    .mar.leg.vm <- .mar.leg.vm * scale_ratio
  }
  .mar.leg.vm
}
