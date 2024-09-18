## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(GSNA)

## -----------------------------------------------------------------------------
head( Bai_CiHep_DN.cerno )

## -----------------------------------------------------------------------------
if( utils::packageVersion( pkg = "tmod" ) >= "0.50.11" ){
  Bai_gsc.tmod <- tmod::tmod2tmodGS( Bai_gsc.tmod )
}

## -----------------------------------------------------------------------------
Bai_gsc.tmod

## -----------------------------------------------------------------------------
.alpha <- 0.05

Bai_CiHep_DN.cerno.sig <- subset( Bai_CiHep_DN.cerno, adj.P.Val <= .alpha )

## -----------------------------------------------------------------------------
Bai_CiHep_DN.sigid <- Bai_CiHep_DN.cerno.sig$ID

Bai_CiHep_DN.sigid

## -----------------------------------------------------------------------------
Bai_CiHep_DN.sig.tmod <- Bai_gsc.tmod[Bai_CiHep_DN.sigid]
Bai_CiHep_DN.sig.tmod

## -----------------------------------------------------------------------------
Bai_CiHep_genes <- toupper( rownames( Bai_CiHep_v_Fib2.de ) )

## -----------------------------------------------------------------------------
Bai_CiHep_DN.sig.GSN <- buildGeneSetNetworkSTLF( ref.background = Bai_CiHep_genes,
                                                 geneSetCollection = Bai_CiHep_DN.sig.tmod )

Bai_CiHep_DN.sig.GSN

## -----------------------------------------------------------------------------
Bai_CiHep_DN.sig.GSN <- gsnAddPathwaysData( object = Bai_CiHep_DN.sig.GSN,
                                            pathways_data = Bai_CiHep_DN.cerno.sig )

Bai_CiHep_DN.sig.GSN

## -----------------------------------------------------------------------------
Bai_CiHep_DN.sig.GSN <- gsnPareNetGenericHierarchic( object = Bai_CiHep_DN.sig.GSN )

## -----------------------------------------------------------------------------
Bai_CiHep_DN.sig.GSN <- gsnAssignSubnets( object = Bai_CiHep_DN.sig.GSN )

## -----------------------------------------------------------------------------
Bai_CiHep_DN.sig.GSN.subnets <- gsnMergePathways(object = Bai_CiHep_DN.sig.GSN )

Bai_CiHep_DN.sig.GSN.subnets

## -----------------------------------------------------------------------------
if( utils::packageVersion( pkg = "tmod" ) >= "0.50.11" ){
  #url_map_l <- list( ID = with( Bai_gsc.tmod$gs, structure( URL, names = ID ) ))
  # For tmod version 0.50.11 and later.
  url_from_ID <- with( Bai_gsc.tmod$gs, structure( URL, names = ID ) )
} else {
  #url_map_l <- list( ID = with( Bai_gsc.tmod$MODULES, structure( URL, names = ID ) ))
  # For earlier versions:
  url_from_ID <- with( Bai_gsc.tmod$MODULES, structure( URL, names = ID ) )
}

## -----------------------------------------------------------------------------
head( url_from_ID  )

## -----------------------------------------------------------------------------
yassifyPathways( Bai_CiHep_DN.sig.GSN.subnets,
                 url_map_list = list( ID = url_from_ID ) )

## -----------------------------------------------------------------------------
Bai_CiHep_DN.sig.GSN.subnetSummary <- gsnSubnetSummary( object = Bai_CiHep_DN.sig.GSN )

yassifyPathways( Bai_CiHep_DN.sig.GSN.subnetSummary,
                 url_map_list = list( Seed.ID = url_from_ID ),
                 url_map_by_words_list = list( IDs = url_from_ID ),
                 options =list( autoWidth = FALSE,
                                scrollX = TRUE
                                )
                 )

## ----eval=FALSE, include=FALSE, echo=FALSE------------------------------------
#  # SVG version of this function, but the output SVG is too large for inclusion in the vignette.
#  plot( x = Bai_CiHep_DN.sig.GSN, filename = "Bai_CiHep_DN.sig.GSN.PL.svg", width = 9, height = 7, n_col = 'N1' ) -> out
#  
#  knitr::include_graphics( "Bai_CiHep_DN.sig.GSN.PL.svg" )

## -----------------------------------------------------------------------------
gsnPlotNetwork( object = Bai_CiHep_DN.sig.GSN, filename = "Bai_CiHep_DN.sig.GSN.PL.png", width = 9, height = 7, n_col = 'N1' ) -> out

knitr::include_graphics( "Bai_CiHep_DN.sig.GSN.PL.png" )

## ----HierarchicalDendrogram_1-------------------------------------------------

gsnHierarchicalDendrogram( object = Bai_CiHep_DN.sig.GSN,
                           width = 8,
                           height = 5,
                           filename = "Bai_CiHep_DN.sig.GSN.HD.png",
                           show.leaves = TRUE,
                           show.legend = TRUE,
                           n_col = 'N1',
                           leaf_cex_range = c(0.3,1.6) )

knitr::include_graphics( path = "Bai_CiHep_DN.sig.GSN.HD.png" )


## -----------------------------------------------------------------------------
Bai_CiHep_v_CiKrt_DN.merge <- merge( x = Bai_CiHep_DN.cerno,
                                     y = Bai_CiKrt_DN.cerno,
                                     by = 'ID' )

## -----------------------------------------------------------------------------
Bai_CiHep_v_CiKrt_DN.merge$Title.y <- NULL

library( 'dplyr' )
Bai_CiHep_v_CiKrt_DN.merge <- dplyr::rename( .data = Bai_CiHep_v_CiKrt_DN.merge,
                                             Title = Title.x,
                                             cerno.CiHep = cerno.x,
                                             N1.CiHep = N1.x,
                                             AUC.CiHep = AUC.x,
                                             cES.CiHep = cES.x,
                                             P.Value.CiHep = P.Value.x,
                                             adj.P.Val.CiHep = adj.P.Val.x,
                                             cerno.CiKrt = cerno.y,
                                             N1.CiKrt = N1.y,
                                             AUC.CiKrt = AUC.y,
                                             cES.CiKrt = cES.y,
                                             P.Value.CiKrt = P.Value.y,
                                             adj.P.Val.CiKrt = adj.P.Val.y
                                             )

head( Bai_CiHep_v_CiKrt_DN.merge )

## -----------------------------------------------------------------------------
.alpha <- 0.05

Bai_CiHep_v_CiKrt_DN.filt <- subset( Bai_CiHep_v_CiKrt_DN.merge, adj.P.Val.CiHep <= .alpha | adj.P.Val.CiKrt <= .alpha )

yassifyPathways( Bai_CiHep_v_CiKrt_DN.filt,
                 url_map_list = list( ID = url_from_ID ),
                 options =list( autoWidth = FALSE,
                                scrollX = TRUE
                                )
                 )

## ----include=FALSE, results=FALSE, eval=FALSE---------------------------------
#  # THis is a little sanity check to figure out why N1's differ between the CiHep and CiKrt CERNO results sets. N1 is not the number of genes in the module in the gene set collection, but the number of genes in the gene set collection that are represented in the differential expression dataset.
#  
#  sum(toupper( rownames(Bai_CiHep_v_Fib2.de) ) %in% toupper( rownames(Bai_CiKrt_v_Fib2.de) ))
#  
#  Ns_table <- data.frame( ID = names(Bai_gsc.tmod$MODULES2GENES),
#                          N.CiHep = sapply( X = Bai_gsc.tmod$MODULES2GENES, FUN = function(x)sum( sum( unlist(x) %in% toupper( rownames(Bai_CiHep_v_Fib2.de) ) ) ) ) ,
#                          N.CiKrt = sapply( X = Bai_gsc.tmod$MODULES2GENES, FUN = function(x)sum( sum( unlist(x) %in% toupper( rownames(Bai_CiKrt_v_Fib2.de) ) ) ) )
#  )
#  
#  merge( x = Ns_table, y = Bai_CiHep_v_CiKrt_DN.merge[,c("ID", "N1.CiHep", "N1.CiKrt")], by = "ID" )
#  

## -----------------------------------------------------------------------------
Bai_CiHep_v_CiKrt_DN.sig.tmod <- Bai_gsc.tmod[ Bai_CiHep_v_CiKrt_DN.filt$ID ]

Bai_CiHep_v_CiKrt_DN.sig.tmod

## -----------------------------------------------------------------------------
Bai_CiHep_genes <- toupper( rownames( Bai_CiHep_v_Fib2.de ) )
Bai_CiKrt_genes <- toupper( rownames( Bai_CiKrt_v_Fib2.de ) )

library( 'gplots' )
gplots::venn( list(CiHep = Bai_CiHep_genes, CiKrt = Bai_CiKrt_genes) )

## -----------------------------------------------------------------------------
Bai_CiHep_v_CiKrt_DN.GSN <- GSNA::buildGeneSetNetworkSTLF( geneSetCollection = Bai_CiHep_v_CiKrt_DN.sig.tmod, ref.background = Bai_CiHep_genes )

## -----------------------------------------------------------------------------
Bai_CiHep_v_CiKrt_DN.GSN <- gsnAddPathwaysData( Bai_CiHep_v_CiKrt_DN.GSN,
                                                pathways_data = Bai_CiHep_v_CiKrt_DN.filt,
                                                stat_col = "adj.P.Val.CiHep",
                                                sig_order = "loToHi",
                                                stat_col_2 = "adj.P.Val.CiKrt",
                                                sig_order_2 = "loToHi" )

## -----------------------------------------------------------------------------
Bai_CiHep_v_CiKrt_DN.GSN

## -----------------------------------------------------------------------------
Bai_CiHep_v_CiKrt_DN.GSN <- gsnPareNetGenericHierarchic( Bai_CiHep_v_CiKrt_DN.GSN )

Bai_CiHep_v_CiKrt_DN.GSN <- gsnAssignSubnets( Bai_CiHep_v_CiKrt_DN.GSN )

yassifyPathways( gsnMergePathways( Bai_CiHep_v_CiKrt_DN.GSN  ),
                 url_map_list = list( ID = url_from_ID ),
                 options =list( autoWidth = FALSE,
                                scrollX = TRUE
                                ) )

## -----------------------------------------------------------------------------
yassifyPathways( gsnSubnetSummary( Bai_CiHep_v_CiKrt_DN.GSN  ), 
                 url_map_list = list( Seed.ID = url_from_ID ),
                 url_map_by_words_list = list( IDs = url_from_ID ),
                 options =list( autoWidth = FALSE,
                                scrollX = TRUE
                                ) )

## -----------------------------------------------------------------------------
gsnPlotNetwork( Bai_CiHep_v_CiKrt_DN.GSN, n_col = "N1.CiHep", filename = "Bai_CiHep_v_CiKrt_DN.GSN.NP.png", width = 7, height = 5 )

knitr::include_graphics( "Bai_CiHep_v_CiKrt_DN.GSN.NP.png" )

## ----NetworkPlot_AUC----------------------------------------------------------

gsnPlotNetwork( object = Bai_CiHep_v_CiKrt_DN.GSN,
                width = 8,
                height = 5,
                file = "Bai_CiHep_v_CiKrt_DN.GSN.NP_AUC.png",
                show.legend = TRUE,
                n_col = "N1.CiHep",
                stat_col = 'AUC.CiHep',
                sig_order = 'hiToLo',
                stat_col_2 = NA,
                vertex_colors = c('darkblue', 'blue', '#9999FF',  '#DDDDFF', 'white' )
)

knitr::include_graphics( path = "Bai_CiHep_v_CiKrt_DN.GSN.NP_AUC.png" )


## -----------------------------------------------------------------------------
gsnHierarchicalDendrogram( Bai_CiHep_v_CiKrt_DN.GSN,
                           n_col = "N1.CiHep",
                           filename = "Bai_CiHep_v_CiKrt_DN.GSN.HD.png",
                           width = 7,
                           height = 5,
                           show.leaves = TRUE,
                           geometry = 'circular' )

knitr::include_graphics( "Bai_CiHep_v_CiKrt_DN.GSN.HD.png" )

## ---- echo=FALSE--------------------------------------------------------------
Bai_CiHep_dorothea_UD.Gsea <- rbind( Bai_CiHep_dorothea_UP.Gsea, Bai_CiHep_dorothea_DN.Gsea )
Bai_CiHep_dorothea_UD.Gsea$`Activated In` <- sapply( X = Bai_CiHep_dorothea_UD.Gsea$NES, function(x) ifelse( x > 0, "CiHep", "Fibroblast2" ) )
Bai_CiHep_dorothea_UD.Gsea$`FDR q-val<=0.05` <- ifelse( Bai_CiHep_dorothea_UD.Gsea$`FDR q-val` <= 0.05, "significant", "not significant" )

ggplot2::ggplot( data = Bai_CiHep_dorothea_UD.Gsea,
                 mapping = ggplot2::aes( x = NES,
                                         y = `NOM p-val`,
                                         color = `Activated In`,
                                         shape = `FDR q-val<=0.05`,
                                         text = paste0( 'Gene Set Name: ',NAME ) ) ) +
  ggplot2::geom_point() +
  ggplot2::scale_shape_manual( values = c(3, 19)) +
  ggplot2::scale_color_manual( values = c( "red", "blue" ) ) 


rm( Bai_CiHep_dorothea_UD.Gsea )

## -----------------------------------------------------------------------------
Bai_CiHep_dorothea_UD.Gsea <- rbind( Bai_CiHep_dorothea_UP.Gsea, Bai_CiHep_dorothea_DN.Gsea )

yassifyPathways( Bai_CiHep_dorothea_UD.Gsea,
                 options = list( autoWidth = FALSE,
                                 scrollX = TRUE ),
                 n = 6   # <= this argument tells the function to just show the first 6 results
                 )

## -----------------------------------------------------------------------------
Bai_CiHep_dorothea_UD.Gsea[[12]] <- NULL
Bai_CiHep_dorothea_UD.Gsea$`GS DETAILS` <- NULL
Bai_CiHep_dorothea_UD.Gsea$`GS<br> follow link to MSigDB` <- NULL


## -----------------------------------------------------------------------------
Bai_CiHep_dorothea_UD.FILT <- subset( Bai_CiHep_dorothea_UD.Gsea, `FDR q-val` <= 0.05 )

## -----------------------------------------------------------------------------
Bai_gsc.tmod.gsea <- Bai_gsc.tmod[ Bai_CiHep_dorothea_UD.FILT$NAME ]

Bai_gsc.tmod.gsea

## -----------------------------------------------------------------------------
Bai_background_genes <- toupper( rownames( Bai_empty_expr_mat ) )

## -----------------------------------------------------------------------------
Bai_CiHep_dorothea_UD.GSN <- buildGeneSetNetworkJaccard( ref.background = Bai_background_genes,
                                                         geneSetCollection = Bai_gsc.tmod.gsea )

## -----------------------------------------------------------------------------
Bai_CiHep_dorothea_UC.GSN <- gsnAddPathwaysData( object = Bai_CiHep_dorothea_UD.GSN,
                                                 pathways_data = Bai_CiHep_dorothea_UD.FILT,
                                                 stat_col = 'FDR q-val',
                                                 sig_order = 'loToHi' )

Bai_CiHep_dorothea_UC.GSN

## -----------------------------------------------------------------------------
gsnDistanceHistogram( Bai_CiHep_dorothea_UC.GSN, stat = "cumulative" )

## -----------------------------------------------------------------------------
Bai_CiHep_dorothea_UC.GSN <- gsnPareNetGenericToNearestNNeighbors( object = Bai_CiHep_dorothea_UC.GSN,
                                                                   N = 1,
                                                                   keepOrphans = TRUE, cutoff = 0.02 )

Bai_CiHep_dorothea_UC.GSN <- gsnAssignSubnets( object = Bai_CiHep_dorothea_UC.GSN )

## -----------------------------------------------------------------------------
Bai_CiHep_dorothea_UC.subnets <- gsnMergePathways( object = Bai_CiHep_dorothea_UC.GSN )

yassifyPathways( Bai_CiHep_dorothea_UC.subnets,
                 options =list( autoWidth = FALSE,
                                scrollX = TRUE
                                ) )

## -----------------------------------------------------------------------------
yassifyPathways( gsnSubnetSummary( object = Bai_CiHep_dorothea_UC.GSN, summary_statistics = c("mean", "max")  ),
                 options =list( autoWidth = FALSE,
                                scrollX = TRUE
                 ) )


## -----------------------------------------------------------------------------
yassifyPathways( gsnSubnetSummary( object = Bai_CiHep_dorothea_UC.GSN,
                                   stat_col = 'FDR q-val',
                                   sig_order = 'loToHi'  ),
                 options =list( autoWidth = FALSE,
                                scrollX = TRUE
                 ) )


## -----------------------------------------------------------------------------
gsnPlotNetwork( object = Bai_CiHep_dorothea_UC.GSN,
                n_col = "SIZE",
                height = 7,
                width = 9,
                filename = "Bai_CiHep_dorothea_UC.GSN.DEF.NP.png" )

knitr::include_graphics( "Bai_CiHep_dorothea_UC.GSN.DEF.NP.png" )


## -----------------------------------------------------------------------------
gsnPlotNetwork( object = Bai_CiHep_dorothea_UC.GSN,
                n_col = "SIZE",
                stat_col = 'NES',
                sig_order = 'hiToLo',
                transform_function = identity,
                vertex_colors = c("blue", "white", "red"), 
                height = 7,
                width = 9,
                filename = "Bai_CiHep_dorothea_UC.GSN.NES.NP.png" )

knitr::include_graphics( "Bai_CiHep_dorothea_UC.GSN.NES.NP.png" )


## -----------------------------------------------------------------------------
gsnHierarchicalDendrogram( object = gsnAssignSubnets(gsnPareNetGenericHierarchic( Bai_CiHep_dorothea_UC.GSN )),
                           stat_col = 'NES',
                           sig_order = 'hiToLo',
                           n_col = "SIZE",
                           transform_function = identity,
                           show.leaves = TRUE,
                           show.legend = TRUE,
                           leaf_colors =  c("blue", "white", "red"),
                           width = 7,
                           height = 11,
                           filename = "Bai_CiHep_dorothea_UC.GSN.HD.png",
                           lab.cex = 0.8 )

knitr::include_graphics( "Bai_CiHep_dorothea_UC.GSN.HD.png" )
 

## ----echo=FALSE, results="asis"-----------------------------------------------
print( citation( package = "GSNA" ), bibtex = FALSE )

