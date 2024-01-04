
# Some functions used by the tests:

load_test_data <- function( e = parent.frame() ){
  testdata_path <- file.path( testthat::test_path(), "testdata" )
  rdafiles <- list.files( path = testdata_path, pattern = "\\.Rda$", full.names = TRUE )
  for( .f in rdafiles ){ load( .f, envir = e ) }
}



# This takes the gsnORAtest data and makes a fake David Annotation Chart
fake_david_chart <- function(ora_data = PW.ORA, .gsc = GSC){
  .nrow <- nrow( ora_data )

  .gsc <- .gsc[ora_data$ID]

  #david_fieldnames <- c("Category", "Term", "Count", "%", "PValue", "Genes", "List Total",
  #                      "Pop Hits", "Pop Total", "Fold Enrichment", "Bonferroni", "Benjamini",  "FDR")
  data.frame( Category = rep( x = "fake_gene_sets", .nrow ),
              Term = ora_data$ID,
              Count = ora_data$N,
              `%` = 100 * ora_data$d / ora_data$N,
              PValue = ora_data$P.1S,
              Genes = mapply( FUN = function( x, y ){
                paste0(collapse = ", ", x[1:y] ) },
                .gsc, ora_data$N ),
              `List Total` = ora_data$N,
              `Pop Hits` = rep( x = 6000, .nrow ),
              `Pop Total` = rep( x = 7000, .nrow ),
              `Fold Enrichment` = ora_data$Enrichment,
              Bonferroni = p.adjust( p = ora_data$P.1S, method = "bonferroni" ),
              Benjamini = p.adjust( p = ora_data$P.1S, method = "BH" ),
              FDR =  p.adjust( p = ora_data$P.1S, method = "fdr" ),
              check.names = FALSE)
}


write_fake_david_chart <- function( fake_david_chart, outfile ){
  write.table( x = fake_david_chart, file = outfile, sep = "\t", row.names = FALSE, quote = FALSE )
}

# This reformats DAVID cluster data as a fake DAVID cluster file, assigning the data to 2 fake clusters:
write_fake_david_cluster <- function( fake_david_chart, outfile, fake_cluster_ct = 2 ){
  .out <- file( description = outfile, open = "w" )
  row.names( fake_david_chart ) <- NULL
  for( i in 1:fake_cluster_ct ){
    .cluster = as.character( i )
    .ES <- - mean( log10(fake_david_chart$PValue), na.rm = TRUE )
    #"Annotation Cluster 1	Enrichment Score: 8.22391577242511"
    write( file = .out, x = paste0( "Annotation Cluster ", .cluster, "\tEnrichment Score: ", .ES ),
           append = TRUE )
    suppressWarnings( write.table( file = .out, x = fake_david_chart, sep = "\t", append = TRUE, row.names = FALSE, quote = FALSE ) )
    write( x = "", file = .out, append = TRUE )
  }
  close( .out )
}



# This takes the gsnORAtest data and makes a fake GSEA
fake_GSEA_data <- function(ora_data = PW.ORA, .gsc = GSC){
  .nrow <- nrow( ora_data )

  .gsc <- .gsc[ora_data$ID]

  # "NAME", "GS<br> follow link to MSigDB", "GS DETAILS", "SIZE", "ES", "NES", "NOM p-val", "FDR q-val",
  # "FWER p-val", "RANK AT MAX", "LEADING EDGE"
  data.frame(
    NAME = ora_data$ID,
    `GS<br> follow link to MSigDB` = ora_data$ID,
    `GS DETAILS` = rep("Details ...", .nrow ),
    SIZE = ora_data$N,
    ES = ora_data$Enrichment * 10,
    NES = ora_data$Enrichment * 10,
    `NOM p-val` = ora_data$P.1S,
    `FDR q-val` = ora_data$adj.P.1S,
    `FWER p-val` = p.adjust( p = ora_data$adj.P.1S, method = "bonferroni" ),
    `RANK AT MAX` = 1:.nrow,
    `LEADING EDGE` = 3,
    check.names = FALSE)
}


