invisible( utils::globalVariables( c("log2FoldChange", "pvalue" ) ) )

orderByNegLogPadj <- function( rslt, hiToLo = TRUE, padjIsSigned = TRUE ){
  transform.padj <- identity
  if( ! padjIsSigned ) transform.padj <- abs
  #rslt <- within( rslt, {pi_score <- log2FoldChange * ifelse( test = is.na( pvalue ), yes = 0, no = -log10(p.adjust( p = pvalue, method = "BH" )))})
  rslt <- within( rslt, {signedLogPadj <- ifelse( test = log2FoldChange >=0, yes = 1, no = -1 ) *
    ifelse( test = is.na( pvalue ), yes = 0, no = -log10(p.adjust( p = pvalue, method = "BH" )))
  } )
  rslt[order((1-hiToLo*2) * transform.padj(rslt$signedLogPadj)),]
}

