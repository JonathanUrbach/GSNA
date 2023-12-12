# .onAttach
.onAttach <- function( libname, pkgname ){
  packageStartupMessage( "Loading GSNA....\nFor a vignette, run:\n  vignette( \"using_the_gsna_package\" )\n" )
}
