test_that("gsnSubnetSummary works", {
  # Load test data:
  load_test_data()

  STLF.GSN.subnetSummary <- gsnSubnetSummary( STLF.GSN )

  # STLF.HM (log harmonic mean STLF) and STLF.GM (log geometric mean STLF) are statistics
  # based on the single tail log-Fisher values between gene sets within a subnet/cluster, so
  # if there's only one gene set in a cluster, the STLF.HM and the STLF.GM will be NA.
  # If there are 2 members of a cluster, them STLF.HM and STLF.GM will be equal.

  LHM_LGM_CHECK <- apply( X = STLF.GSN.subnetSummary[,c("Members", "STLF.HM","STLF.GM")],
                          MARGIN = 1,
                          FUN = function(x){ # If 1 member, then NA
                            if( x["Members"] == 1 && is.na(x["STLF.HM"]) && is.na(x["STLF.GM"]) )
                              return( TRUE ) # If 2 members, then STLF.HM == STLF.GM
                            if( x["Members"] == 2 && x["STLF.HM"] == x["STLF.GM"])
                              return( TRUE ) # If > 2 members, then STLF.HM STLF.GM not NA
                            if( x["Members"] == 3 && (!is.na( x["STLF.HM"] ) ) &&  (!is.na( x["STLF.GM"] ) ) )
                              return( TRUE )
                            return( FALSE )
                          } )

  testthat::expect_equal( object = structure( LHM_LGM_CHECK, names = NULL ),
                          expected = rep( TRUE, length( LHM_LGM_CHECK ) ) )

  testthat::expect_true( object = all( !is.na( STLF.GSN.subnetSummary$subnet ) ) )

  testthat::expect_true( object = class( STLF.GSN.subnetSummary$Seed.ID ) == 'character' )
  testthat::expect_true( object = all( !is.na( STLF.GSN.subnetSummary$Seed.ID ) ) )

  testthat::expect_true( object = class( STLF.GSN.subnetSummary$Members ) %in% c( 'integer', 'numeric') )

  testthat::expect_true( object = class( STLF.GSN.subnetSummary$`Harmonic Mean adj.P.1S` ) == 'numeric' )

  testthat::expect_true( object = class( STLF.GSN.subnetSummary$`min adj.P.1S` ) == 'numeric' )

  # The number of IDs listed must be the same as Members
  testthat::expect_equal( expected = STLF.GSN.subnetSummary$Members,
                  object = sapply(X = stringr::str_split( string = STLF.GSN.subnetSummary$IDs, pattern = "," ),
                       FUN = length) )

  # Check that lhm() works properly. Function lhm( v ) function calculates the log of the harmonic mean of the
  # e to the power of the numeric vector (v). Calculations are performed in log space to avoid numeric underruns
  # or overruns.
  testthat::expect_equal( expected = psych::harmonic.mean( c(1,2,3,4) ),
                          object =  exp( GSNA:::lhm( log( c(1,2,3,4) ) ) ) )

  testthat::expect_equal( expected = psych::harmonic.mean( c(1E-1,1E-20,1E-300) ),
                          object =  exp( GSNA:::lhm( log( c(1E-1,1E-20,1E-300) ) ) ) )
})
