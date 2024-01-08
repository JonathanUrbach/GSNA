test_that("gsnSubnetSummary works", {
  # Load test data:
  load_test_data()

  STLF.GSN.subnetSummary <- gsnSubnetSummary( STLF.GSN )

  # LHM.STLF (log harmonic mean STLF) and LGM.STLF (log geometric mean STLF) are statistics
  # based on the single tail log-Fisher values between gene sets within a subnet/cluster, so
  # if there's only one gene set in a cluster, the LHM.STLF and the LGM.STLF will be NA.
  # If there are 2 members of a cluster, them LHM.STLF and LGM.STLF will be equal.

  LHM_LGM_CHECK <- apply( X = STLF.GSN.subnetSummary[,c("Members", "LHM.STLF","LGM.STLF")],
                          MARGIN = 1,
                          FUN = function(x){ # If 1 member, then NA
                            if( x["Members"] == 1 && is.na(x["LHM.STLF"]) && is.na(x["LGM.STLF"]) )
                              return( TRUE ) # If 2 members, then LHM.STLF == LGM.STLF
                            if( x["Members"] == 2 && x["LHM.STLF"] == x["LGM.STLF"])
                              return( TRUE ) # If > 2 members, then LHM.STLF LGM.STLF not NA
                            if( x["Members"] == 3 && (!is.na( x["LHM.STLF"] ) ) &&  (!is.na( x["LGM.STLF"] ) ) )
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
})
