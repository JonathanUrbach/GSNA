# Scoring
test_that( "score_word_clusters works", {
  .terms <- c("complement activation", "complement cascade", "complement binding",
             "wp complement activation classical pathway",
             "complement activation classical pathway",
             "humoral immune response mediated by circulating immunoglobulin",
             "comp pathway", "regulation of complement activation",
             "classic pathway", "complement activation alternative pathway",
             "initial triggering of complement",  "lectin pathway",
             "opsonin binding")
  word_cluster_scores <- score_word_clusters( .terms )
  expect_equal( object = word_cluster_scores[1], expected = c(`complement activation` = 5.0676903) )
  word_cluster_scores2 <- score_word_clusters( .terms, single_word_score = 0.4, additional_word_score = 1, priority_factor = 1.1 )
  expect_equal( object = word_cluster_scores2[1], expected = c(`complement activation classical pathway` = 4.876716 ) )
})

# Filtering
test_that( "filterTerms works", {
  .unfiltered_terms <- c("GOBP_COMPLEMENT_ACTIVATION", "REACTOME_COMPLEMENT_CASCADE",
                                "GOMF_COMPLEMENT_BINDING", "WP_COMPLEMENT_ACTIVATION_CLASSICAL_PATHWAY",
                                "GOBP_COMPLEMENT_ACTIVATION_CLASSICAL_PATHWAY", "BIOCARTA_LECTIN_PATHWAY",
                                "GOMF_OPSONIN_BINDING", "GOCC_GOLGI_LUMEN" )

  .filtered_terms <- filterTerms( terms = .unfiltered_terms )

  expect_equal( object = .filtered_terms,
                expected = c(GOBP_COMPLEMENT_ACTIVATION = "complement activation",
                             REACTOME_COMPLEMENT_CASCADE = "complement cascade",
                             GOMF_COMPLEMENT_BINDING = "complement binding",
                             WP_COMPLEMENT_ACTIVATION_CLASSICAL_PATHWAY = "complement activation classical pathway",
                             GOBP_COMPLEMENT_ACTIVATION_CLASSICAL_PATHWAY = "complement activation classical pathway",
                             BIOCARTA_LECTIN_PATHWAY = "lectin pathway",
                             GOMF_OPSONIN_BINDING = "opsonin binding",
                             GOCC_GOLGI_LUMEN = "golgi lumen") )

})

# termSummary
test_that( "termSummary works", {
  .expect <- c(`1` = "natural killer cell", `2` = "cell activation involved immune response")
  .terms = c( "GOBP_REGULATION_OF_NATURAL_KILLER_CELL_ACTIVATION", # 1...
              "GOBP_POSITIVE_REGULATION_OF_NATURAL_KILLER_CELL_ACTIVATION",
              "GOBP_NATURAL_KILLER_CELL_ACTIVATION",
              "GOBP_POSITIVE_REGULATION_OF_NATURAL_KILLER_CELL_PROLIFERATION",
              "GOBP_ANTIBODY_DEPENDENT_CELLULAR_CYTOTOXICITY",
              "GOBP_TYPE_II_HYPERSENSITIVITY",
              "GOBP_NATURAL_KILLER_CELL_PROLIFERATION",
              "GOBP_MYELOID_LEUKOCYTE_ACTIVATION",                 # 2...
              "GOBP_MYELOID_LEUKOCYTE_MEDIATED_IMMUNITY",
              "GOBP_NATURAL_KILLER_CELL_ACTIVATION_INVOLVED_IN_IMMUNE_RESPONSE",
              "GOBP_MYELOID_CELL_ACTIVATION_INVOLVED_IN_IMMUNE_RESPONSE",
              "GOBP_LEUKOCYTE_DEGRANULATION",
              "GOBP_NATURAL_KILLER_CELL_DEGRANULATION",
              "GOBP_PHAGOCYTOSIS",
              "GOBP_REGULATED_EXOCYTOSIS" )
   .groups = c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,2)
   expect_equal( object = unname( termSummary( terms = .terms, group = .groups ) ),
                 expected = unname( .expect ) )

   # When there's no group argument, the output vector has no names.
   .expect2 <- c( "natural killer cell", "cell activation involved immune response")
   .terms2 <- c( "GOBP_REGULATION_OF_NATURAL_KILLER_CELL_ACTIVATION, GOBP_POSITIVE_REGULATION_OF_NATURAL_KILLER_CELL_ACTIVATION, GOBP_NATURAL_KILLER_CELL_ACTIVATION, GOBP_POSITIVE_REGULATION_OF_NATURAL_KILLER_CELL_PROLIFERATION, GOBP_ANTIBODY_DEPENDENT_CELLULAR_CYTOTOXICITY, GOBP_TYPE_II_HYPERSENSITIVITY, GOBP_NATURAL_KILLER_CELL_PROLIFERATION",
      "GOBP_MYELOID_LEUKOCYTE_ACTIVATION, GOBP_MYELOID_LEUKOCYTE_MEDIATED_IMMUNITY, GOBP_NATURAL_KILLER_CELL_ACTIVATION_INVOLVED_IN_IMMUNE_RESPONSE, GOBP_MYELOID_CELL_ACTIVATION_INVOLVED_IN_IMMUNE_RESPONSE, GOBP_LEUKOCYTE_DEGRANULATION, GOBP_NATURAL_KILLER_CELL_DEGRANULATION, GOBP_PHAGOCYTOSIS, GOBP_REGULATED_EXOCYTOSIS" )
   expect_equal( object = unname( termSummary( terms = .terms2 ) ), expected = unname( .expect2 ) )
})

