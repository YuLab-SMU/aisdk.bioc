if (!exists("create_semantic_adapter_registry", envir = asNamespace("aisdk"), inherits = FALSE)) {
  testthat::skip("installed aisdk lacks semantic registry split API; load current aisdk source or install newer core")
}

test_that("register_bioc_semantic_components attaches Bioc adapters", {
  registry <- getFromNamespace("create_semantic_adapter_registry", "aisdk")()

  register_bioc_semantic_components(registry, include_workflow_hints = FALSE)

  adapters <- registry$list_adapters()
  expect_true("summarized-experiment" %in% adapters)
  expect_true("single-cell-experiment" %in% adapters)
  expect_true("granges" %in% adapters)
})

test_that("register_bioc_semantic_components can add workflow hints", {
  registry <- getFromNamespace("create_semantic_adapter_registry", "aisdk")()

  register_bioc_semantic_components(registry, include_workflow_hints = TRUE)

  hints <- registry$list_workflow_hints()
  expect_true("summarized-experiment-default" %in% hints)
  expect_true("single-cell-experiment-default" %in% hints)
  expect_true("granges-default" %in% hints)
})
