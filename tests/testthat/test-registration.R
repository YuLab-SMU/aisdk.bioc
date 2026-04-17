test_that("register_bioc_semantic_components attaches Bioc adapters", {
  registry <- aisdk::create_semantic_adapter_registry()

  register_bioc_semantic_components(registry, include_workflow_hints = FALSE)

  adapters <- registry$list_adapters()
  expect_true("deseq-dataset" %in% adapters)
  expect_true("delayed-array" %in% adapters)
  expect_true("hdf5-array" %in% adapters)
  expect_true("summarized-experiment" %in% adapters)
  expect_true("single-cell-experiment" %in% adapters)
  expect_true("granges" %in% adapters)
})

test_that("register_bioc_semantic_components can add workflow hints", {
  registry <- aisdk::create_semantic_adapter_registry()

  register_bioc_semantic_components(registry, include_workflow_hints = TRUE)

  hints <- registry$list_workflow_hints()
  expect_true("deseq-dataset-default" %in% hints)
  expect_true("delayed-array-default" %in% hints)
  expect_true("hdf5-array-default" %in% hints)
  expect_true("summarized-experiment-default" %in% hints)
  expect_true("single-cell-experiment-default" %in% hints)
  expect_true("granges-default" %in% hints)
})
