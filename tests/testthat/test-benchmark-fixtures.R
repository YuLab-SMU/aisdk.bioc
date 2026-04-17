test_that("benchmark fixture generators return the expected fixture specs", {
  testthat::skip_if_not_installed("SummarizedExperiment")
  testthat::skip_if_not_installed("SingleCellExperiment")
  testthat::skip_if_not_installed("GenomicRanges")
  testthat::skip_if_not_installed("IRanges")
  testthat::skip_if_not_installed("S4Vectors")
  testthat::skip_if_not_installed("DESeq2")
  testthat::skip_if_not_installed("DelayedArray")
  testthat::skip_if_not_installed("HDF5Array")

  fixtures <- create_bioc_semantic_fixture_set()
  on.exit(fixtures$cleanup(), add = TRUE)

  expect_true(dir.exists(fixtures$fixture_dir))
  expect_setequal(
    names(fixtures$specs),
    c(
      "summarized_experiment",
      "single_cell_experiment",
      "deseq_dataset",
      "delayed_array",
      "hdf5_array",
      "granges"
    )
  )

  for (spec in fixtures$specs) {
    expect_type(spec, "list")
    expect_true(all(c("object", "adapter", "workflow", "warn_action") %in% names(spec)))
    expect_true(is.character(spec$adapter))
    expect_true(length(spec$adapter) == 1)
    expect_true(is.character(spec$workflow))
    expect_true(length(spec$workflow) == 1)
    expect_true(is.character(spec$warn_action))
    expect_true(length(spec$warn_action) == 1)
  }
})

test_that("benchmark fixtures resolve to the expected adapters and workflow hints", {
  testthat::skip_if_not_installed("SummarizedExperiment")
  testthat::skip_if_not_installed("SingleCellExperiment")
  testthat::skip_if_not_installed("GenomicRanges")
  testthat::skip_if_not_installed("IRanges")
  testthat::skip_if_not_installed("S4Vectors")
  testthat::skip_if_not_installed("DESeq2")
  testthat::skip_if_not_installed("DelayedArray")
  testthat::skip_if_not_installed("HDF5Array")

  fixtures <- create_bioc_semantic_fixture_set()
  on.exit(fixtures$cleanup(), add = TRUE)

  registry <- aisdk::create_semantic_adapter_registry()
  register_bioc_semantic_components(registry, include_workflow_hints = TRUE)

  for (spec in fixtures$specs) {
    adapter <- registry$resolve(spec$object)
    expect_identical(adapter$name, spec$adapter)

    hint <- registry$resolve_workflow_hint(spec$object)
    expect_identical(hint$workflow, spec$workflow)

    safety <- adapter$validate_action(spec$object, spec$warn_action)
    expect_true(safety$status %in% c("warn", "deny"))
    expect_true(isTRUE(safety$expensive))
  }
})

test_that("benchmark fixture cleanup removes the generated fixture directory", {
  testthat::skip_if_not_installed("HDF5Array")

  fixtures <- create_bioc_semantic_fixture_set()
  fixture_dir <- fixtures$fixture_dir

  expect_true(dir.exists(fixture_dir))
  fixtures$cleanup()
  expect_false(dir.exists(fixture_dir))
})
