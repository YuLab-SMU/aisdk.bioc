test_that("workflow hint names and priorities are stable", {
  testthat::skip_if_not_installed("SummarizedExperiment")
  testthat::skip_if_not_installed("SingleCellExperiment")
  testthat::skip_if_not_installed("GenomicRanges")

  registry <- aisdk::create_semantic_adapter_registry()
  register_bioc_semantic_components(registry, include_workflow_hints = TRUE)

  hints <- registry$list_workflow_hints()
  expect_true("single-cell-experiment-default" %in% hints)
  expect_true("summarized-experiment-default" %in% hints)
  expect_true("granges-default" %in% hints)
})

test_that("workflow hints return advisory structure without over-promising", {
  testthat::skip_if_not_installed("SingleCellExperiment")
  testthat::skip_if_not_installed("SummarizedExperiment")
  library(SingleCellExperiment, quietly = TRUE)

  registry <- aisdk::create_semantic_adapter_registry()
  register_bioc_semantic_components(registry, include_workflow_hints = TRUE)

  counts <- matrix(1:20, nrow = 5)
  sce <- SingleCellExperiment::SingleCellExperiment(assays = list(counts = counts))

  hint <- registry$resolve_workflow_hint(sce)
  expect_type(hint, "list")
  expect_named(hint, c("workflow", "goal", "steps"))
  expect_false("recommended_tools" %in% names(hint))
  expect_false("critical_checkpoints" %in% names(hint))
  expect_identical(hint$workflow, "single_cell_default")
  expect_true(length(hint$steps) >= 3)
})

test_that("SummarizedExperiment workflow hint is advisory", {
  testthat::skip_if_not_installed("SummarizedExperiment")
  library(SummarizedExperiment, quietly = TRUE)

  registry <- aisdk::create_semantic_adapter_registry()
  register_bioc_semantic_components(registry, include_workflow_hints = TRUE)

  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = matrix(1:12, nrow = 3))
  )

  hint <- registry$resolve_workflow_hint(se)
  expect_named(hint, c("workflow", "goal", "steps"))
  expect_identical(hint$workflow, "summarized_experiment_default")
})

test_that("GRanges workflow hint is advisory", {
  testthat::skip_if_not_installed("GenomicRanges")
  library(GenomicRanges, quietly = TRUE)

  registry <- aisdk::create_semantic_adapter_registry()
  register_bioc_semantic_components(registry, include_workflow_hints = TRUE)

  gr <- GenomicRanges::GRanges(
    seqnames = c("chr1"),
    ranges = IRanges::IRanges(start = 100, width = 10)
  )

  hint <- registry$resolve_workflow_hint(gr)
  expect_named(hint, c("workflow", "goal", "steps"))
  expect_identical(hint$workflow, "granges_default")
})
