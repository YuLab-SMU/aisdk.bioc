test_that("workflow hint names and priorities are stable", {
  testthat::skip_if_not_installed("SummarizedExperiment")
  testthat::skip_if_not_installed("SingleCellExperiment")
  testthat::skip_if_not_installed("GenomicRanges")

  registry <- aisdk::create_semantic_adapter_registry()
  register_bioc_semantic_components(registry, include_workflow_hints = TRUE)

  hints <- registry$list_workflow_hints()
  expect_true("deseq-dataset-default" %in% hints)
  expect_true("delayed-array-default" %in% hints)
  expect_true("hdf5-array-default" %in% hints)
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

test_that("DESeqDataSet workflow hint is advisory", {
  testthat::skip_if_not_installed("DESeq2")
  library(DESeq2, quietly = TRUE)

  registry <- aisdk::create_semantic_adapter_registry()
  register_bioc_semantic_components(registry, include_workflow_hints = TRUE)

  counts <- matrix(c(1, 5, 2, 6, 3, 7, 4, 8), nrow = 4)
  coldata <- S4Vectors::DataFrame(condition = factor(c("A", "B")))
  dds <- DESeq2::DESeqDataSetFromMatrix(
    countData = counts,
    colData = coldata,
    design = ~condition
  )

  hint <- registry$resolve_workflow_hint(dds)
  expect_named(hint, c("workflow", "goal", "steps"))
  expect_identical(hint$workflow, "deseq_dataset_default")
  expect_false("recommended_tools" %in% names(hint))
})

test_that("DelayedArray workflow hint is advisory", {
  testthat::skip_if_not_installed("DelayedArray")
  library(DelayedArray, quietly = TRUE)

  registry <- aisdk::create_semantic_adapter_registry()
  register_bioc_semantic_components(registry, include_workflow_hints = TRUE)

  darr <- DelayedArray::DelayedArray(matrix(1:12, nrow = 3))

  hint <- registry$resolve_workflow_hint(darr)
  expect_named(hint, c("workflow", "goal", "steps"))
  expect_identical(hint$workflow, "delayed_array_default")
  expect_false("recommended_tools" %in% names(hint))
})

test_that("HDF5Array workflow hint is advisory", {
  testthat::skip_if_not_installed("HDF5Array")
  library(HDF5Array, quietly = TRUE)

  registry <- aisdk::create_semantic_adapter_registry()
  register_bioc_semantic_components(registry, include_workflow_hints = TRUE)

  h5_path <- tempfile(fileext = ".h5")
  on.exit(unlink(h5_path), add = TRUE)
  h5mat <- HDF5Array::writeHDF5Array(matrix(1:12, nrow = 3), filepath = h5_path, name = "counts")

  hint <- registry$resolve_workflow_hint(h5mat)
  expect_named(hint, c("workflow", "goal", "steps"))
  expect_identical(hint$workflow, "hdf5_array_default")
  expect_false("recommended_tools" %in% names(hint))
})
