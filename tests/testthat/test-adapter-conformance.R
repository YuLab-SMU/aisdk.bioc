expect_minimal_adapter_conformance <- function(adapter, obj, warn_action) {
  expect_true(adapter$supports(obj))

  identity <- adapter$describe_identity(obj)
  expect_type(identity, "list")
  expect_true(is.character(identity$primary_class))
  expect_true(length(identity$primary_class) >= 1)

  schema <- adapter$describe_schema(obj)
  expect_type(schema, "list")
  expect_true(is.character(schema$kind))
  expect_true(length(schema$kind) >= 1)

  semantics <- adapter$describe_semantics(obj)
  expect_type(semantics, "list")
  expect_true(is.character(semantics$summary))
  expect_true(length(semantics$summary) >= 1)

  estimate <- adapter$estimate_cost(obj)
  expect_type(estimate, "list")
  expect_true(all(c("tokens", "compute", "io") %in% names(estimate)))

  provenance <- adapter$provenance(obj)
  expect_type(provenance, "list")
  expect_identical(provenance$package, "aisdk.bioc")
  expect_true(is.character(provenance$adapter))
  expect_true(length(provenance$adapter) >= 1)

  safety <- adapter$validate_action(obj, warn_action)
  expect_type(safety, "list")
  expect_true(all(c("status", "reason", "category", "expensive") %in% names(safety)))
  expect_true(safety$status %in% c("warn", "deny"))
  expect_true(isTRUE(safety$expensive))

  summary_text <- adapter$render_summary(obj)
  expect_true(is.character(summary_text))
  expect_true(nzchar(summary_text))

  inspection_text <- adapter$render_inspection(obj)
  expect_true(is.character(inspection_text))
  expect_true(nzchar(inspection_text))
}

test_that("built-in semantic adapters satisfy minimal conformance on representative fixtures", {
  testthat::skip_if_not_installed("SummarizedExperiment")
  testthat::skip_if_not_installed("SingleCellExperiment")
  testthat::skip_if_not_installed("GenomicRanges")
  testthat::skip_if_not_installed("DESeq2")
  testthat::skip_if_not_installed("DelayedArray")
  testthat::skip_if_not_installed("HDF5Array")

  library(SummarizedExperiment, quietly = TRUE)
  library(SingleCellExperiment, quietly = TRUE)
  library(GenomicRanges, quietly = TRUE)
  library(DESeq2, quietly = TRUE)
  library(DelayedArray, quietly = TRUE)
  library(HDF5Array, quietly = TRUE)

  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = matrix(1:12, nrow = 3)),
    rowData = S4Vectors::DataFrame(gene = letters[1:3]),
    colData = S4Vectors::DataFrame(sample = letters[1:4])
  )
  expect_minimal_adapter_conformance(
    create_summarized_experiment_semantic_adapter(),
    se,
    "materialize_all_assays"
  )

  sce <- SingleCellExperiment::SingleCellExperiment(
    assays = list(counts = matrix(1:20, nrow = 5)),
    colData = S4Vectors::DataFrame(cell = letters[1:4])
  )
  SingleCellExperiment::reducedDim(sce, "PCA") <- matrix(1:8, nrow = 4)
  expect_minimal_adapter_conformance(
    create_single_cell_experiment_semantic_adapter(),
    sce,
    "materialize_counts"
  )

  dds <- DESeq2::DESeqDataSetFromMatrix(
    countData = matrix(c(1, 5, 2, 6, 3, 7, 4, 8), nrow = 4),
    colData = S4Vectors::DataFrame(condition = factor(c("A", "B"))),
    design = ~condition
  )
  expect_minimal_adapter_conformance(
    create_deseq_dataset_semantic_adapter(),
    dds,
    "materialize_counts"
  )

  darr <- DelayedArray::DelayedArray(matrix(1:12, nrow = 3))
  expect_minimal_adapter_conformance(
    create_delayed_array_semantic_adapter(),
    darr,
    "materialize_as_matrix"
  )

  h5_path <- tempfile(fileext = ".h5")
  on.exit(unlink(h5_path), add = TRUE)
  h5mat <- HDF5Array::writeHDF5Array(matrix(1:12, nrow = 3), filepath = h5_path, name = "counts")
  expect_minimal_adapter_conformance(
    create_hdf5_array_semantic_adapter(),
    h5mat,
    "materialize_full_array"
  )

  gr <- GenomicRanges::GRanges(
    seqnames = c("chr1", "chr2"),
    ranges = IRanges::IRanges(start = c(100, 200), width = 10),
    strand = c("+", "-"),
    score = c(1L, 2L)
  )
  expect_minimal_adapter_conformance(
    create_granges_semantic_adapter(),
    gr,
    "materialize_ranges_table"
  )
})

test_that("built-in workflow hints satisfy the advisory conformance shape", {
  testthat::skip_if_not_installed("SummarizedExperiment")
  testthat::skip_if_not_installed("SingleCellExperiment")
  testthat::skip_if_not_installed("GenomicRanges")
  testthat::skip_if_not_installed("DESeq2")
  testthat::skip_if_not_installed("DelayedArray")
  testthat::skip_if_not_installed("HDF5Array")

  library(SummarizedExperiment, quietly = TRUE)
  library(SingleCellExperiment, quietly = TRUE)
  library(GenomicRanges, quietly = TRUE)
  library(DESeq2, quietly = TRUE)
  library(DelayedArray, quietly = TRUE)
  library(HDF5Array, quietly = TRUE)

  registry <- aisdk::create_semantic_adapter_registry()
  register_bioc_semantic_components(registry, include_workflow_hints = TRUE)

  h5_path <- tempfile(fileext = ".h5")
  on.exit(unlink(h5_path), add = TRUE)

  fixtures <- list(
    SummarizedExperiment::SummarizedExperiment(assays = list(counts = matrix(1:12, nrow = 3))),
    {
      sce <- SingleCellExperiment::SingleCellExperiment(assays = list(counts = matrix(1:20, nrow = 5)))
      SingleCellExperiment::reducedDim(sce, "PCA") <- matrix(1:8, nrow = 4)
      sce
    },
    DESeq2::DESeqDataSetFromMatrix(
      countData = matrix(c(1, 5, 2, 6, 3, 7, 4, 8), nrow = 4),
      colData = S4Vectors::DataFrame(condition = factor(c("A", "B"))),
      design = ~condition
    ),
    DelayedArray::DelayedArray(matrix(1:12, nrow = 3)),
    HDF5Array::writeHDF5Array(matrix(1:12, nrow = 3), filepath = h5_path, name = "counts"),
    GenomicRanges::GRanges(
      seqnames = c("chr1"),
      ranges = IRanges::IRanges(start = 100, width = 10)
    )
  )

  for (fixture in fixtures) {
    hint <- registry$resolve_workflow_hint(fixture)
    expect_type(hint, "list")
    expect_named(hint, c("workflow", "goal", "steps"))
    expect_true(is.character(hint$workflow))
    expect_true(length(hint$workflow) == 1)
    expect_true(is.character(hint$steps))
    expect_true(length(hint$steps) >= 3)
    expect_false("recommended_tools" %in% names(hint))
    expect_false("critical_checkpoints" %in% names(hint))
  }
})
