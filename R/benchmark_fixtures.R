#' @keywords internal
new_fixture_spec <- function(object, adapter, workflow, warn_action) {
  list(
    object = object,
    adapter = adapter,
    workflow = workflow,
    warn_action = warn_action
  )
}

#' @keywords internal
create_bioc_semantic_fixture_set <- function(root_dir = tempdir()) {
  fixture_dir <- tempfile(pattern = "aisdk-bioc-fixtures-", tmpdir = root_dir)
  dir.create(fixture_dir, recursive = TRUE, showWarnings = FALSE)

  specs <- list()

  if (requireNamespace("SummarizedExperiment", quietly = TRUE) &&
      requireNamespace("S4Vectors", quietly = TRUE)) {
    se <- SummarizedExperiment::SummarizedExperiment(
      assays = list(counts = matrix(1:12, nrow = 3)),
      rowData = S4Vectors::DataFrame(gene = letters[1:3]),
      colData = S4Vectors::DataFrame(sample = letters[1:4])
    )
    specs$summarized_experiment <- new_fixture_spec(
      object = se,
      adapter = "summarized-experiment",
      workflow = "summarized_experiment_default",
      warn_action = "materialize_all_assays"
    )
  }

  if (requireNamespace("SingleCellExperiment", quietly = TRUE) &&
      requireNamespace("S4Vectors", quietly = TRUE)) {
    sce <- SingleCellExperiment::SingleCellExperiment(
      assays = list(counts = matrix(1:20, nrow = 5)),
      colData = S4Vectors::DataFrame(cell = letters[1:4])
    )
    SingleCellExperiment::reducedDim(sce, "PCA") <- matrix(1:8, nrow = 4)
    specs$single_cell_experiment <- new_fixture_spec(
      object = sce,
      adapter = "single-cell-experiment",
      workflow = "single_cell_default",
      warn_action = "materialize_counts"
    )
  }

  if (requireNamespace("DESeq2", quietly = TRUE) &&
      requireNamespace("S4Vectors", quietly = TRUE)) {
    dds <- DESeq2::DESeqDataSetFromMatrix(
      countData = matrix(c(1, 5, 2, 6, 3, 7, 4, 8), nrow = 4),
      colData = S4Vectors::DataFrame(condition = factor(c("A", "B"))),
      design = ~condition
    )
    specs$deseq_dataset <- new_fixture_spec(
      object = dds,
      adapter = "deseq-dataset",
      workflow = "deseq_dataset_default",
      warn_action = "materialize_counts"
    )
  }

  if (requireNamespace("DelayedArray", quietly = TRUE)) {
    darr <- DelayedArray::DelayedArray(matrix(1:12, nrow = 3))
    specs$delayed_array <- new_fixture_spec(
      object = darr,
      adapter = "delayed-array",
      workflow = "delayed_array_default",
      warn_action = "materialize_as_matrix"
    )
  }

  if (requireNamespace("HDF5Array", quietly = TRUE)) {
    h5_path <- file.path(fixture_dir, "counts.h5")
    h5mat <- HDF5Array::writeHDF5Array(
      matrix(1:12, nrow = 3),
      filepath = h5_path,
      name = "counts"
    )
    specs$hdf5_array <- new_fixture_spec(
      object = h5mat,
      adapter = "hdf5-array",
      workflow = "hdf5_array_default",
      warn_action = "materialize_full_array"
    )
  }

  if (requireNamespace("GenomicRanges", quietly = TRUE) &&
      requireNamespace("IRanges", quietly = TRUE)) {
    gr <- GenomicRanges::GRanges(
      seqnames = c("chr1", "chr2"),
      ranges = IRanges::IRanges(start = c(100, 200), width = 10),
      strand = c("+", "-"),
      score = c(1L, 2L)
    )
    specs$granges <- new_fixture_spec(
      object = gr,
      adapter = "granges",
      workflow = "granges_default",
      warn_action = "materialize_ranges_table"
    )
  }

  list(
    fixture_dir = fixture_dir,
    specs = specs,
    cleanup = function() {
      unlink(fixture_dir, recursive = TRUE, force = TRUE)
      invisible(NULL)
    }
  )
}
