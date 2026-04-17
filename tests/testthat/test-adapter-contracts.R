test_that("SummarizedExperiment adapter contract is stable", {
  testthat::skip_if_not_installed("SummarizedExperiment")
  library(SummarizedExperiment, quietly = TRUE)

  adapter <- create_summarized_experiment_semantic_adapter()

  # supports() matches the intended class
  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = matrix(1:12, nrow = 3)),
    rowData = S4Vectors::DataFrame(gene = letters[1:3]),
    colData = S4Vectors::DataFrame(sample = letters[1:4])
  )
  expect_true(adapter$supports(se))
  expect_false(adapter$supports("not-an-object"))
  expect_false(adapter$supports(list()))

  # describe_identity contains expected keys
  info <- adapter$describe_identity(se)
  expect_identical(info$primary_class, "SummarizedExperiment")
  expect_equal(info$dimensions, c(3L, 4L))
  expect_true("counts" %in% info$assays)

  # describe_schema returns class-relevant fields
  schema <- adapter$describe_schema(se)
  expect_identical(schema$kind, "SummarizedExperiment")
  expect_true("counts" %in% schema$assays)
  expect_true("gene" %in% schema$row_data_columns)
  expect_true("sample" %in% schema$col_data_columns)

  # validate_action warns on expensive materialization
  safety <- adapter$validate_action(se, "materialize_all_assays")
  expect_identical(safety$status, "warn")
  expect_true(isTRUE(safety$expensive))

  # safe actions are allowed
  safe <- adapter$validate_action(se, "preview_assay")
  expect_identical(safe$status, "allow")

  # provenance points to aisdk.bioc
  prov <- adapter$provenance(se)
  expect_identical(prov$package, "aisdk.bioc")
})

test_that("SingleCellExperiment adapter contract is stable", {
  testthat::skip_if_not_installed("SingleCellExperiment")
  testthat::skip_if_not_installed("SummarizedExperiment")
  library(SingleCellExperiment, quietly = TRUE)

  adapter <- create_single_cell_experiment_semantic_adapter()

  counts <- matrix(1:20, nrow = 5)
  sce <- SingleCellExperiment::SingleCellExperiment(
    assays = list(counts = counts),
    colData = S4Vectors::DataFrame(cell = letters[1:4])
  )
  SingleCellExperiment::reducedDim(sce, "PCA") <- matrix(1:8, nrow = 4)

  expect_true(adapter$supports(sce))
  expect_false(adapter$supports("string"))

  info <- adapter$describe_identity(sce)
  expect_identical(info$primary_class, "SingleCellExperiment")
  expect_equal(info$dimensions, c(5L, 4L))
  expect_true("counts" %in% info$assays)
  expect_true("PCA" %in% info$reduced_dims)

  schema <- adapter$describe_schema(sce)
  expect_identical(schema$kind, "SingleCellExperiment")
  expect_true("PCA" %in% schema$reduced_dims)

  safety <- adapter$validate_action(sce, "materialize_counts")
  expect_identical(safety$status, "warn")

  prov <- adapter$provenance(sce)
  expect_identical(prov$package, "aisdk.bioc")
})

test_that("GRanges adapter contract is stable", {
  testthat::skip_if_not_installed("GenomicRanges")
  library(GenomicRanges, quietly = TRUE)

  adapter <- create_granges_semantic_adapter()

  gr <- GenomicRanges::GRanges(
    seqnames = c("chr1", "chr2"),
    ranges = IRanges::IRanges(start = c(100, 200), width = 10),
    strand = c("+", "-"),
    score = c(1L, 2L)
  )

  expect_true(adapter$supports(gr))
  expect_false(adapter$supports(1:10))

  info <- adapter$describe_identity(gr)
  expect_identical(info$primary_class, "GRanges")
  expect_equal(info$length, 2L)
  expect_true("chr1" %in% info$seqlevels)

  schema <- adapter$describe_schema(gr)
  expect_identical(schema$kind, "GRanges")
  expect_true("score" %in% schema$metadata_columns)

  safety <- adapter$validate_action(gr, "materialize_ranges_table")
  expect_identical(safety$status, "warn")

  prov <- adapter$provenance(gr)
  expect_identical(prov$package, "aisdk.bioc")
})

test_that("DESeqDataSet adapter contract is stable", {
  testthat::skip_if_not_installed("DESeq2")
  library(DESeq2, quietly = TRUE)

  adapter <- create_deseq_dataset_semantic_adapter()

  counts <- matrix(c(1, 5, 2, 6, 3, 7, 4, 8), nrow = 4)
  coldata <- S4Vectors::DataFrame(condition = factor(c("A", "B")))
  dds <- DESeq2::DESeqDataSetFromMatrix(
    countData = counts,
    colData = coldata,
    design = ~condition
  )

  expect_true(adapter$supports(dds))
  expect_false(adapter$supports("not-a-dds"))

  info <- adapter$describe_identity(dds)
  expect_identical(info$primary_class, "DESeqDataSet")
  expect_equal(info$dimensions, c(4L, 2L))
  expect_true("counts" %in% info$assays)
  expect_identical(info$design, "~condition")
  expect_false(info$has_size_factors)
  expect_false(info$has_dispersions)

  schema <- adapter$describe_schema(dds)
  expect_identical(schema$kind, "DESeqDataSet")
  expect_identical(schema$design, "~condition")
  expect_true("condition" %in% schema$design_variables)

  safety <- adapter$validate_action(dds, "materialize_counts")
  expect_identical(safety$status, "warn")
  expect_true(isTRUE(safety$expensive))

  prov <- adapter$provenance(dds)
  expect_identical(prov$package, "aisdk.bioc")
})

test_that("DelayedArray adapter contract is stable", {
  testthat::skip_if_not_installed("DelayedArray")
  library(DelayedArray, quietly = TRUE)

  adapter <- create_delayed_array_semantic_adapter()
  darr <- DelayedArray::DelayedArray(matrix(1:12, nrow = 3))

  expect_true(adapter$supports(darr))
  expect_false(adapter$supports("not-a-delayed-array"))

  info <- adapter$describe_identity(darr)
  expect_identical(info$primary_class, "DelayedArray")
  expect_equal(info$dimensions, c(3L, 4L))
  expect_true("matrix" %in% info$seed_class)
  expect_false(info$is_sparse)

  schema <- adapter$describe_schema(darr)
  expect_identical(schema$kind, "DelayedArray")
  expect_true("matrix" %in% schema$seed_class)
  expect_equal(schema$seed_dimensions, c(3L, 4L))

  safety <- adapter$validate_action(darr, "materialize_as_matrix")
  expect_identical(safety$status, "warn")
  expect_true(isTRUE(safety$expensive))

  prov <- adapter$provenance(darr)
  expect_identical(prov$package, "aisdk.bioc")
})

test_that("HDF5Array adapter contract is stable", {
  testthat::skip_if_not_installed("HDF5Array")
  library(HDF5Array, quietly = TRUE)

  adapter <- create_hdf5_array_semantic_adapter()
  h5_path <- tempfile(fileext = ".h5")
  on.exit(unlink(h5_path), add = TRUE)
  h5mat <- HDF5Array::writeHDF5Array(matrix(1:12, nrow = 3), filepath = h5_path, name = "counts")
  h5_path <- normalizePath(h5_path, winslash = "/", mustWork = FALSE)

  expect_true(adapter$supports(h5mat))
  expect_false(adapter$supports(matrix(1:12, nrow = 3)))

  info <- adapter$describe_identity(h5mat)
  expect_identical(info$primary_class, "HDF5Array")
  expect_equal(info$dimensions, c(3L, 4L))
  expect_true("HDF5ArraySeed" %in% info$seed_class)
  expect_identical(info$filepath, h5_path)
  expect_identical(info$dataset_name, "/counts")

  schema <- adapter$describe_schema(h5mat)
  expect_identical(schema$kind, "HDF5Array")
  expect_identical(schema$filepath, h5_path)
  expect_identical(schema$dataset_name, "/counts")
  expect_equal(schema$chunkdim, c(3L, 4L))

  safety <- adapter$validate_action(h5mat, "materialize_full_array")
  expect_identical(safety$status, "warn")
  expect_true(isTRUE(safety$expensive))

  prov <- adapter$provenance(h5mat)
  expect_identical(prov$package, "aisdk.bioc")
})
