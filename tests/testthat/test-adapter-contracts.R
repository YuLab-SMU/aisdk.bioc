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
