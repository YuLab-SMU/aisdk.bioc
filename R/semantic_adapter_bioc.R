#' @title Bioc Semantic Adapter Boundary
#' @description Built-in Bioconductor-oriented semantic adapters and default
#' workflow hints. This file is the domain layer boundary that can later be
#' split into a separate extension package such as `aisdk.bioc`.
#' @name semantic_adapter_bioc
NULL

#' @keywords internal
available_bioc_packages <- function() {
  pkgs <- c("SummarizedExperiment", "SingleCellExperiment", "GenomicRanges")
  available <- vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
  pkgs[available]
}

#' @keywords internal
is_bioc_package_available <- function(pkg) {
  if (!nzchar(pkg)) return(FALSE)
  requireNamespace(pkg, quietly = TRUE)
}

#' @keywords internal
register_default_bioc_semantic_components <- function(registry, include_workflow_hints = TRUE) {
  registry$register(create_single_cell_experiment_semantic_adapter())
  registry$register(create_summarized_experiment_semantic_adapter())
  registry$register(create_granges_semantic_adapter())

  if (isTRUE(include_workflow_hints)) {
    registry$register_workflow_hint(
      name = "single-cell-experiment-default",
      supports = function(obj) is_semantic_class(obj, "SingleCellExperiment"),
      priority = 300,
      hint_fn = function(obj, goal = NULL) {
        list(
          workflow = "single_cell_default",
          goal = goal,
          steps = c("inspect assays and cell metadata", "quality control", "normalization and reduced dimensions", "cluster annotation", "report")
        )
      }
    )

    registry$register_workflow_hint(
      name = "summarized-experiment-default",
      supports = function(obj) is_semantic_class(obj, "SummarizedExperiment"),
      priority = 200,
      hint_fn = function(obj, goal = NULL) {
        list(
          workflow = "summarized_experiment_default",
          goal = goal,
          steps = c("inspect assays and design metadata", "check experimental design", "normalize and analyze", "summarize and report")
        )
      }
    )

    registry$register_workflow_hint(
      name = "granges-default",
      supports = function(obj) is_semantic_class(obj, "GRanges"),
      priority = 250,
      hint_fn = function(obj, goal = NULL) {
        list(
          workflow = "granges_default",
          goal = goal,
          steps = c("inspect seqlevels and metadata", "harmonize chromosome naming if needed", "annotate or overlap ranges", "summarize and report")
        )
      }
    )
  }

  registry
}

#' @keywords internal
create_summarized_experiment_semantic_adapter <- function() {
  create_semantic_adapter(
    name = "summarized-experiment",
    supports = function(obj) is_semantic_class(obj, "SummarizedExperiment"),
    capabilities = c("identity", "schema", "semantics", "metadata", "preview", "budget_estimate", "safety_checks"),
    priority = 200,
    describe_identity = function(obj) {
      dims <- tryCatch(dim(obj), error = function(e) NULL)
      list(
        class = class(obj),
        primary_class = "SummarizedExperiment",
        dimensions = if (is.null(dims)) NULL else as.integer(dims),
        assays = call_object_accessor(obj, c("assayNames"), default = character(0))
      )
    },
    describe_schema = function(obj) {
      assay_names <- call_object_accessor(obj, c("assayNames"), default = character(0))
      row_data <- call_object_accessor(obj, c("rowData"), default = NULL)
      col_data <- call_object_accessor(obj, c("colData"), default = NULL)
      metadata_names <- names(call_object_accessor(obj, c("metadata"), default = list())) %||% character(0)
      list(
        kind = "SummarizedExperiment",
        assays = assay_names,
        row_data_columns = colnames(row_data) %||% character(0),
        col_data_columns = colnames(col_data) %||% character(0),
        metadata_names = metadata_names
      )
    },
    describe_semantics = function(obj) {
      list(summary = "Feature-by-sample experimental container with assays, row annotations, and column annotations.")
    },
    list_accessors = function(obj) {
      c("assayNames()", "assays()", "rowData()", "colData()", "metadata()")
    },
    estimate_cost = function(obj) {
      dims <- tryCatch(dim(obj), error = function(e) c(NA_integer_, NA_integer_))
      size_hint <- if (all(!is.na(dims))) prod(dims) else NA_real_
      list(
        tokens = "low",
        compute = if (!is.na(size_hint) && size_hint > 1e6) "medium" else "low",
        io = "none"
      )
    },
    provenance = function(obj) {
      list(adapter = "summarized-experiment", package = "aisdk.bioc")
    },
    validate_action = function(obj, action) {
      if (identical(action, "materialize_all_assays")) {
        return(list(
          status = "warn",
          reason = "Materializing all assays may be expensive; prefer preview or slicing.",
          category = "materialization",
          expensive = TRUE
        ))
      }
      list(status = "allow", reason = "Safe read-oriented operation.", category = "read", expensive = FALSE)
    },
    render_summary = function(obj, name = NULL) {
      dims <- tryCatch(dim(obj), error = function(e) c(NA_integer_, NA_integer_))
      assay_names <- call_object_accessor(obj, c("assayNames"), default = character(0))
      row_data <- call_object_accessor(obj, c("rowData"), default = NULL)
      col_data <- call_object_accessor(obj, c("colData"), default = NULL)

      paste0(
        "**SummarizedExperiment** (",
        if (all(!is.na(dims))) paste0(dims[1], " features x ", dims[2], " samples") else "dimensions unavailable",
        ")\n\n",
        "**Assays:** ", if (length(assay_names)) paste(assay_names, collapse = ", ") else "(none)", "\n",
        "**rowData columns:** ", if (!is.null(row_data) && ncol(row_data) > 0) paste(colnames(row_data), collapse = ", ") else "(none)", "\n",
        "**colData columns:** ", if (!is.null(col_data) && ncol(col_data) > 0) paste(colnames(col_data), collapse = ", ") else "(none)"
      )
    },
    render_inspection = function(obj, name = NULL, head_rows = 6) {
      dims <- tryCatch(dim(obj), error = function(e) c(NA_integer_, NA_integer_))
      assay_names <- call_object_accessor(obj, c("assayNames"), default = character(0))
      row_data <- call_object_accessor(obj, c("rowData"), default = NULL)
      col_data <- call_object_accessor(obj, c("colData"), default = NULL)

      lines <- c(
        paste0("Variable: ", name %||% "<unnamed>"),
        paste0("Type: ", paste(class(obj), collapse = ", ")),
        paste0("Size: ", format(utils::object.size(obj), units = "auto")),
        ""
      )
      if (all(!is.na(dims))) {
        lines <- c(lines, paste0("Dimensions: ", dims[1], " features x ", dims[2], " samples"))
      }
      lines <- c(lines, paste0("Assays: ", if (length(assay_names)) paste(assay_names, collapse = ", ") else "(none)"))
      lines <- c(lines, paste0("rowData columns: ", if (!is.null(row_data) && ncol(row_data) > 0) paste(colnames(row_data), collapse = ", ") else "(none)"))
      lines <- c(lines, paste0("colData columns: ", if (!is.null(col_data) && ncol(col_data) > 0) paste(colnames(col_data), collapse = ", ") else "(none)"))
      if (!is.null(col_data) && nrow(col_data) > 0) {
        lines <- c(lines, "", "colData preview:")
        lines <- c(lines, utils::capture.output(print(utils::head(as.data.frame(col_data), head_rows))))
      }
      paste(lines, collapse = "\n")
    }
  )
}

#' @keywords internal
create_single_cell_experiment_semantic_adapter <- function() {
  create_semantic_adapter(
    name = "single-cell-experiment",
    supports = function(obj) is_semantic_class(obj, "SingleCellExperiment"),
    capabilities = c("identity", "schema", "semantics", "metadata", "reduced_dims", "preview", "budget_estimate", "safety_checks"),
    priority = 300,
    describe_identity = function(obj) {
      dims <- tryCatch(dim(obj), error = function(e) NULL)
      list(
        class = class(obj),
        primary_class = "SingleCellExperiment",
        dimensions = if (is.null(dims)) NULL else as.integer(dims),
        assays = call_object_accessor(obj, c("assayNames"), default = character(0)),
        reduced_dims = call_object_accessor(obj, c("reducedDimNames"), default = character(0))
      )
    },
    describe_schema = function(obj) {
      sce_schema <- create_summarized_experiment_semantic_adapter()$describe_schema(obj)
      sce_schema$kind <- "SingleCellExperiment"
      sce_schema$reduced_dims <- call_object_accessor(obj, c("reducedDimNames"), default = character(0))
      sce_schema
    },
    describe_semantics = function(obj) {
      list(summary = "Single-cell annotated matrix with assays, cell metadata, feature metadata, and reduced dimensions.")
    },
    list_accessors = function(obj) {
      c("assayNames()", "assays()", "rowData()", "colData()", "reducedDimNames()", "reducedDims()", "metadata()")
    },
    estimate_cost = function(obj) {
      dims <- tryCatch(dim(obj), error = function(e) c(NA_integer_, NA_integer_))
      size_hint <- if (all(!is.na(dims))) prod(dims) else NA_real_
      list(
        tokens = "low",
        compute = if (!is.na(size_hint) && size_hint > 5e6) "medium" else "low",
        io = "none"
      )
    },
    provenance = function(obj) {
      list(adapter = "single-cell-experiment", package = "aisdk.bioc")
    },
    validate_action = function(obj, action) {
      if (identical(action, "materialize_all_assays")) {
        return(list(
          status = "warn",
          reason = "Materializing all assays in a single-cell object may be expensive; prefer previews or slices.",
          category = "materialization",
          expensive = TRUE
        ))
      }
      if (identical(action, "materialize_counts")) {
        return(list(
          status = "warn",
          reason = "Materializing the full count matrix may be expensive; prefer previews or slices.",
          category = "materialization",
          expensive = TRUE
        ))
      }
      list(status = "allow", reason = "Safe read-oriented operation.", category = "read", expensive = FALSE)
    },
    render_summary = function(obj, name = NULL) {
      dims <- tryCatch(dim(obj), error = function(e) c(NA_integer_, NA_integer_))
      assay_names <- call_object_accessor(obj, c("assayNames"), default = character(0))
      reduced_dims <- call_object_accessor(obj, c("reducedDimNames"), default = character(0))
      col_data <- call_object_accessor(obj, c("colData"), default = NULL)

      paste0(
        "**SingleCellExperiment** (",
        if (all(!is.na(dims))) paste0(dims[1], " features x ", dims[2], " cells") else "dimensions unavailable",
        ")\n\n",
        "**Assays:** ", if (length(assay_names)) paste(assay_names, collapse = ", ") else "(none)", "\n",
        "**Reduced dims:** ", if (length(reduced_dims)) paste(reduced_dims, collapse = ", ") else "(none)", "\n",
        "**colData columns:** ", if (!is.null(col_data) && ncol(col_data) > 0) paste(colnames(col_data), collapse = ", ") else "(none)"
      )
    },
    render_inspection = function(obj, name = NULL, head_rows = 6) {
      dims <- tryCatch(dim(obj), error = function(e) c(NA_integer_, NA_integer_))
      assay_names <- call_object_accessor(obj, c("assayNames"), default = character(0))
      reduced_dims <- call_object_accessor(obj, c("reducedDimNames"), default = character(0))
      col_data <- call_object_accessor(obj, c("colData"), default = NULL)

      lines <- c(
        paste0("Variable: ", name %||% "<unnamed>"),
        paste0("Type: ", paste(class(obj), collapse = ", ")),
        paste0("Size: ", format(utils::object.size(obj), units = "auto")),
        ""
      )
      if (all(!is.na(dims))) {
        lines <- c(lines, paste0("Dimensions: ", dims[1], " features x ", dims[2], " cells"))
      }
      lines <- c(lines, paste0("Assays: ", if (length(assay_names)) paste(assay_names, collapse = ", ") else "(none)"))
      lines <- c(lines, paste0("Reduced dims: ", if (length(reduced_dims)) paste(reduced_dims, collapse = ", ") else "(none)"))
      lines <- c(lines, paste0("colData columns: ", if (!is.null(col_data) && ncol(col_data) > 0) paste(colnames(col_data), collapse = ", ") else "(none)"))
      if (!is.null(col_data) && nrow(col_data) > 0) {
        lines <- c(lines, "", "colData preview:")
        lines <- c(lines, utils::capture.output(print(utils::head(as.data.frame(col_data), head_rows))))
      }
      paste(lines, collapse = "\n")
    }
  )
}

#' @keywords internal
create_granges_semantic_adapter <- function() {
  create_semantic_adapter(
    name = "granges",
    supports = function(obj) is_semantic_class(obj, "GRanges"),
    capabilities = c("identity", "schema", "semantics", "genomic_ranges", "preview", "budget_estimate", "safety_checks"),
    priority = 250,
    describe_identity = function(obj) {
      list(
        class = class(obj),
        primary_class = "GRanges",
        length = tryCatch(length(obj), error = function(e) NA_integer_),
        seqlevels = call_object_accessor(obj, c("seqlevels"), default = character(0))
      )
    },
    describe_schema = function(obj) {
      mcols_df <- call_object_accessor(obj, c("mcols"), default = NULL)
      list(
        kind = "GRanges",
        metadata_columns = colnames(mcols_df) %||% character(0),
        seqlevels = call_object_accessor(obj, c("seqlevels"), default = character(0))
      )
    },
    describe_semantics = function(obj) {
      list(summary = "Genomic interval container with seqnames, ranges, strand, and metadata columns.")
    },
    list_accessors = function(obj) {
      c("seqnames()", "ranges()", "strand()", "mcols()", "seqlevels()")
    },
    estimate_cost = function(obj) {
      n <- tryCatch(length(obj), error = function(e) NA_integer_)
      list(
        tokens = "low",
        compute = if (!is.na(n) && n > 1e5) "medium" else "low",
        io = "none"
      )
    },
    provenance = function(obj) {
      list(adapter = "granges", package = "aisdk.bioc")
    },
    validate_action = function(obj, action) {
      if (identical(action, "materialize_ranges_table")) {
        return(list(
          status = "warn",
          reason = "Coercing the full GRanges object to a dense table is often avoidable; inspect seqlevels or metadata first.",
          category = "materialization",
          expensive = TRUE
        ))
      }
      list(status = "allow", reason = "GRanges adapter is read-oriented.", category = "read", expensive = FALSE)
    },
    render_summary = function(obj, name = NULL) {
      n <- tryCatch(length(obj), error = function(e) NA_integer_)
      seqlevels <- call_object_accessor(obj, c("seqlevels"), default = character(0))
      mcols_df <- call_object_accessor(obj, c("mcols"), default = NULL)

      paste0(
        "**GRanges** (",
        if (!is.na(n)) paste0(n, " ranges") else "length unavailable",
        ")\n\n",
        "**seqlevels:** ", if (length(seqlevels)) paste(head(seqlevels, 10), collapse = ", ") else "(none)", "\n",
        "**metadata columns:** ", if (!is.null(mcols_df) && ncol(mcols_df) > 0) paste(colnames(mcols_df), collapse = ", ") else "(none)"
      )
    },
    render_inspection = function(obj, name = NULL, head_rows = 6) {
      n <- tryCatch(length(obj), error = function(e) NA_integer_)
      seqlevels <- call_object_accessor(obj, c("seqlevels"), default = character(0))
      mcols_df <- call_object_accessor(obj, c("mcols"), default = NULL)
      seqnames_vals <- call_object_accessor(obj, c("seqnames"), default = NULL)
      strand_vals <- call_object_accessor(obj, c("strand"), default = NULL)

      lines <- c(
        paste0("Variable: ", name %||% "<unnamed>"),
        paste0("Type: ", paste(class(obj), collapse = ", ")),
        paste0("Size: ", format(utils::object.size(obj), units = "auto")),
        ""
      )
      if (!is.na(n)) {
        lines <- c(lines, paste0("Length: ", n))
      }
      lines <- c(lines, paste0("seqlevels: ", if (length(seqlevels)) paste(head(seqlevels, 10), collapse = ", ") else "(none)"))
      lines <- c(lines, paste0("metadata columns: ", if (!is.null(mcols_df) && ncol(mcols_df) > 0) paste(colnames(mcols_df), collapse = ", ") else "(none)"))
      if (!is.null(seqnames_vals)) {
        lines <- c(lines, paste0("seqnames preview: ", as_preview_text(as.character(seqnames_vals), max_items = 10) %||% "(none)"))
      }
      if (!is.null(strand_vals)) {
        lines <- c(lines, paste0("strand preview: ", as_preview_text(as.character(strand_vals), max_items = 10) %||% "(none)"))
      }
      paste(lines, collapse = "\n")
    }
  )
}
