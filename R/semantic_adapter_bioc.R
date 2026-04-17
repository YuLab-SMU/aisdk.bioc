#' @title Bioc Semantic Adapter Boundary
#' @description Built-in Bioconductor-oriented semantic adapters and default
#' workflow hints. This file is the domain layer boundary that can later be
#' split into a separate extension package such as `aisdk.bioc`.
#' @name semantic_adapter_bioc
NULL

#' @keywords internal
available_bioc_packages <- function() {
  pkgs <- c("SummarizedExperiment", "SingleCellExperiment", "GenomicRanges", "DESeq2", "DelayedArray", "HDF5Array")
  available <- vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
  pkgs[available]
}

#' @keywords internal
is_bioc_package_available <- function(pkg) {
  if (!nzchar(pkg)) return(FALSE)
  requireNamespace(pkg, quietly = TRUE)
}

#' @keywords internal
format_formula_text <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  paste(deparse(x, width.cutoff = 500L), collapse = "")
}

#' @keywords internal
has_non_missing_values <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(FALSE)
  }
  any(!is.na(x))
}

#' @keywords internal
safe_slot_value <- function(x, slot_name, default = NULL) {
  if (is.null(x)) {
    return(default)
  }
  if (!isS4(x)) {
    return(default)
  }
  if (!(slot_name %in% methods::slotNames(x))) {
    return(default)
  }
  tryCatch(methods::slot(x, slot_name), error = function(e) default)
}

#' @keywords internal
delayed_seed <- function(obj) {
  call_object_accessor(obj, c("seed"), default = NULL, package = "DelayedArray")
}

#' @keywords internal
delayed_seed_class <- function(obj) {
  seed_obj <- delayed_seed(obj)
  if (is.null(seed_obj)) {
    return(character(0))
  }
  class(seed_obj)
}

#' @keywords internal
delayed_showtree_preview <- function(obj) {
  if (!requireNamespace("DelayedArray", quietly = TRUE)) {
    return(NULL)
  }
  tryCatch(
    paste(utils::capture.output(DelayedArray::showtree(obj)), collapse = "\n"),
    error = function(e) NULL
  )
}

#' @keywords internal
normalize_optional_path <- function(path) {
  if (is.null(path) || !nzchar(path)) {
    return(path)
  }
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

#' @keywords internal
register_default_bioc_semantic_components <- function(registry, include_workflow_hints = TRUE) {
  registry$register(create_single_cell_experiment_semantic_adapter())
  registry$register(create_summarized_experiment_semantic_adapter())
  registry$register(create_deseq_dataset_semantic_adapter())
  registry$register(create_delayed_array_semantic_adapter())
  registry$register(create_hdf5_array_semantic_adapter())
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

    registry$register_workflow_hint(
      name = "deseq-dataset-default",
      supports = function(obj) is_semantic_class(obj, "DESeqDataSet"),
      priority = 280,
      hint_fn = function(obj, goal = NULL) {
        list(
          workflow = "deseq_dataset_default",
          goal = goal,
          steps = c(
            "inspect counts, sample metadata, and design formula",
            "confirm contrasts and coefficient names before modeling",
            "run normalization and model fitting when appropriate",
            "inspect differential expression results and summarize findings"
          )
        )
      }
    )

    registry$register_workflow_hint(
      name = "delayed-array-default",
      supports = function(obj) is_semantic_class(obj, "DelayedArray"),
      priority = 160,
      hint_fn = function(obj, goal = NULL) {
        list(
          workflow = "delayed_array_default",
          goal = goal,
          steps = c(
            "inspect dimensions and delayed seed backend",
            "prefer summaries or block-aware operations over full materialization",
            "only materialize explicit slices or reduced summaries when necessary",
            "return to the parent container workflow once the backend risk is understood"
          )
        )
      }
    )

    registry$register_workflow_hint(
      name = "hdf5-array-default",
      supports = function(obj) is_semantic_class(obj, "HDF5Matrix"),
      priority = 170,
      hint_fn = function(obj, goal = NULL) {
        list(
          workflow = "hdf5_array_default",
          goal = goal,
          steps = c(
            "inspect dimensions, file path, and chunk geometry",
            "avoid converting the full disk-backed array into an in-memory matrix",
            "prefer backend-aware summaries or focused subsets",
            "only materialize data that is required for the immediate analysis step"
          )
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
create_deseq_dataset_semantic_adapter <- function() {
  create_semantic_adapter(
    name = "deseq-dataset",
    supports = function(obj) is_semantic_class(obj, "DESeqDataSet"),
    capabilities = c("identity", "schema", "semantics", "metadata", "preview", "budget_estimate", "safety_checks"),
    priority = 280,
    describe_identity = function(obj) {
      se_identity <- create_summarized_experiment_semantic_adapter()$describe_identity(obj)
      design_formula <- call_object_accessor(obj, c("design"), default = NULL, package = "DESeq2")
      size_factors <- call_object_accessor(obj, c("sizeFactors"), default = NULL, package = "DESeq2")
      dispersions <- call_object_accessor(obj, c("dispersions"), default = NULL, package = "DESeq2")

      se_identity$primary_class <- "DESeqDataSet"
      se_identity$design <- format_formula_text(design_formula)
      se_identity$has_size_factors <- has_non_missing_values(size_factors)
      se_identity$has_dispersions <- has_non_missing_values(dispersions)
      se_identity$results_names <- call_object_accessor(obj, c("resultsNames"), default = character(0), package = "DESeq2")
      se_identity
    },
    describe_schema = function(obj) {
      schema <- create_summarized_experiment_semantic_adapter()$describe_schema(obj)
      design_formula <- call_object_accessor(obj, c("design"), default = NULL, package = "DESeq2")
      schema$kind <- "DESeqDataSet"
      schema$design <- format_formula_text(design_formula)
      schema$design_variables <- all.vars(design_formula) %||% character(0)
      schema$results_names <- call_object_accessor(obj, c("resultsNames"), default = character(0), package = "DESeq2")
      schema
    },
    describe_semantics = function(obj) {
      list(summary = "DESeq2 differential expression dataset with count assays, sample metadata, and an explicit design formula.")
    },
    list_accessors = function(obj) {
      c("assayNames()", "assays()", "rowData()", "colData()", "metadata()", "design()", "sizeFactors()", "dispersions()", "resultsNames()")
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
      list(adapter = "deseq-dataset", package = "aisdk.bioc")
    },
    validate_action = function(obj, action) {
      if (identical(action, "materialize_counts")) {
        return(list(
          status = "warn",
          reason = "Materializing the full count matrix is often unnecessary; inspect design, metadata, and assay summaries first.",
          category = "materialization",
          expensive = TRUE
        ))
      }
      if (identical(action, "materialize_all_assays")) {
        return(list(
          status = "warn",
          reason = "Materializing all assays in a DESeqDataSet may be expensive; prefer summary-level inspection first.",
          category = "materialization",
          expensive = TRUE
        ))
      }
      list(status = "allow", reason = "Safe read-oriented operation.", category = "read", expensive = FALSE)
    },
    render_summary = function(obj, name = NULL) {
      identity <- create_deseq_dataset_semantic_adapter()$describe_identity(obj)
      schema <- create_deseq_dataset_semantic_adapter()$describe_schema(obj)

      paste0(
        "**DESeqDataSet** (",
        if (!is.null(identity$dimensions) && all(!is.na(identity$dimensions))) {
          paste0(identity$dimensions[1], " features x ", identity$dimensions[2], " samples")
        } else {
          "dimensions unavailable"
        },
        ")\n\n",
        "**Assays:** ", if (length(identity$assays)) paste(identity$assays, collapse = ", ") else "(none)", "\n",
        "**Design:** ", identity$design %||% "(none)", "\n",
        "**Design variables:** ", if (length(schema$design_variables)) paste(schema$design_variables, collapse = ", ") else "(none)", "\n",
        "**Results names:** ", if (length(identity$results_names)) paste(identity$results_names, collapse = ", ") else "(none)"
      )
    },
    render_inspection = function(obj, name = NULL, head_rows = 6) {
      identity <- create_deseq_dataset_semantic_adapter()$describe_identity(obj)
      schema <- create_deseq_dataset_semantic_adapter()$describe_schema(obj)
      col_data <- call_object_accessor(obj, c("colData"), default = NULL)

      lines <- c(
        paste0("Variable: ", name %||% "<unnamed>"),
        paste0("Type: ", paste(class(obj), collapse = ", ")),
        paste0("Size: ", format(utils::object.size(obj), units = "auto")),
        ""
      )
      if (!is.null(identity$dimensions) && all(!is.na(identity$dimensions))) {
        lines <- c(lines, paste0("Dimensions: ", identity$dimensions[1], " features x ", identity$dimensions[2], " samples"))
      }
      lines <- c(lines, paste0("Assays: ", if (length(identity$assays)) paste(identity$assays, collapse = ", ") else "(none)"))
      lines <- c(lines, paste0("Design: ", identity$design %||% "(none)"))
      lines <- c(lines, paste0("Design variables: ", if (length(schema$design_variables)) paste(schema$design_variables, collapse = ", ") else "(none)"))
      lines <- c(lines, paste0("Results names: ", if (length(identity$results_names)) paste(identity$results_names, collapse = ", ") else "(none)"))
      lines <- c(lines, paste0("Size factors present: ", if (isTRUE(identity$has_size_factors)) "yes" else "no"))
      lines <- c(lines, paste0("Dispersions present: ", if (isTRUE(identity$has_dispersions)) "yes" else "no"))
      if (!is.null(col_data) && nrow(col_data) > 0) {
        lines <- c(lines, "", "colData preview:")
        lines <- c(lines, utils::capture.output(print(utils::head(as.data.frame(col_data), head_rows))))
      }
      paste(lines, collapse = "\n")
    }
  )
}

#' @keywords internal
create_delayed_array_semantic_adapter <- function() {
  create_semantic_adapter(
    name = "delayed-array",
    supports = function(obj) is_semantic_class(obj, "DelayedArray"),
    capabilities = c("identity", "schema", "semantics", "preview", "budget_estimate", "safety_checks"),
    priority = 160,
    describe_identity = function(obj) {
      dims <- tryCatch(dim(obj), error = function(e) NULL)
      list(
        class = class(obj),
        primary_class = "DelayedArray",
        dimensions = if (is.null(dims)) NULL else as.integer(dims),
        seed_class = delayed_seed_class(obj),
        is_sparse = call_object_accessor(obj, c("is_sparse"), default = FALSE, package = "DelayedArray")
      )
    },
    describe_schema = function(obj) {
      seed_obj <- delayed_seed(obj)
      showtree <- delayed_showtree_preview(obj)
      list(
        kind = "DelayedArray",
        seed_class = class(seed_obj) %||% character(0),
        seed_dimensions = tryCatch(dim(seed_obj), error = function(e) NULL),
        operation_tree = if (is.null(showtree)) NULL else strsplit(showtree, "\n", fixed = TRUE)[[1]]
      )
    },
    describe_semantics = function(obj) {
      list(summary = "DelayedArray-backed matrix or array with deferred operations that may be expensive to fully materialize.")
    },
    list_accessors = function(obj) {
      c("DelayedArray::seed()", "DelayedArray::showtree()", "dim()", "dimnames()")
    },
    estimate_cost = function(obj) {
      dims <- tryCatch(dim(obj), error = function(e) c(NA_integer_, NA_integer_))
      size_hint <- if (all(!is.na(dims))) prod(dims) else NA_real_
      list(
        tokens = "low",
        compute = if (!is.na(size_hint) && size_hint > 1e6) "medium" else "low",
        io = if (!is.na(size_hint) && size_hint > 1e6) "medium" else "low"
      )
    },
    provenance = function(obj) {
      list(adapter = "delayed-array", package = "aisdk.bioc")
    },
    validate_action = function(obj, action) {
      if (action %in% c("materialize_full_array", "materialize_as_matrix", "materialize_counts")) {
        return(list(
          status = "warn",
          reason = "DelayedArray objects may represent deferred or disk-backed computations; prefer summaries or focused subsets before full materialization.",
          category = "materialization",
          expensive = TRUE
        ))
      }
      list(status = "allow", reason = "Safe read-oriented operation.", category = "read", expensive = FALSE)
    },
    render_summary = function(obj, name = NULL) {
      identity <- create_delayed_array_semantic_adapter()$describe_identity(obj)
      paste0(
        "**DelayedArray** (",
        if (!is.null(identity$dimensions) && all(!is.na(identity$dimensions))) {
          paste(identity$dimensions, collapse = " x ")
        } else {
          "dimensions unavailable"
        },
        ")\n\n",
        "**Seed class:** ", if (length(identity$seed_class)) paste(identity$seed_class, collapse = ", ") else "(none)", "\n",
        "**Sparse hint:** ", if (isTRUE(identity$is_sparse)) "yes" else "no"
      )
    },
    render_inspection = function(obj, name = NULL, head_rows = 6) {
      identity <- create_delayed_array_semantic_adapter()$describe_identity(obj)
      schema <- create_delayed_array_semantic_adapter()$describe_schema(obj)

      lines <- c(
        paste0("Variable: ", name %||% "<unnamed>"),
        paste0("Type: ", paste(class(obj), collapse = ", ")),
        paste0("Size: ", format(utils::object.size(obj), units = "auto")),
        ""
      )
      if (!is.null(identity$dimensions) && all(!is.na(identity$dimensions))) {
        lines <- c(lines, paste0("Dimensions: ", paste(identity$dimensions, collapse = " x ")))
      }
      lines <- c(lines, paste0("Seed class: ", if (length(identity$seed_class)) paste(identity$seed_class, collapse = ", ") else "(none)"))
      lines <- c(lines, paste0("Sparse hint: ", if (isTRUE(identity$is_sparse)) "yes" else "no"))
      if (!is.null(schema$operation_tree) && length(schema$operation_tree)) {
        lines <- c(lines, "", "Delayed operation tree:")
        lines <- c(lines, schema$operation_tree)
      }
      paste(lines, collapse = "\n")
    }
  )
}

#' @keywords internal
create_hdf5_array_semantic_adapter <- function() {
  create_semantic_adapter(
    name = "hdf5-array",
    supports = function(obj) is_semantic_class(obj, "HDF5Matrix"),
    capabilities = c("identity", "schema", "semantics", "preview", "budget_estimate", "safety_checks"),
    priority = 170,
    describe_identity = function(obj) {
      dims <- tryCatch(dim(obj), error = function(e) NULL)
      seed_obj <- delayed_seed(obj)
      list(
        class = class(obj),
        primary_class = "HDF5Array",
        dimensions = if (is.null(dims)) NULL else as.integer(dims),
        seed_class = class(seed_obj) %||% character(0),
        filepath = normalize_optional_path(safe_slot_value(seed_obj, "filepath")),
        dataset_name = safe_slot_value(seed_obj, "name")
      )
    },
    describe_schema = function(obj) {
      seed_obj <- delayed_seed(obj)
      list(
        kind = "HDF5Array",
        seed_class = class(seed_obj) %||% character(0),
        filepath = normalize_optional_path(safe_slot_value(seed_obj, "filepath")),
        dataset_name = safe_slot_value(seed_obj, "name"),
        chunkdim = safe_slot_value(seed_obj, "chunkdim"),
        seed_dimensions = safe_slot_value(seed_obj, "dim")
      )
    },
    describe_semantics = function(obj) {
      list(summary = "HDF5-backed delayed array that stores data on disk and should be inspected with backend-aware caution.")
    },
    list_accessors = function(obj) {
      c("DelayedArray::seed()", "DelayedArray::showtree()", "dim()", "dimnames()")
    },
    estimate_cost = function(obj) {
      dims <- tryCatch(dim(obj), error = function(e) c(NA_integer_, NA_integer_))
      size_hint <- if (all(!is.na(dims))) prod(dims) else NA_real_
      list(
        tokens = "low",
        compute = if (!is.na(size_hint) && size_hint > 1e6) "medium" else "low",
        io = if (!is.na(size_hint) && size_hint > 1e6) "high" else "medium"
      )
    },
    provenance = function(obj) {
      list(adapter = "hdf5-array", package = "aisdk.bioc")
    },
    validate_action = function(obj, action) {
      if (action %in% c("materialize_full_array", "materialize_as_matrix", "materialize_counts")) {
        return(list(
          status = "warn",
          reason = "HDF5-backed arrays live on disk; full materialization can be expensive in both I/O and memory. Prefer summaries or targeted subsets.",
          category = "materialization",
          expensive = TRUE
        ))
      }
      list(status = "allow", reason = "Safe read-oriented operation.", category = "read", expensive = FALSE)
    },
    render_summary = function(obj, name = NULL) {
      identity <- create_hdf5_array_semantic_adapter()$describe_identity(obj)
      paste0(
        "**HDF5Array** (",
        if (!is.null(identity$dimensions) && all(!is.na(identity$dimensions))) {
          paste(identity$dimensions, collapse = " x ")
        } else {
          "dimensions unavailable"
        },
        ")\n\n",
        "**Seed class:** ", if (length(identity$seed_class)) paste(identity$seed_class, collapse = ", ") else "(none)", "\n",
        "**File:** ", identity$filepath %||% "(unknown)", "\n",
        "**Dataset:** ", identity$dataset_name %||% "(unknown)"
      )
    },
    render_inspection = function(obj, name = NULL, head_rows = 6) {
      identity <- create_hdf5_array_semantic_adapter()$describe_identity(obj)
      schema <- create_hdf5_array_semantic_adapter()$describe_schema(obj)
      showtree <- delayed_showtree_preview(obj)

      lines <- c(
        paste0("Variable: ", name %||% "<unnamed>"),
        paste0("Type: ", paste(class(obj), collapse = ", ")),
        paste0("Size: ", format(utils::object.size(obj), units = "auto")),
        ""
      )
      if (!is.null(identity$dimensions) && all(!is.na(identity$dimensions))) {
        lines <- c(lines, paste0("Dimensions: ", paste(identity$dimensions, collapse = " x ")))
      }
      lines <- c(lines, paste0("Seed class: ", if (length(identity$seed_class)) paste(identity$seed_class, collapse = ", ") else "(none)"))
      lines <- c(lines, paste0("File: ", identity$filepath %||% "(unknown)"))
      lines <- c(lines, paste0("Dataset: ", identity$dataset_name %||% "(unknown)"))
      lines <- c(lines, paste0("Chunk geometry: ", if (!is.null(schema$chunkdim)) paste(schema$chunkdim, collapse = " x ") else "(unknown)"))
      if (!is.null(showtree) && nzchar(showtree)) {
        lines <- c(lines, "", "Delayed operation tree:")
        lines <- c(lines, strsplit(showtree, "\n", fixed = TRUE)[[1]])
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
