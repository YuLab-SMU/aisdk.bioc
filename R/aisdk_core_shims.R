#' Register Bioconductor semantic components into an aisdk registry
#'
#' Public entrypoint for attaching the Bioconductor semantic adapters and
#' workflow hints owned by `aisdk.bioc`.
#'
#' @param registry An adapter registry created by `aisdk`.
#' @param include_workflow_hints Logical; register default workflow hints when
#'   `TRUE`.
#' @return The updated registry.
#' @export
register_bioc_semantic_components <- function(registry, include_workflow_hints = TRUE) {
  register_default_bioc_semantic_components(
    registry = registry,
    include_workflow_hints = include_workflow_hints
  )
}

#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' @keywords internal
create_semantic_adapter <- function(...) {
  getFromNamespace("create_semantic_adapter", "aisdk")(...)
}

#' @keywords internal
call_object_accessor <- function(...) {
  getFromNamespace("call_object_accessor", "aisdk")(...)
}

#' @keywords internal
is_semantic_class <- function(...) {
  getFromNamespace("is_semantic_class", "aisdk")(...)
}

#' @keywords internal
as_preview_text <- function(...) {
  getFromNamespace("as_preview_text", "aisdk")(...)
}
