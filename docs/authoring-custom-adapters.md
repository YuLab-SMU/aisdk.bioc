# Authoring Custom Semantic Adapters

This guide explains how to write a custom semantic adapter for your own R objects, using only the public `aisdk` API.

## When to Write a Custom Adapter

Write a custom adapter when:

- Your lab or package defines custom S3, S4, R6, or plain list-based objects.
- You want AI assistants to understand these objects without relying on generic `str()` summaries.
- You need structured identity, schema, semantics, cost estimates, or safety checks for a class.

You do **not** need a custom adapter for plain data frames or base R vectors—`aisdk` already provides reasonable generic handling for those.

## Minimal Example

```r
library(aisdk)

# 1. Define your object
my_obj <- structure(
  list(values = 1:3, labels = c("a", "b", "c")),
  class = "MyLabObject"
)

# 2. Create an adapter using only public aisdk APIs
adapter <- aisdk::create_semantic_adapter(
  name = "my-lab-object",
  supports = function(x) inherits(x, "MyLabObject"),
  capabilities = c("identity", "schema", "semantics"),
  describe_identity = function(x) {
    list(
      primary_class = "MyLabObject",
      length = length(x$values)
    )
  },
  describe_schema = function(x) {
    list(fields = names(x))
  },
  describe_semantics = function(x) {
    list(summary = "A small labeled vector used in our lab pipeline.")
  }
)

# 3. Register it in a session registry
registry <- aisdk::create_semantic_adapter_registry()
registry$register(adapter)

# 4. Verify it resolves
registry$get_adapter(my_obj)
```

## Required vs Optional Fields

### Required adapter arguments

- `name`: A stable, kebab-case identifier.
- `supports`: A predicate function `function(x)` returning `TRUE` or `FALSE`.
- `capabilities`: A character vector of declared capabilities.
- `describe_identity(obj)`: Returns a list with at least `primary_class`.

### Optional but recommended

- `describe_schema(obj)`: Returns structural metadata (slots, fields, assay names, etc.).
- `describe_semantics(obj)`: Returns a human-readable domain summary.
- `estimate_cost(obj)`: Returns a list with `tokens`, `compute`, and `io` levels (`"low"`, `"medium"`, `"high"`).
- `validate_action(obj, action)`: Returns a list with `status` (`"allow"`, `"warn"`, or `"deny"`) and `reason`.
- `provenance(obj)`: Returns `list(adapter = "name", package = "yourpkg")`.
- `render_summary(obj, name = NULL)`: Returns a short Markdown string.
- `render_inspection(obj, name = NULL, head_rows = 6)`: Returns a longer plain-text inspection.

## Test Checklist

Before distributing a custom adapter, verify:

1. `supports()` correctly matches the target class and rejects unrelated objects.
2. Every declared capability returns a well-formed result.
3. `validate_action()` intercepts at least one expensive or dangerous operation.
4. `provenance()` correctly identifies your package.
5. The adapter registers cleanly in `aisdk::create_semantic_adapter_registry()`.

## Anti-Patterns

Do **not**:

- Use `getFromNamespace()` to reach into `aisdk` internals. The public API is sufficient.
- Force full-object materialization in `describe_identity()` or `describe_schema()`. Keep these cheap.
- Fake provenance by claiming `package = "aisdk"` or another package you do not own.
- Over-promise planner semantics. Workflow hints should remain advisory, not executable plans.
