# Template: Custom Semantic Adapter
# Use this as a starting point for lab-specific or package-specific objects.

library(aisdk)

# ---------------------------------------------------------------------------
# 1. Object definition (replace with your actual class)
# ---------------------------------------------------------------------------
my_custom_object <- structure(
  list(values = 1:3, labels = c("a", "b", "c")),
  class = "MyCustomObject"
)

# ---------------------------------------------------------------------------
# 2. Adapter definition
# ---------------------------------------------------------------------------
my_custom_adapter <- aisdk::create_semantic_adapter(
  name = "my-custom-object",
  supports = function(x) inherits(x, "MyCustomObject"),
  capabilities = c("identity", "schema", "semantics", "budget_estimate", "safety_checks"),

  describe_identity = function(x) {
    list(
      primary_class = "MyCustomObject",
      length = length(x$values)
    )
  },

  describe_schema = function(x) {
    list(fields = names(x))
  },

  describe_semantics = function(x) {
    list(summary = "A labeled vector used in our custom pipeline.")
  },

  estimate_cost = function(x) {
    list(tokens = "low", compute = "low", io = "none")
  },

  validate_action = function(x, action) {
    if (identical(action, "expensive_conversion")) {
      return(list(
        status = "warn",
        reason = "This conversion may be expensive for large objects.",
        category = "materialization",
        expensive = TRUE
      ))
    }
    list(status = "allow", reason = "Safe operation.", category = "read", expensive = FALSE)
  },

  provenance = function(x) {
    list(adapter = "my-custom-object", package = "mylab")
  }
)

# ---------------------------------------------------------------------------
# 3. Registration
# ---------------------------------------------------------------------------
registry <- aisdk::create_semantic_adapter_registry()
registry$register(my_custom_adapter)

# Verify resolution
stopifnot("my-custom-object" %in% registry$list_adapters())
resolved <- registry$resolve(my_custom_object)
stopifnot(resolved$name == "my-custom-object")
