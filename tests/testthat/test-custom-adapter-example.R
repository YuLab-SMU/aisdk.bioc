test_that("custom adapter example works with public aisdk api", {
  obj <- structure(list(values = 1:3), class = "MyLabObject")
  registry <- aisdk::create_semantic_adapter_registry()
  adapter <- aisdk::create_semantic_adapter(
    name = "my-lab-object",
    supports = function(x) inherits(x, "MyLabObject"),
    capabilities = c("identity", "schema", "semantics"),
    describe_identity = function(x) list(primary_class = "MyLabObject"),
    describe_schema = function(x) list(fields = names(x)),
    describe_semantics = function(x) list(summary = "Lab-specific object")
  )
  registry$register(adapter)
  expect_true("my-lab-object" %in% registry$list_adapters())

  resolved <- registry$resolve(obj)
  expect_identical(resolved$name, "my-lab-object")
  expect_identical(resolved$describe_identity(obj)$primary_class, "MyLabObject")
})
