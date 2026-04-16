# aisdk.bioc

`aisdk.bioc` is the Bioconductor extension package for `aisdk`.

Need the plain-language overview first?

- 中文说明: [docs/understand-aisdk-bioc.md](docs/understand-aisdk-bioc.md)
- Minimal framework diagram: [docs/diagrams/aisdk-bioc-framework-minimal.png](docs/diagrams/aisdk-bioc-framework-minimal.png)
- Custom adapter guide: [docs/authoring-custom-adapters.md](docs/authoring-custom-adapters.md)

It owns:

- Bioconductor semantic adapters
- workflow hints for Bioconductor object families
- Bioconductor-specific tests and documentation

The `aisdk` core package remains responsible for the runtime, session, tool,
provider, and generic semantic adapter protocol layers.
