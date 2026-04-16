# 理解 `aisdk.bioc`

## 一句话

`aisdk.bioc` 的作用，是让 `aisdk` 在遇到 Bioconductor 里的复杂对象时，先“看懂对象”，再决定怎么和它协作。

它不是单纯帮 AI 写几段生信代码，而是在 `aisdk` 和 Bioconductor 对象之间，加了一层面向 AI 的语义翻译。

## 它到底在做什么

如果没有这层翻译，AI 看到一个 `SingleCellExperiment` 或 `SummarizedExperiment`，很容易只知道“这是个 S4 对象”，但不知道：

- 这个对象里有哪些 assay
- `rowData()` 和 `colData()` 里分别放了什么
- 有没有 `reducedDims()`
- 这个对象大不大，能不能直接全量读出来
- 常见的下一步分析路径是什么

`aisdk.bioc` 做的事情，就是把这些信息整理成 AI 更容易使用的格式，比如：

- 对象身份：这是什么类
- 结构摘要：里面有哪些重要组成部分
- 领域语义：这个对象在生信里通常代表什么
- 安全提醒：哪些操作可能很贵、很慢、或者没必要
- workflow hints：这类对象常见的下一步分析步骤

## 可以把它理解成什么

可以把 `aisdk.bioc` 理解成一个“生信对象翻译器”。

`aisdk` 负责通用运行时，比如 registry、session、工具调用和语义适配协议；`aisdk.bioc` 负责把 Bioconductor 世界里的对象知识挂接进去。

所以它更像是在回答：

> 当 AI 在 live R session 里碰到一个复杂生信对象时，怎样先看懂它，而不是乱猜它？

## “翻译”是怎么发生的

过程可以粗略理解成 4 步：

1. `aisdk.bioc` 把自己的 Bioconductor adapters 注册到 `aisdk` 的 registry 里。
2. 当 AI 遇到某个对象时，registry 找到最匹配的 adapter。
3. adapter 提供这个对象的身份、结构、语义、预算和安全信息。
4. 如果适合，这个 adapter 还会给出默认 workflow hints，告诉 AI 这类对象通常下一步该怎么做。

这个包当前主要覆盖：

- `SingleCellExperiment`
- `SummarizedExperiment`
- `GRanges`

## 一个具体例子

假设当前 R session 里有一个对象：

```r
sce
```

它的类是 `SingleCellExperiment`。

如果没有 `aisdk.bioc`，AI 可能会像这样想：

- “这是个复杂对象，我猜里面可能有 counts”
- “我试试直接把整个矩阵转成普通矩阵”
- “我再猜一下元数据字段名”

这很容易出错，也很容易做出代价很高的操作。

有了 `aisdk.bioc` 之后，AI 拿到的更像是一张对象说明卡：

- 这是 `SingleCellExperiment`
- 维度大概是多少
- assays 有哪些，比如 `counts`、`logcounts`
- reduced dims 有哪些，比如 `PCA`、`UMAP`
- `colData()` 里有哪些字段
- 如果直接把整个 count matrix 全量读出来，可能会很贵
- 这类对象的默认分析顺序通常是：先看 metadata，再做 QC、归一化、降维、聚类注释

也就是说，AI 会更倾向于先“检查和预览”，而不是直接“大力出奇迹”。

## 最小可运行示例

下面这段代码展示了这个包最直接的用途：把 Bioconductor 语义组件注册进 `aisdk` 的 registry。

```r
library(aisdk)
library(aisdk.bioc)

# 如果你的 aisdk 版本较新，会提供 semantic adapter registry
registry <- aisdk::create_semantic_adapter_registry()

# 注册 aisdk.bioc 提供的 Bioconductor adapters 和 workflow hints
register_bioc_semantic_components(registry, include_workflow_hints = TRUE)

# 看看现在 registry 里有哪些 Bioconductor adapters
registry$list_adapters()
#> [1] "single-cell-experiment" "summarized-experiment" "granges"

# 看看默认 workflow hints
registry$list_workflow_hints()
#> [1] "single-cell-experiment-default"
#> [2] "summarized-experiment-default"
#> [3] "granges-default"
```

这段代码本身不会跑分析。它做的是“接线”工作：把 Bioconductor 这套对象理解能力接到 `aisdk` 上。

## 一个更直观的“示意输出”

下面不是当前包里承诺的公开用户 API，而是一个接近真实 adapter 输出的示意，目的是帮助理解“翻译”到底翻成什么。

如果一个对象是 `SingleCellExperiment`，adapter 可能会整理出类似这样的信息：

```r
list(
  identity = list(
    primary_class = "SingleCellExperiment",
    dimensions = c(20000L, 5000L),
    assays = c("counts", "logcounts"),
    reduced_dims = c("PCA", "UMAP")
  ),
  semantics = list(
    summary = "Single-cell annotated matrix with assays, cell metadata, feature metadata, and reduced dimensions."
  ),
  safety = list(
    status = "warn",
    reason = "Materializing the full count matrix may be expensive; prefer previews or slices."
  ),
  workflow_hint = list(
    workflow = "single_cell_default",
    steps = c(
      "inspect assays and cell metadata",
      "quality control",
      "normalization and reduced dimensions",
      "cluster annotation",
      "report"
    )
  )
)
```

用户真正关心的点其实很简单：

- AI 知道这是什么对象
- AI 知道先看什么
- AI 知道哪些操作不要一上来就做

## 这个包不做什么

为了避免误解，这个包不等于：

- 一个完整的单细胞分析框架
- 一个自动生成所有生信流程的 agent
- 一个替代 `aisdk` core runtime 的项目

它更像一个 domain extension，专门负责：

- 把 Bioconductor 对象知识接入 `aisdk`
- 减少 AI 对复杂对象的“幻觉”
- 让 AI 的下一步动作更像懂对象的人

## 适合什么时候用

当你希望 AI 在 live R session 中和这些对象协作时，这个包才真正有价值：

- `SingleCellExperiment`
- `SummarizedExperiment`
- `GRanges`

尤其是在下面这些场景里更有意义：

- 对象很复杂，不适合靠猜
- 对象可能很大，不适合直接全量 materialize
- 希望 AI 的下一步建议更贴近生信工作流

## 搭配示意图

如果你想先看结构图，再读文字说明，可以配合这张极简图一起看：

- [项目极简框架图](./diagrams/aisdk-bioc-framework-minimal.png)

## 总结

`aisdk.bioc` 的核心价值，不是让 AI “更会写生信代码”，而是先让 AI “更会看懂生信对象”。

先看懂，后协作；先预览，后决策；先知道边界，再做动作。这就是这个包最重要的事情。
