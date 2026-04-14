# Semantic Adapter Protocol Design Spec

## Purpose

`aisdk` 不应该把 Bioconductor 或其他 R 生态中的类一个个内置进 core。正确方向是让 core 提供一个严格的语义运行时底座，再让领域扩展包和实验室自定义包按协议接入对象语义。这个协议必须同时满足两件事：

1. 规则严格。对象语义、执行模式、预算、安全和 provenance 需要被显式建模，而不是隐藏在 prompt 里。
2. 生态开放。core 不需要知道未来会出现哪些类，只需要知道如何发现和调用 adapter。

本文定义 `SemanticAdapter` 协议和相关 runtime 约束，使 `aisdk` 能从“通用 R 对象摘要器”升级为“live-session semantic runtime”。

## Current Gaps

当前仓库已经有 live session 的雏形，但还不自洽。

1. `ChatSession$send()` 没有像 `send_stream()` 一样把 `session = self` 传入 `generate_text()`，导致非流式路径下工具和 sandbox 拿不到 live session 环境。见 [R/session.R](/Users/xiayh/Projects/aisdk/R/session.R#L90)。
2. `SharedSession` 额外维护了 `variable_scopes`，其 `global` scope 不是 `ChatSession$get_envir()` 返回的共享环境，导致 `set_var()/execute_code()` 和工具层读取的对象不一定来自同一个真相源。见 [R/shared_session.R](/Users/xiayh/Projects/aisdk/R/shared_session.R#L39)。
3. `Computer$execute_r_code()` 在独立 `callr` 进程中执行，环境是隔离的 `globalenv()`，不能访问或持久化 session 对象，因此不能当成 live session 执行路径。见 [R/computer.R](/Users/xiayh/Projects/aisdk/R/computer.R#L209)。
4. 当前对象摘要仍以 `str()` 为 fallback，无法提供 Bioconductor 或自定义对象的语义级理解。见 [R/context.R](/Users/xiayh/Projects/aisdk/R/context.R#L21) 和 [R/agent_library.R](/Users/xiayh/Projects/aisdk/R/agent_library.R#L281)。

这几个问题不修，后续所有 semantic layer 都会建立在不稳定的 runtime 上。

## Design Goals

1. `aisdk` core 不再内置类知识，而是内置 adapter 协议、调度、预算和安全。
2. 所有对象理解请求都走同一条协议化路径，而不是局部函数各自做摘要。
3. 支持 S3、S4、R6 和实验室自定义类。
4. 支持分层披露，避免大对象直接炸上下文或触发错误物化。
5. 允许下游扩展包提供 workflow hints，但 core 不绑定具体领域方法。

## Non-Goals

1. 不在 core 中穷举 Bioconductor、Seurat 或其他包的类支持。
2. 不把具体分析 workflow 写死在 core。
3. 不要求所有 adapter 一次性实现全部能力；允许渐进增强。
4. 不把 UI、console 或 channel orchestration 作为本协议的核心内容。

## Runtime Precondition Fixes

在引入 adapter 之前，先修 runtime。

### 1. Unify Session Truth

`ChatSession$get_envir()` 必须成为唯一 canonical session env。`SharedSession` 的 `variable_scopes$global` 应直接绑定到这一个环境，而不是额外维护平行状态。

要求：

- `session$get_envir()` 是所有对象持久化的唯一真相源。
- `SharedSession$set_var()`、`get_var()`、`execute_code()` 默认作用于 canonical env。
- 额外 scope 只能是 canonical env 之上的子环境视图，不能是另一套独立存储。

### 2. Forward Session Consistently

`ChatSession$send()` 必须和 `send_stream()` 一样，把 `session = self` 传给 `generate_text()`。

要求：

- 所有标准交互路径都能把 session env 传到工具调用。
- sandbox mode 在需要 live session 时，必须显式用 canonical env 作为 parent env。

### 3. Make Execution Modes Explicit

系统里至少存在三种执行模式，必须文档化和程序化。

- `session_exec`: 在 live session 中执行，可读写共享对象。
- `sandbox_exec`: 在隔离环境中执行，不持久化 session 对象。
- `global_exec`: 显式授权后打到用户 `globalenv()`。

任何工具或 agent 都必须声明自己运行在哪个模式下，避免把隔离执行误当 live session。

## Core Abstractions

### SemanticAdapter

一个 `SemanticAdapter` 表示“某类对象的语义适配器”。它不负责所有业务，只负责以受控、可审计的方式向 runtime 暴露对象身份、结构、语义、预算和安全边界。

### AdapterRegistry

一个 `AdapterRegistry` 维护按优先级排序的 adapters，并负责对对象请求做路由、记录 provenance、暴露能力和触发 fallback。

### Layered Disclosure Controller

该控制器负责 identity -> schema -> evidence -> slice 的分层披露流程。它根据预算和 adapter capabilities 决定允许看多深，并缓存先前结果。

### Workflow Hint Registry

该层将对象语义与 domain workflow 连接起来。对象 adapter 可选地注册 workflow hints，供 planner 或 mission 层调用。

## SemanticAdapter Interface

建议定义最小协议如下。

### Required

1. `supports(obj)`
   - 判断当前 adapter 是否支持该对象。
   - 支持按 `inherits()`、S4 `is()`、命名空间、类名或自定义 predicate 匹配。

2. `describe_identity(obj)`
   - 返回最小身份信息。
   - 例如类名、包名、维度、后端类型、是否 delayed、估计 materialization 风险。

3. `describe_schema(obj)`
   - 返回结构 schema。
   - 例如 slots、assay names、metadata 字段、reduced dim names、row/column 注释结构。

4. `describe_semantics(obj)`
   - 返回对象在领域中的含义。
   - 例如“带细胞元数据和嵌入结果的单细胞计数矩阵”。

5. `estimate_cost(obj, operation = NULL)`
   - 返回 token / compute / I/O 预算估计。
   - runtime 据此控制是否允许 deeper inspection。

6. `provenance(obj)`
   - 返回对象来源信息。
   - 例如 package/version、构建来源、文件来源、样本 ID、生成 pipeline。

7. `validate_action(obj, action)`
   - 对危险操作做前置判断。
   - 例如拒绝对 HDF5-backed assay 直接 `as.matrix()`。

### Optional Capabilities

1. `peek(obj, budget)`
   - 在预算内提供轻量 preview。

2. `slice(obj, spec, budget = NULL)`
   - 执行小范围切片，返回 subset 和切片 provenance。

3. `list_accessors(obj)`
   - 列出推荐 getter/accessor。

4. `workflow_hint(obj, goal = NULL)`
   - 提供面向 planner 的 workflow 建议。

5. `backend_info(obj)`
   - 返回 delayed backend、磁盘路径、块大小等信息。

## Capability Model

每个 adapter 需要显式声明能力集，而不是假设实现了所有功能。

建议能力字段包含：

- `identity`
- `schema`
- `semantics`
- `preview`
- `slice_rows`
- `slice_cols`
- `metadata`
- `reduced_dims`
- `genomic_ranges`
- `workflow_hint`
- `budget_estimate`
- `safety_checks`

调用方必须先问 capability，再发请求。没有某项能力时，runtime 应降级或 fallback，而不是猜。

## AdapterRegistry

Registry 负责以下行为：

1. 按优先级存放 adapters，允许实验室自定义 adapter 覆盖 generic adapter。
2. 对每次请求执行 dispatch，选中最合适的 adapter。
3. 将“哪个 adapter 回答了哪个问题”写入审计日志。
4. 当没有 adapter 命中时，返回 generic fallback，并记录缺失类供后续扩展。
5. 向 session 暴露已注册 adapter 集合，使多 session 共享同一 registry 成为可能。

建议 API：

- `create_adapter_registry()`
- `register_semantic_adapter(adapter, priority = 0)`
- `unregister_semantic_adapter(name)`
- `resolve_semantic_adapter(obj)`
- `list_registered_adapters()`
- `adapter_capabilities(obj)`

## Layered Disclosure

对象上下文不应一次性展开，而应按层次推进。

### Layer 1: Identity

最小必要信息：

- class
- package
- dimensions
- backend
- cost summary

### Layer 2: Schema

结构层信息：

- slots / components
- assay names
- metadata schema
- row/column annotation schema
- embedding names

### Layer 3: Evidence

预算内的轻量证据：

- representative stats
- top-level summaries
- small previews
- cached `peek()` output

### Layer 4: Slice

只有在任务需要且预算允许时才进入：

- assay subset
- metadata filter
- genomic range subset
- delayed backend summary slice

runtime 必须优先使用缓存，避免重复物化大对象。

## Safety, Budget, and Provenance

### Safety

安全不是执行层独有，语义层也要参与。

- adapter 必须知道哪些操作危险。
- `validate_action()` 在切片或强制 materialization 之前运行。
- 高风险操作可返回 `deny`、`warn` 或 `require_confirmation`。

### Budget

预算应同时覆盖：

- token 成本
- compute 成本
- I/O 成本
- materialization 风险

预算不是提示词里的建议，而应成为 runtime 决策输入。

### Provenance

每一段语义上下文都应附带 provenance。这样 benchmark、日志和未来论文图表都能回答“这个理解来自哪个 adapter、哪个对象版本、哪个来源”。

## Workflow Hints

workflow hints 不应写死在 core 中，而应由 adapter 或扩展包注册。

建议接口：

- `register_workflow_hint(class_or_predicate, hint_fn)`

`hint_fn` 返回结构化 pipeline：

- steps
- recommended tools
- critical checkpoints
- required artifacts
- failure-sensitive operations

对于 `SingleCellExperiment`，可提供 QC、normalization、cluster annotation 的 hint；对于 `GRanges`，可提供 interval overlap、annotation、style harmonization 的 hint。

## Handling S3, S4, R6, and Custom Classes

协议必须支持不同对象模型。

### S3

- 基于 `inherits()`
- 可使用 `attributes()` 和 class-specific accessor

### S4

- 基于 `is()` 或 `methods::extends()`
- 常见 schema 信息来自 `slotNames()` 和标准 accessor

### R6

- 通过 public fields/methods 暴露 identity/schema
- 需特别注意 mutability 和 side effect

### Custom Classes

- 支持实验室注册自定义 predicate
- 支持 priority override
- 不要求自定义类和官方类共享同一继承树

## Conformance Tests

第三方 adapter 不能“随便注册”。建议提供轻量 conformance suite。

每个 adapter 至少验证：

1. `supports()` 能正确识别目标对象。
2. 每个声明的 capability 返回符合规范的结构。
3. `peek()` 和 `slice()` obey budget。
4. provenance 非空且含 package/version 信息。
5. `validate_action()` 至少能拦截一种高风险操作。
6. 缺失 capability 时，adapter 能返回明确的 `not_supported` 而不是 silent failure。

建议 API：

- `run_adapter_conformance_tests(adapter, fixtures)`

## Fallback Strategy

generic fallback 仍然需要存在，但它必须被重新定位：

- 不是默认主路径，而是没有 adapter 时的保底层。
- fallback 输出里应明确标注：
  - adapter not found
  - semantic coverage limited
  - class name
  - 建议扩展点

这样 fallback 不再掩盖缺口，而是显式暴露缺口。

## Integration Points

以下位置需要统一改造为 registry-backed：

1. [R/context.R](/Users/xiayh/Projects/aisdk/R/context.R)
   - `get_r_context()`
   - `summarize_object()`

2. [R/agent_library.R](/Users/xiayh/Projects/aisdk/R/agent_library.R)
   - `inspect_variable`
   - 其他对象 inspection 工具

3. `SharedSession`
   - session env 统一
   - registry 注入 session

4. sandbox 路径
   - live session parent env
   - 执行模式显式化

## Implementation Roadmap

### Phase 0: Session Tightening

- 修 `ChatSession$send()` 的 session 转发
- 合并 `SharedSession` 双轨环境
- 明确 `session_exec` / `sandbox_exec` / `global_exec`
- 在 session 中挂载 `AdapterRegistry`

### Phase 1: Semantic Adapter Scaffolding

- 定义 `SemanticAdapter` 协议
- 实现 `AdapterRegistry`
- 实现 generic fallback adapter
- 将 `get_r_context()` 和 `inspect_variable` 改为 registry-backed
- 引入 layered disclosure controller

### Phase 2: Domain Packs and Workflow Hints

- 实现 `aisdk.bioc`
- 首批支持：
  - `SummarizedExperiment`
  - `SingleCellExperiment`
  - `DESeqDataSet`
  - `GRanges`
  - `DelayedArray/HDF5Array`
- 实现 workflow hint registry
- 加入 benchmark fixtures 和 conformance tests

## Expected Outcome

完成后，`aisdk` 的定位会变得非常清楚：

- `aisdk` 是 semantic runtime 和 live-session substrate
- `aisdk.bioc` 是 Bioconductor semantic pack
- 第三方或实验室扩展包继续注册自定义类 adapter

这样系统既保持 core 的规则严谨，又允许生态的语义多样性持续增长。"}}
</subagent_notification>numerusform to=functions.exec_command մեկնաբանություն  北京赛车开奖_json
