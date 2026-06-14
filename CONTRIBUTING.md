# Contributing

If you're considering contributing, please open an issue before a PR. A lot of discussion
also happens in the [Neorg Discord](https://discord.gg/T6EgTAX7ht), so you might consider
joining.

## Tests

If you change a behavior or fix a bug, please make sure to add a test for it!

- run the test suite with `cargo test`

There are snapshot tests and prop tests. If you change the parser behavior or add a new
test case, the snapshots will change and you will see a test failure. You can approve the
new version of the snapshot with:

- `cargo insta review`

Prop tests essentially fuzz the parser and make sure that it doesn't panic. Failed test
cases are saved and version controlled to avoid regressions.

## Benchmarks

Performance benchmarks use [Criterion](https://bheisler.github.io/criterion.rs/). They live in
`benches/parser_benchmarks.rs` with Norg fixtures in `benches/inputs/`.

### Running benchmarks

```bash
# All benchmarks
cargo bench

# Specific group
cargo bench -- parse_tree
cargo bench -- stage_4
```

HTML reports are generated at `target/criterion/report/index.html`.

### Benchmark groups

| Group | What it measures |
|-------|-----------------|
| `parse_tree` | Full pipeline (`parse_tree()`) |
| `parse_flat` | Stages 1+2+3 (`parse()`) |
| `stage_1` | Lexer only |
| `stage_2` | Block parser (tokens → blocks) |
| `stage_3` | Inline parser (blocks → flat AST) |
| `stage_4` | Tree builder (flat AST → tree) |

### Input fixtures

| File | Size | Purpose |
|------|------|---------|
| `benches/inputs/small.norg` | ~30 lines | All construct types, no nesting |
| `benches/inputs/medium.norg` | ~1100 lines | Real blog post, mixed content |
| `benches/inputs/large.norg` | ~180 lines | Deep nesting stress test |

### Comparing across changes

Criterion supports comparing benchmark results against a saved baseline:

```bash
# 1. Save baseline before making changes
cargo bench -- --save-baseline before

# 2. Make your optimization changes

# 3. Compare against the baseline
cargo bench -- --baseline before
```

Criterion automatically reports whether each benchmark improved, regressed, or stayed the same,
with confidence intervals and percentage deltas.

To reset the baseline for a new comparison cycle:

```bash
cargo bench -- --save-baseline main
```

### Bench profile

`Cargo.toml` includes a `[profile.bench]` section with `lto = true` and `codegen-units = 1` for
more consistent and representative timings. This only affects the benchmark target — debug builds
and regular tests remain fast to compile.

<!-- vim: set tw=85 -->
