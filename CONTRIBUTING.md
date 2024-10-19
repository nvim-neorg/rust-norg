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

<!-- vim: set tw=85 -->
