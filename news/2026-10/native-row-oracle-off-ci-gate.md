# The native method row Rakudo-oracle check is off the CI gate

The two tests in `src/builtins/native_method_row_rakudo_oracle.rs`,
`recognized_rakudo_methods_are_never_denied` and
`declared_bits_are_never_false_claims`, are now `#[ignore]`d. Each time a PR
taught a native cascade a new method, it also had to edit a row in the shared
`native_method_row_table.rs`. On 2026-10-03, parallel PRs doing that kept `main`
red for over an hour. The check will be rebuilt in a form that cannot break
`main` (#11405). Until then, run it on demand with
`cargo test --lib native_method_row_rakudo_oracle -- --ignored`.
