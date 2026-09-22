# Collection loads with qualified matchers and hyphenated return types

`Collection` v0.18.0 could not load because two valid Raku type spellings
were parsed as executable code. A qualified exception matcher inside a
`unit module` caused the following `CATCH` block to be swallowed, and a
hyphenated qualified type after a `returns` trait was parsed as a call to the
`returns` routine.

The parser now recognizes locally declared qualified types in `when` matchers
and accepts hyphens and apostrophes in qualified return-type names. The fixes
also unblock `Raku::Pod::Render`; both distributions now pass their complete
measured suites. `raku-pod-extraction` loads its `PodExtraction` module but
remains `no_baseline` because its `RakuDocRender` dependency fails under the
Rakudo oracle as well.

Pinned by `t/control/when-undeclared-type-gobbles-block.t` and
`t/routines/call/return-type-hyphenated-qualified.t`.
