# `item` parses as a listop, so `item [1,2,3]` keeps its argument

`item` was missing from the parser's listop table, so only the parenthesized
form worked. Written paren-less it swallowed nothing: `item [1, 2, 3]` parsed as
a bare word `item` with the array literal stranded, and `@vars.push: item [...]`
pushed the **string** `"item"`.

```raku
my @v; @v.push: item [1,2,3]; say @v.raku
# raku:  [[1, 2, 3],]
# mutsu: ["item"]
```

Found in rakudo's own `Test.rakumod`, which saves and restores its module state
around every `subtest` exactly that way:

```raku
sub _push_vars { @vars.push: item [$subtest_callable_type, $num_of_tests_run, …] }
sub _pop_vars  { ($subtest_callable_type, $num_of_tests_run, …) = @(@vars.pop) }
```

so each `subtest` restored garbage and the outer test counter fell back to 1 —
the fourth general interpreter bug on the way to running the genuine upstream
module (`todo/tickets/vendor-real-test-module.md`). The list assignment in
`_pop_vars` was fine all along; only the snapshot was.

Pinned by `t/item-listop.t`, which includes the save/restore shape and a guard
that a bareword `item => 1` pair key still parses. Passes under `raku` too.
