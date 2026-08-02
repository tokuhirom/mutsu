# An EVAL'd `my` no longer clobbers a same-named caller lexical

```raku
my $a = 10;
EVAL 'my $a = 999';
say $a;          # mutsu printed 999; raku prints 10
```

A `my` inside EVAL'd code is lexically scoped to that EVAL. mutsu already knew
this for *new* names — `parse_and_eval_with_operators` snapshotted the plain
user-lexical keys present before the snippet ran and dropped any the snippet
introduced, so `EVAL 'my $y'; $y` stayed undeclared. But when the caller already
used the name, the declaration **shadowed** it, and mutsu's shared env has one
entry per name: the declaration overwrote the caller's value in place, and the
key already existed so nothing removed it afterwards.

Two changes fix it:

- The snippet's parsed AST is walked for the names it declares with `my`
  (`collect_eval_declared_lexical_keys`, skipping `our`/`state`/dynamics and not
  descending into routine or class bodies, whose lexicals live in their own
  frame). The caller's value for exactly those names is snapshotted before the
  run and restored afterwards. A plain assignment — `EVAL '$a = 999'`, which
  *must* write through to the caller — is not a declaration and is untouched.
- The post-EVAL lexical cleanup now runs whatever the snippet did. It used to sit
  behind `?` on the evaluation result, so a snippet that *threw* skipped it
  entirely. That is the common case in practice: `throws-like 'my $x = 999; die
  "x"', Exception` is a standard assertion shape, and under the vendored upstream
  `Test.rakumod` it left `$x` at 999 in the file that called it.

Pinned by `t/eval-my-shadows-caller-lexical.t` — scalar, array and hash shapes,
with and without a throw, through a closure frame and through a routine frame,
plus the assignment case that must still write through and the EVAL's own view of
its declaration. Six of its ten assertions fail without the fix.

Found while running `t/` against the vendored upstream `Test.rakumod`
(`todo/tickets/vendor-real-test-module.md`): the real `throws-like` EVALs its
string argument, so `t/throws-like-outer-var-writeback.t`'s "a fresh `my` inside
`throws-like` does not clobber the caller" read 999. That file now passes under
`MUTSU_REAL_TEST=1`.
