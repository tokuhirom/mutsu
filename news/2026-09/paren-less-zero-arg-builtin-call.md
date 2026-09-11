# A paren-less zero-arg builtin call is a call, not a bareword

`sleep;` returned instantly instead of sleeping forever, and `my $x = exit;` bound
the *string* `"exit"` and let execution carry on:

```raku
sub f() { my $x = exit; say "STILL RUNNING, x=$x.raku()"; }
f();
say "AND HERE TOO";
```

Rakudo prints nothing and exits 0. mutsu printed both lines.

## Root cause

Neither `builtin_sleep` nor `builtin_exit` was at fault — the call never reached
them. `sleep` is registered as a listop, but the listop parse needs at least one
argument to follow it. With nothing after the identifier the parse fell through
to the last resort at the end of `parse_identifier_call`, which turns an
unrecognised word into `Expr::BareWord(name)` — the string `"sleep"`.

That fallback had grown an ad-hoc allowlist of names rescued from it one at a
time (`await`/`slip`/`slurp`, then `callframe`/`caller`, then
`return`/`return-rw`, each added after a bug report — the last because
Text::CSV's `$error and return;` guard kept executing the rest of the sub). The
allowlist growing one name per bug report was the actual defect: every core
routine whose parameters are *all* optional was affected in paren-less zero-arg
position, and `sleep`, `exit`, `get` and `prompt` simply had not been reported
yet.

## Fix

The three hardcoded arms are replaced by one table,
`is_zero_arg_callable_builtin()` in `src/parser/primary/ident/predicates.rs`.
Membership is measured against Rakudo rather than guessed: a name belongs there
iff `raku -e 'my $x = NAME;'` compiles. Rakudo rejects a bare
argument-requiring routine at compile time — either with
`Unsupported use of bare "say"` (the Perl 5 unary carve-out) or with
`Calling elems() will never work with signature of the proto ($, *%)` — so a
name that compiles there is one whose signature really does accept zero
arguments. Sweeping mutsu's builtin and listop name tables through that oracle
produced the 38 entries now in the table; everything else (`say`, `ord`,
`elems`, `floor`, `map`, `set`, `bag`, `sleep-until`, ...) keeps falling through
to the bareword/`X::Obsolete` path unchanged.

Two names Rakudo accepts are deliberately excluded because mutsu has no zero-arg
routine behind them, so compiling them to a call would trade a wrong value for a
worse error: `take-rw` (reachable only with an argument, so a zero-arg call is an
"Undeclared routine" *parse* error) and `parse-names` (implemented only as a
method bypass, so a zero-arg call is "Unknown function").

`callframe`/`caller`/`return`/`return-rw` keep their own arm, merged into one:
they must compile to a call even when what follows is not a statement
terminator, which the table's gate does not allow.

## Result

| code | before | after (= raku) |
| --- | --- | --- |
| `sleep;` | returned immediately | sleeps indefinitely |
| `my $x = exit;` | `Str "exit"`, execution continued | exits |
| `my $x = get;` | `Str "get"` | `Any` (reads `$*IN`) |
| `my $x = prompt;` | `Str "prompt"` | `Any` |
| `my $x = join;` | `Str "join"` | `Str ""` |
| `my $x = sum;` | `Str "sum"` | `0` |

Beyond the four names in the report this also corrects `cross`, `flat`, `hash`,
`item`, `join`, `lines`, `max`, `min`, `minmax`, `note`, `repeated`, `repl`,
`roundrobin`, `run`, `sleep-timer`, `sort`, `squish`, `succeed`, `sum`, `take`,
`undefine`, `unique`, `val`, `warn`, `words`, `zip` and `chdir` in the same
position.

This also removes the raciness the finding was hit through: a child spawned as
`Proc::Async.new: $*EXECUTABLE, "-e", "sleep"` now blocks, so a parent's `.kill`
no longer races the child's own 20ms exit. That in turn exposed two real
`Proc::Async` deadlocks in `roast/S17-procasync/kill.t`, which the 20ms exit had
been hiding; they are fixed alongside and written up in
`proc-async-ready-kill-cross-thread.md`.

Pinned by `t/lang/parsing/bare-zero-arg-builtin-call.t`, which checks all three
directions — the zero-arg call really happens, `sleep;` really blocks (measured
in a child process, where only a *broken* `sleep` can fail the assertion),
`exit` really exits, argument-requiring routines still report
`Unsupported use of bare ...`, and calls with arguments are untouched.
