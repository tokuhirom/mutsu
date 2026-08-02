# An undeclared variable is `X::Undeclared`, not `X::Undeclared::Symbols`

Rakudo splits the two undeclared-symbol exception classes by *what the symbol
is*, not by when the failure was noticed:

| source | class | message |
| --- | --- | --- |
| `$undeclared` | `X::Undeclared` | `Variable '$undeclared' is not declared` |
| `zzz()` | `X::Undeclared::Symbols` | `Undeclared routine:\n    zzz used at line 1` |

The two are **not related**: they share the `X::Comp` *role*, not a superclass,
so `X::Undeclared::Symbols ~~ X::Undeclared` is `False`. Answering with the
wrong one is therefore directly visible to `throws-like`, which is how this
surfaced — under the vendored upstream `Test` module
(`todo/tickets/vendor-real-test-module.md`) two files failed on
`right exception type (X::Undeclared)` where mutsu had produced
`X::Undeclared::Symbols`.

Two independent causes, both now fixed.

## The VM's variable read used the wrong class

mutsu already raised a well-formed `X::Undeclared` from the CHECK-time scan
(`src/runtime/system_eval_vars.rs`) and from the regex-interpolation paths. But
the VM's own fallback — the read of a local slot whose name is in no env
(`src/vm/vm_var_assign_local_get.rs`) — hand-rolled a message string tagged
`X::Undeclared::Symbols:`, and it reported the env key verbatim, which for a
scalar carries no sigil:

```
$ mutsu -e 'try { { our $sa2 = my $sb2 = 42; }; ($sa2, $sb2) }; say $!.^name; say $!.message'
X::Undeclared::Symbols
Variable 'sa2' is not declared
```

That shape reaches the VM rather than the CHECK-time scan whenever the scan's
conservative declaration collection has already seen the name — here the
chained `our $sa2 = my $sb2 = 42` inside a block. So the same program text
produced one class or the other depending on which path noticed it.

The site now builds a real typed exception via a new
`RuntimeError::undeclared_variable()`, which carries `what`, `symbol`, `name`,
`post`, `highexpect` and `suggestions` the way the CHECK-time path does, and
restores the sigil:

```
X::Undeclared
Variable '$sa2' is not declared
```

## Calling a CORE term constant is the variable-shaped error

`e`, `pi`, `tau`, `i`, `Inf`, `NaN`, `True` and `False` are terms, not
routines. Calling one is not "nobody declared this name" — the symbol exists,
it just does not exist under the `&` sigil — so rakudo reports
`X::Undeclared` naming `&e`:

```
$ raku -e 'try EVAL q[e()]; say $!.^name, "  ", $!.message.lines[0]'
X::Undeclared  Variable '&e' is not declared. Perhaps you forgot a 'sub' if
```

whereas an entirely unknown `zzz()` gets the CHECK-time
`X::Undeclared::Symbols`. mutsu answered `X::Undeclared::Symbols: Unknown
function: e` for all eight names. The runtime call fallback now recognises the
set (`CORE_TERM_CONSTANTS` in `src/runtime/undeclared_routines.rs`) and raises
`X::Undeclared` for `&name`. `now`, `time` and `rand` are deliberately not in
the set — they are real routines, and rakudo does answer
`X::Undeclared::Symbols` for `now()` and `time()`.

Pin: `t/undeclared-symbol-exception-class.t`, which passes unchanged under
`raku`. This freed `t/block-lexical-scope.t` and
`t/gate-b-callee-name-collision-and-deref-capture.t` under the real `Test`
module, taking that ledger from 15 regressions to 13.

Worth recording for the next triage: both files had been filed under
`todo/deep/exception-class-hierarchy-is-mostly-unregistered.md` on the strength
of the failure text mentioning two `X::Undeclared*` names. Neither was a
hierarchy-registration problem — mutsu was simply raising the wrong class, and
registering `X::Undeclared::Symbols` under `X::Undeclared` would have been
actively wrong, since raku says they are unrelated.
