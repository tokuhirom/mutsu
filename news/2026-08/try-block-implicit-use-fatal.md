# `try` blocks now implicitly turn on `use fatal` for their whole scope

Per `raku-doc/doc/Language/exceptions.rakudoc`'s "try blocks" section: "A
`try` block is a normal block which implicitly turns on the `use fatal`
pragma". mutsu already had full `use fatal` pragma machinery (a
`fatal_mode` flag on `Interpreter`, consulted at assignment and other sink
points throughout the VM) for the explicit `use fatal;` form, but `try {}`
never set it — only a Failure that happened to be the `try` block's own
*trailing* expression was ever caught (a narrow special case already
handled at the end of `exec_try_catch_op_inner`). Any other `fail()` inside
the body — e.g. one assigned to a `my` variable mid-block — stayed a soft,
unthrown `Failure`, so the `CATCH` block that was supposed to catch it never
ran.

Fixed by toggling `self.fatal_mode` around the whole `try`/`CATCH`/`CONTROL`
region in `exec_try_catch_op` (`src/vm/vm_try_catch_ops.rs`), gated on
`traps` — the flag that already distinguishes a genuine `try` from the
implicit `TryCatch` wrapper the compiler adds around any block that merely
*contains* a `CATCH`/`CONTROL` phaser (that wrapper is not `try` itself and
correctly does not get implicit fatal, matching raku). This reuses all of
the existing `fatal_mode`-gated sink points rather than adding a new
mechanism.

## Effect

This was a general (Cro-independent) binding-semantics gap, not specific to
any one construct — any code of the shape

```raku
try {
    my $x = a_call_that_might_fail();
    ...
    CATCH { default { ... } }
}
```

silently skipped its `CATCH` handler whenever the failing call was not the
try block's last statement. In the Cro compatibility campaign, this exact
pattern is `Cro::HTTP::Session::Persistent::process-requests`'s session
loading (`try { my $session = self.load($cookie-value); $req.auth =
$session; CATCH { default { $req.remove-cookie($!cookie-name); } } }`) — an
expired or missing session's `fail('No such session')` never reached the
`CATCH`, so `$req.auth` stayed a raw unhandled `Failure` instead of falling
back to a fresh session, and the router's route param binding blew up with
a 401 instead of serving a fresh session.

`t/http-session-persistent.rakutest` (vendored Cro::HTTP suite): 12/19 (rc=1,
died) → 19/19. `t/http-session-inmemory.rakutest`: also reaches 13/13
(same root cause, a sibling `Session::InMemory` implementation).

Pin: `t/try-block-implicit-use-fatal.t`.
