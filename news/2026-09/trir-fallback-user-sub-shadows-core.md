# TRIR bind-decline fallback reaches the user sub, not the core routine

A statically linked `CallTrir` call site re-dispatches by name when it declines
to bind its arguments (for instance, any sigilless parameter declines for a
caller variable, because it would have to bind the container). That fallback,
`exec_call_trir_fallback` in `src/trir/entry.rs`, went through
`call_function`, whose builtin `match` runs before the registry is consulted.
So a user routine that shares its name with a core routine ran the core one
instead:

```raku
use nqp;
sub copy(Uni:D \codes) { nqp::atpos_i(codes, 0) }
my $codes := nqp::strtocodes("abc", nqp::const::NORMALIZE_NFC, nqp::create(NFC));
say copy($codes);   # was "copy requires a destination path"; now 97
```

The site was linked to a user routine at compile time, so the fallback now
uses `call_function_fallback`, the same entry the untyped `CallFunc` uses for a
user sub that shadows a builtin. It prints `97` like rakudo and like
`MUTSU_TRIR=off`, and a type-mismatched call still raises the user sub's
binding error rather than falling into the core routine.

The issue's second question (why the bind declines at all) is by design: the
TRIR binder copies values into its frame, and a sigilless parameter bound to a
caller variable must see that variable's container. That is a speed matter
only and is left as it is.

Pinned by `t/vm/codegen/trir-fallback-user-sub-shadows-core.t`. Closes #9288.
