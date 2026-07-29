# `with $!attr { $_ = ... }` writes back to the attribute — DBIish 35-pg-common SEGV fixed

2026-07-29. DBIish's `t/35-pg-common.rakutest` segfaulted after test 76. The
gdb backtrace showed a double-free inside libpq's `PQclear`, called from
`DBDish::Pg::StatementHandle.finish`:

```raku
method finish() {
    with $!result { .PQclear; $_ = Nil }
    $!Finished = True;
}
```

raku aliases a bare scalar-variable topic read-write, and an attribute
variable is such a variable — so `$_ = Nil` must clear `$!result`. In mutsu
the topic write-back (`topic_source_var`) wrote the env mirror of `!result`
but never reached self's attribute cell, so `$!result` kept the freed
`PGresult` pointer; the next `finish` (allrows' teardown runs it again)
PQclear'd the same pointer — double-free, SEGV.

Fix: the `$_`-assignment topic write-back (both the SetGlobal store path and
the SetLocal path) now also writes an attribute-twigil source through
`write_self_attr_cell`. With it, `35-pg-common` runs 109/109 (raku parity) —
the earlier ledger theory ("PQlibVersion stub → version type-check error →
crash") was wrong; the version error seen mid-file was unrelated to the
crash, which was pure use-after-free.

The DBIish upstream Pg suite is now 9/11 files at raku parity (remaining:
36-pg-enum, 38-pg-errors). Pin: `t/with-attr-topic-writeback.t` (passes under
raku too).
