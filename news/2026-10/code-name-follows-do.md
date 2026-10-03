# `Code.name` follows the routine's `$!do`

In rakudo a routine's `.name` is `nqp::getcodename($!do)`, so the routine and
the body held in its `$!do` share one name. mutsu's `$!do` is a separate copy
of the body (calling it must bypass the routine's wrap chain), so
`nqp::setcodename(nqp::getattr($r, Code, '$!do'), 'bar')` renamed only that
copy and `$r.name` kept answering the old name.

A `$!do` copy now links back to the code object it was copied from, and
`Code.name`, `Code.set_name` and `nqp::setcodename` resolve the one object that
holds the name: a routine whose `$!do` was rebound answers its bound body's
name, and renaming a shared body renames every routine that runs it. Reading
`$!do` back after a bind also answers the very object that was bound, so
`nqp::eqaddr` on it holds (#11462).
