# `self` is lexical inside a block, even when the block runs elsewhere

`self` in Raku is a lexical: a bare block has no invocant of its own, so a
`self` (or the `$!attr` / `$.attr` forms that desugar through it) inside one
resolves outwards to the enclosing method's invocant. mutsu resolved it
*dynamically* instead — the closure's captured `self` was deliberately not
allowed to overwrite whatever the running frame held. That is invisible while
the block runs in the frame that created it, and wrong the moment it escapes:

```raku
class Conn { method protect(Callable $code) { $code() } }
class SH {
    has $.parent;
    has $!stmt = 'STMT';
    method execute() { $!parent.protect: { $!stmt } }
}
SH.new(parent => Conn.new).execute;
```

The block runs inside `Conn.protect`, where the live `self` is the `Conn`, so
the `$!stmt` read failed with `P6opaque: no such attribute '$!stmt' on type
Conn`. The message was misleading twice over: the attribute name was right and
the invocant was simply the wrong object.

This is exactly what stopped mutsu one line short of running a query against
MariaDB. `DBDish::mysql::StatementHandle.execute` hands the statement's work to
its connection:

```raku
$!parent.protect-connection: {
    $!stmt.mysql_stmt_execute or $!Prefetch and $!stmt.mysql_stmt_store_result;
}
```

`DBDish::Pg` shares the same `DBDish::StatementHandle` role, so it hit the same
line. The recorded diagnosis blamed a private-method call chain leaving `self`
unrestored, because the failing read sits two statements after
`self!enter-execute`; that was wrong. The private chain restores `self` fine —
the block passed to another object's method never carried its own `self` to
begin with.

The fix makes a captured `self` overwrite the running binding at closure entry,
in the VM's `call_compiled_closure_with_topic` and in the interpreter's native
map/grep/first loops, which pre-insert a closure's captured env the same way. A
method's own invocant is bound from its arguments *after* that merge, so a real
method call still binds its own `self`; the map/grep loops now also list `self`
among the keys they restore on exit.

Pinned by `t/self-is-lexical-in-blocks.t`, which covers the escaping block, the
pointy block, the anonymous sub, a block returned out of the method that made
it, per-instance capture, and the `map` / `grep` / `first` / `.()` / `start`
invocation paths.
