# A `:$name` colonpair loses sight of an outer pointy binding

`DBDish::mysql::Connection.prepare` is

```raku
self.protect-connection: {
    with $!mysql-client.mysql_stmt_init -> $stmt {
        with self!handle-errors($stmt.mysql_stmt_prepare($statement, $statement.encode.bytes)) {
            DBDish::mysql::StatementHandle.new(
                    :$!mysql-client, :parent(self), :$stmt
                    :$statement, :$!RaiseError, |%args
                    );
        } else { .fail }
    } else { … }
}
```

and mutsu dies on the `:$stmt`:

```
P6opaque: no such attribute '$!stmt' on type DBDish::mysql::Connection
          in a DBDish::mysql::Connection when trying to get a value
```

`:$stmt` compiles to `"stmt" => Var("stmt")` (confirmed with `--dump-ast`), so
this is an ordinary variable read that misses. On the miss, mutsu's
`missing_private_attr_read_error` maps the bare name to the private attribute
`$!stmt` and reports that instead — a good error for a genuine attribute typo,
misleading here.

**The tell: adding `note $stmt;` immediately before the `.new` makes it work.**
So the binding exists and is reachable; something about *this* read does not
find it, and touching the name once beforehand fixes it. That is the shape of an
env/locals coherence gap — the `-> $stmt` binding lives in a local slot and the
colonpair's by-name lookup does not consult it until something else has synced
it into `env`.

## Not yet reduced

Five reductions were tried and all **pass**, so none of them is the trigger on
its own:

- a `with … -> $stmt { … }` inside a block passed to a method taking `Callable`;
- the same with a second, nested `with` around the `.new`;
- the comma-less colonpair juxtaposition (`:$stmt` newline `:$statement`);
- attribute colonpairs (`:$!mysql-client`) mixed with lexical ones in one call;
- a native method call on `$stmt` before the `.new` (which is present in the real
  code, and which would have been the obvious suspect since native-method
  dispatch is new).

The next step is to instrument **mutsu** rather than the module: log the scope
chain `Var("stmt")` searches at that call site, on the failing path and on the
`note`-fixed one, and compare.

## Why it matters

It is the last thing between mutsu and running a query against MariaDB. The
connection itself works
([news](../../news/2026-07/nativecall-cglobal-and-native-methods.md)), and
`prepare` reaches this line. `DBDish::Pg` is likely to hit the same wall, so it
is worth fixing before the Pg driver is measured.

Worth checking while there: whether `missing_private_attr_read_error` should
fire at all for a name that has no `$!`/`$.` twigil in the source. It turns a
"variable not found" into an attribute error, which cost real time here.
