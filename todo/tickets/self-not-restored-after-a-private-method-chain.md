# `self` is not restored after a private-method call chain

Reading a private attribute right after a private method call can throw, because
the frame's `self` is left pointing at some *other* object the chain touched:

```
P6opaque: no such attribute '$!stmt' on type DBDish::mysql::Connection
          in a DBDish::mysql::Connection when trying to get a value
```

The attribute is `DBDish::mysql::StatementHandle`'s, and the invocant really is
one. Instrumented two statements earlier in the very same method, everything is
correct:

```raku
method execute(*@params --> DBDish::StatementHandle) {
    # PROBE self=DBDish::mysql::StatementHandle   attrs=… $!stmt …
    # PROBE $!stmt reads fine -> MYSQL_STMT
    self!enter-execute(@params.elems, $!param-count);
    if $!stmt {                       # <-- throws; `self` is now the Connection
```

`self!enter-execute` (`DBDish/StatementHandle.rakumod:36`) is

```raku
method !enter-execute(int $got = 0, int $expect = 0) {
    self.finish unless $!Finished;
    $!affected-rows = Nil;
    self!ftr;
    self!set-err(-1, "…").fail unless $got == $expect;
}
```

and `set-err` reaches the handle's `$!parent`, which is the
`DBDish::mysql::Connection` the error message names. So a nested call that ends
up invoking something on another object leaves `self` bound to that object after
it returns.

## Where to look

`missing_private_attr_read_error` (`src/vm/vm_var_assign_computed_attr.rs`)
reads the invocant with `get_env_with_main_alias("self")`, which is what
observes the wrong value — but it is only the messenger. The bug is that
`env["self"]` (and/or the method-class stack, which also reported
`DBDish::mysql::Connection` as the *owner*) is not restored when a private
method call returns. Both are set on method entry; find the exit that fails to
put them back.

Note the error message is misleading twice over: it names the class that
happened to be in `self` rather than the invocant, and it turns a "variable not
found" into an attribute error. Worth fixing the message once the cause is.

## Not yet reduced

A one-level version passes:

```raku
role R { has $!parent; method !enter($n) { $!parent.ping; $n } }
class SH does R {
    has $!stmt = "S";
    method exec() { self!enter(1); if $!stmt { "ok" } else { "missing" } }
}
say SH.new(…).exec;      # both: ok
```

The real chain is three deep (`self.finish`, `self!ftr`, `self!set-err` → the
parent object) and mixes a role-provided private method with an attribute write
in between, so try growing the repro along those axes.

## Why it matters

It is the last thing between mutsu and running a query against MariaDB. The
connection works and `prepare` now returns a real `StatementHandle`
([news](../../news/2026-07/nativecall-cglobal-and-native-methods.md),
[news](../../news/2026-07/cstruct-handles-carry-their-registered-name.md));
`execute` is where it stops. `DBDish::Pg` shares `DBDish::StatementHandle`, so
it will hit exactly the same line.
