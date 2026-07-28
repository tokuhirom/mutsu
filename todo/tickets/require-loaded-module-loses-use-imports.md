# A `require`d module's methods do not see the types its `use` imported

`DBIish.install-driver('mysql')` loads the driver with

```raku
my \M = (require ::("DBDish::$drivername"));
```

Loaded that way, `DBDish::mysql`'s methods no longer see the types
`DBDish::mysql::Native` exports, so a bare `MYSQL` is undeclared and degrades to
`Str`:

```
No such method 'mysql_init' for invocant of type 'Str'
```

Loading the same module with a plain `use` works, and that is the difference —
nothing else about the call changes:

```raku
use DBDish::mysql;
my $c = DBDish::mysql.new.connect(:host<127.0.0.1>, :port(13306), …);
say $c.^name;      # DBDish::mysql::Connection  -- connects
```

Instrumented, the split is visible: at the module's **file scope** `MYSQL` still
resolves to `DBDish::mysql::Native::MYSQL`, but inside any **method** of the
class it is `Str`. So the import lands in the module's outer scope and is not
carried into the compiled method bodies on the `require` path.

## Not yet reduced

A small reproduction did **not** reproduce it — a `unit class` importing a
module (including a sub-package of its own name, with attributes, a role, a
qualified class name, an instance invocant, and with the imported module already
loaded) resolves the type fine under `require`. Something about
`DBDish::mysql` specifically is needed; bisecting the real file was inconclusive
because instrumented copies changed the behaviour (edits to the file shifted
which of the two failures appeared).

The next step is to instrument **mutsu**, not the module: find where a bareword
type is resolved and log the scope chain it searches on the `require` path
versus the `use` path.

## Why it matters

It is the front door. `DBIish.connect('mysql', …)` and
`DBIish.install-driver('mysql').connect(…)` both go through `install-driver`, so
every user-facing entry point to the mysql (and Pg) drivers takes this path even
though the driver itself now works
([news](../../news/2026-07/nativecall-cglobal-and-native-methods.md)).

`DBDish::SQLite` is unaffected in practice because it does not name imported
types inside its methods the way the mysql driver does.
