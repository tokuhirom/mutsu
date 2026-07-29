# A module's imported type names outlive the frame that `require`d it

`DBIish.connect('mysql', …)` died with

```
No such method 'mysql_init' for invocant of type 'Str'
```

`MYSQL` is `class MYSQL is repr('CPointer')` in `DBDish::mysql::Native`, `use`d
by `DBDish::mysql`, whose `connect` opens with `my $mysql-client = MYSQL.mysql_init;`.
The invocant being a **`Str`** means the bareword fell through to the
bareword-as-string fallback: the type was not visible from the method body.

## Root cause

`load_module` runs a module body with `run_block` in **the caller's `env`**. So
the short-name `Package` aliases the module's own `use` statements install
(`env["MYSQL"] = Package("DBDish::mysql::Native::MYSQL")`) land in whatever frame
triggered the load, and die with it.

A compile-time `use` at file scope never noticed: the alias outlives every later
call. A `require` executed *inside a method* loses it the moment the method
returns — and the module's own methods can then no longer resolve their own
imports. That is exactly DBIish's `install-driver`, which does the `require`
inside a method (memoised in a `//= do { CATCH {…} }` inside a
`$installed-lock.protect` callback) and calls the driver's methods afterwards.

The `require`-inside-a-**sub** spelling appeared to work, but only by accident:
the sub-return env merge leaked the alias all the way out to the script scope,
where nothing had removed it yet. raku does not publish a module's imports to
the requiring scope at all.

Reduced to two four-line modules, with `Drv2.rakumod` = `unit class Drv2; use Drv2::Native;`
and a method reading the exported `THING2`:

```raku
class Loader { method install($n) { my \M = (require ::($n)); M.new } }
Loader.install('Drv2').go;    # was: THING2 resolved to Str
```

## Fix

Type-name resolution for these names is now lexical to the *declaring module*
instead of dynamic to the running frame. `load_module` diffs `env` around the
module body and records every newly added `Package` alias that names a
registered type into `package_type_aliases`, keyed by the module name **and** by
every class/role that module declares — the same set `package_distributions` is
already recorded against. `has_type` and the `GetBareWord` type branch consult it
(via `package_type_alias`) when nothing else accounts for a short name.

The owning package is taken from the running frame rather than `current_package`,
which is `GLOBAL` while a method body runs: the method's class first
(`method_class_stack`), then the routine frame's package, then the current
package — each walked up its `::` chain. A name that is directly registered
short-circuits before the table is consulted, so a locally declared type still
wins over an import.

## Effect

`DBIish.connect` now reaches a live MariaDB (`connected: True`) through the real
`DBIish` → `install-driver` → `DBDish::mysql` path. The remaining failure is
further along, in `prepare`.

Pinned by `t/require-in-method-keeps-module-type-alias.t` (fixtures
`t/lib/RequiredDriver.rakumod` and `t/lib/RequiredDriver/Native.rakumod`), which
covers a `require` in a method, in a sub, and in DBIish's exact
memoise-inside-a-lock-inside-a-method shape. All six assertions pass under raku
too.
