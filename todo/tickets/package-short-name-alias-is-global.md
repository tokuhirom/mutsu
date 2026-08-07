# A `class A::B` binds the bare name `B` globally, not in package `A`

Declaring a class whose name is already qualified installs its short name into
the *global* env, so it resolves from anywhere:

```raku
class Cro::Hdr { }
say Hdr.^name;   # raku: Undeclared name 'Hdr'   mutsu: Cro::Hdr (was), Str (now)
```

In raku the class is installed into package `Cro`, so bare `Hdr` resolves only
from inside that package. In mutsu the binding is a plain env entry, which means
it is visible everywhere *and* it shadows a later same-short-name declaration in
an inner scope:

```raku
class Cro::Hdr {
    my grammar Hdr { token TOP { \w+ } }
    method check(Str $s) { so Hdr.parse($s) }
}
my $s = Supplier.new;
my $b = supply {
    my enum E2 <X Hdr Y>;
    whenever $s.Supply { emit Hdr.key }     # raku: Hdr    mutsu: dies
};
react {
    whenever $b -> $v { say $v; done }
    whenever Promise.in(0.3) { $s.emit(1) }
}
```

This is the live blocker behind `Cro::HTTP::ResponseParser` and
`Cro::HTTP::RequestParser`: both declare `my enum Expecting <StatusLine Header
Body>` inside their `transformer` supply block, and `Cro::HTTP::Header` is a
class whose short name is `Header`. The enum member loses, and the parsers die
with `X::Undeclared::Symbols: Header`, which is why
`t/http-response-parser.rakutest` sits at 129/154 and
`t/http-request-parser.rakutest` at 93/108.

## Where it comes from

`exec_register_class_op` (`src/vm/vm_typedecl_ops.rs`), the `!parent_is_class`
branch:

```rust
if qualified_name.contains("::") && !parent_is_class {
    let short = /* last :: segment */;
    self.env_mut().entry_or_insert_with(short, || Value::package(...));
}
```

## Why the obvious gate does not work

Restricting the alias to declarations whose `current_package` is the declaring
package (so a file-scope `class Cro::Hdr` gets none) breaks `URI`: `class
URI::Path` is declared at file scope in `URI/Path.rakumod`, and `unit class URI`'s
methods legitimately write `my Path $path`. Their `current_package` at method-run
time is not `URI` either (method dispatch only re-points `current_package` when
the owner class has class-scoped subs, package lexicals, or a `::` in its name).

## Attempt #1 (2026-08-07, reverted — closed PR #6010): package-chain walking is not enough

Moved the short-name alias out of the global `env` into `package_type_aliases`
(the existing table that already carries a module's own `use`-import aliases),
keyed by the *declaring parent package*, and left bareword resolution's existing
`package_type_alias`/`lookup_in_running_package` machinery — which walks
`method_class_stack` → the running frame's `package` → `current_package()`, each
further walked up its own `::` ancestor chain — to find it. This does fix the
two examples above (verified): `Cro::Hdr`'s `Hdr` no longer leaks globally, and
`URI::Path`'s `Path` still resolves inside `URI`'s own methods (case 3 of the
method-dispatch anchoring gate, `owner_class.contains("::")`, already anchors
`current_package`/`method_class_stack` to the *class's own* qualified name,
whose ancestor chain includes its own declaring package).

**But it breaks a real, working ecosystem module (DBIish's Postgres driver) —
CI's bundled-library gate failed 4/4 times on `DBIish/38-pg-errors.rakutest`.**
Root cause, minimal repro:

```raku
# lib/Foo/Native.rakumod
unit module Foo::Native;
class PGconn is export is repr('CPointer') { }

# lib/Foo/Driver.rakumod
unit class Foo::Driver;
use Foo::Native;
method make() { PGconn.new }   # "Undeclared name: PGconn" — fails on the
                                # package-chain-walking fix, works on main
```

`DBDish::Pg::Native` declares `class PGconn` (non-`my`, package-scoped).
`DBDish::Pg` (`unit class DBDish::Pg ... does DBDish::Driver`) `use`s that
module and references bare `PGconn` from inside `method connect(...)`. Method
dispatch anchors `current_package`/`method_class_stack` to the *receiver
class* — here `DBDish::Pg` — and the ancestor-chain walk from there is
`DBDish::Pg` → `DBDish` → stop. **`DBDish::Pg::Native` is a sibling of
`DBDish::Pg`, not an ancestor**, so no ancestor-chain walk can ever reach it,
no matter how the write side is keyed. This is not a narrow edge case: "declare
a native-handle class in a `::Native` submodule, `use` it from a sibling driver
class, reference its bare name" is an ordinary, common pattern (also present in
DBIish's `mysql` and `SQLite` drivers, and structurally identical to how many
NativeCall-based bindings are organized).

The old global-env write, despite being architecturally wrong (the original bug
this ticket is about), *accidentally* made this idiom work, because it doesn't
care about ancestry at all — every package-scoped short name was visible
everywhere. Attempt #1's fix trades "occasionally wrong resolution in a naming
collision" for "a legitimately `use`-imported class's bare name is now
`Undeclared` — code that worked stops working." That is a worse regression than
the bug being fixed, so it was reverted (PR #6010 closed, unmerged; no commits
landed on `main`).

## What a real fix needs

Not ancestor-chain walking. Real Raku's actual rule is closer to "a name a file
brought into scope via `use`/`need` is visible in *that file's own lexical
scope*, regardless of what package/class the file's declarations end up
qualified under." That is fundamentally a **per-importing-scope symbol table**
problem (what did *this specific file or class body* `use`?), not a
package-hierarchy problem. `package_type_aliases` already has half of this
(diffing env before/after a `use` to capture a module's own transitively
imported names, keyed by what that module itself declares) but that mechanism
answers "what can code *inside* the imported-from module see", not "what can
code *that did the importing* see" — the direction this bug needs.

Building that properly is the "own design pass" this ticket has needed from the
start (still XL effort). Do not re-attempt the narrow ancestor-chain-walking
fix — it is now proven insufficient by the DBIish counter-example above; any
new attempt needs a mechanism that also covers sibling-package imports.

## Related

`todo/tickets/class-nested-my-class-clobbers-outer-short-name.md` was the
*class-body* half of this problem and is fixed — a type declared in a class body
now has its short-name binding restored when the body ends, and nested
classes/roles/subsets are recorded as class-scoped short names so the class's own
methods still resolve them (`t/class-body-type-scope.t`). This ticket is the
remaining *package* half.
