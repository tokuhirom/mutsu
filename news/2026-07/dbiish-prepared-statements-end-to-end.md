# DBIish prepared statements work end to end against a live MariaDB

`DBIish.connect('mysql', ...)` now runs the full prepared-statement lifecycle
on mutsu against a real MariaDB server: `prepare`, parameter binding,
`execute` with typed parameters (Str/Int/Rat), `allrows` with per-column type
conversion, `.row`, and `dispose` — byte-identical output to Rakudo. This
closes `todo/deep/module-scope-lexicals-need-a-defining-module-anchor.md`,
whose "no defining-module anchor at the read site" diagnosis turned out to be
stale: the routine frames DO carry the defining module's package now, and the
remaining blocker chain was six distinct general-purpose bugs, each found by
running the real driver and instrumenting the exact failure point.

1. **A `%h{...}` / `@a[...]` read never reached the module-scope fallback.**
   `GetGlobal` already consulted `module_scope_lexicals` (the file-scope names
   a module declared for itself, surviving a `require`-inside-a-method), but
   `GetHashVar`/`GetArrayVar` silently produced an empty container instead.
   `MoarVM::Guts::REPRs`' private `my %known-bodies` read from its own
   `BODY_OF` sub therefore came back empty and `pointer-to` died with
   "Cannot dereference a Pointer[Any]".

2. **A CStruct field's `constant` type alias died with the loading frame.**
   `cstruct_layout` resolved `has intptr $.length` through the live env only;
   once the `require` frame was gone, the alias was unresolvable, the whole
   layout failed, and `nativesizeof(MYSQL_BIND)` reported the class as
   P6opaque. Field aliases now resolve against the declaring class's own
   module chain (`resolve_native_type_alias_for_owner`, anchored on the
   attribute's owner rather than the running frame).

3. **A parametric role's body lexicals died with the composition frame.**
   `LinearArray[::T]`'s `my int $sol = nativesizeof(T)` / `my \ty = T` lived
   only in the env of whichever frame first parameterised the role. The first
   `LinearArray[MYSQL_BIND].new` worked (composition had just run there); the
   second, in a fresh frame, read `$sol` as Nil and calloc'd a zero-stride
   bind array — the segfault inside `mysql_stmt_fetch`. The composition now
   persists the role-body lexicals as class-body statics of the composed pun
   class (the same store `inject_class_body_statics` reads), so every later
   method call sees them.

4. **`bless` seeded an `is Type` container attribute as a plain Hash.**
   `has %.Converter is DBDish::TypeConverter` built a `TypeConverter`
   instance through `dispatch_new`'s seeding but a plain empty Hash through
   the `bless` route (which is how `DBDish::mysql::Connection` is
   constructed), so `.convert-function` had no invocant. Both routes now
   share `build_is_type_container`.

5. **A `when` succeed escaped its `do { }` block.** DBIish's `execute`
   computes each parameter buffer with `do { when Blob {...} when Str {...}
   default {...} }` and then fills the `MYSQL_BIND` in a following `given`.
   In raku a matched `when` exits only its innermost enclosing block; mutsu
   let the succeed signal travel on, skipping the whole rest of the `with`
   body — every inserted value became NULL. `DoBlockExpr` and the
   block-local-scope executor now absorb the succeed (yielding the matched
   body's value) and reset the `when_matched` flag so an enclosing `given`
   continues.

6. **Writing an enum value into a CStruct field stored 0.** `to_int` had no
   `Enum` arm, so `.buffer_type = MYSQL_TYPE_DOUBLE` wrote
   `MYSQL_TYPE_DECIMAL` (0) and MariaDB rejected the doubles with "Out of
   range value". Enums now numify to their underlying value there.

Pins: `t/when-succeed-innermost-block.t`, `t/cstruct-field-enum-write.t`,
`t/require-in-method-cstruct-role-statics.t` (a LinearArray-shaped fixture
under `t/lib/DeferredCStruct*`), `t/bless-is-type-container-attr.t`, and three
new assertions in `t/require-in-method-keeps-module-type-alias.t`. All pass on
Rakudo unchanged. The end-to-end MariaDB run itself needs a live server
(`docker mutsu-mariadb`, port 13306) and stays a manual check:
`tmp/dbiish-e2e.raku` against the vendored `DBIish-0.6.8` tree.
