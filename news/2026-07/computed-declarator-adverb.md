# A parenthesised `:ver(...)` declarator adverb is an expression

`class C:ver(EXPR)` / `:auth(EXPR)` / `:api(EXPR)` stored **the source text of
`EXPR`** as the metadata value instead of evaluating it. `class C:ver($v) { }`
answered `Version.new('$v')`, and the shape that matters in the wild —

```raku
unit class DBDish::SQLite:ver($?DISTRIBUTION.meta<ver>):api($?DISTRIBUTION.meta<api>):auth($?DISTRIBUTION.meta<auth>);
```

— answered a `Version` built from the literal string
`$?DISTRIBUTION.meta<ver>`. `unit class` was worse still: it parsed the adverbs
and threw them away entirely, so `DBDish::SQLite.^ver` was `Mu`.

That is the single remaining failure in `DBIish`'s two big SQLite files.
`role DBDish::Driver` declares `has $.Version = ::?CLASS.^ver`, and
`DBIish::CommonTesting` asserts `ok $drh.Version ~~ Version:D` — which cannot
hold when `.^ver` is `Mu`.

## Fix

`parse_declarator_traits` now returns an `Expr` per adverb rather than a `Value`.
The parenthesised form is parsed as a full expression (falling back to the old
literal text if the expression parser cannot consume it whole, so nothing that
used to parse stops parsing); the angle form `:ver<1.2.3>` stays a literal
string, as its Raku semantics require. `meta_setter_stmt` passes the expression
straight through to `__MUTSU_SET_META__`, so it is evaluated where the
declaration sits.

The `unit class` / `unit role` / `unit grammar` forms now emit those setters too.
They come back from `unit_module_stmt` as a `Stmt::SyntheticBlock` — a
*non-lexical* statement sequence, so the declaration keeps its compilation-unit
scope — with the declaration itself last, which is the element `stmt_list`
already reaches for when it absorbs the rest of the file into the declaration's
body. `unit module` / `unit package` are deliberately left alone: several passes
locate the compilation unit by scanning the top-level statements for
`Stmt::Package { is_unit: true }`, and wrapping it would hide it from them.

Two supporting corrections fell out of making the expression actually run:

- **`Version.new(Any)` is a part-less `Version`, not `Version.new('(Any)')`.**
  `version_from_value` stringified anything it did not recognise, so an
  undefined argument produced a `Version` whose only part was the type object's
  gist. raku answers a defined but empty `Version`, which is what
  `:ver($?DISTRIBUTION.meta<ver>)` yields outside a distribution.
- **A method call on a `Nil` *named* receiver absorbs to `Nil`.** raku's
  `Nil.FALLBACK` makes `$?DISTRIBUTION.meta<ver>` safe when there is no
  distribution. mutsu applied that verdict in the scalar `CallMethod` opcode and
  in the hyper path but not in `CallMethodMut`, the opcode used when the receiver
  is a named variable — so `$?DISTRIBUTION.meta` died with
  `No such method 'meta'`. Without this, making the adverb evaluate would have
  turned a silently-wrong `.^ver` into a hard failure for any module using the
  idiom from a directory with no `META6.json`. It is applied *after* normal
  dispatch fails to find the method, not as a pre-dispatch shortcut: `Nil` really
  does define control-flow and introspection methods (`&?BLOCK.leave` on a Nil
  block), and short-circuiting those silently skipped them. Falling back on the
  not-found error is what `FALLBACK` means. `is_nil` is strictly `Nil`, so an
  uninitialised `Any` receiver still errors as before.
- **An unresolved package-qualified `&Pkg::name` is `Any`, not `Nil`.** A package
  symbol table with no such entry is a different thing from an explicitly absent
  value, and the difference is observable exactly through the rule above:
  `Any.assuming` raises `X::Method::NotFound` naming `assuming`, whereas `Nil`
  absorbs it. `S32-exceptions/misc.t` asserts the former for `&A::b.assuming($a)`
  — `b` is a *method*, so it is not among `A`'s `&`-symbols. Unqualified `&name`
  still returns `Nil`; custom `EXPORT` routines probe it that way.

## Effect on `DBIish`

`44-sqlite-memory` and `45-sqlite-common` go from one failing subtest each to
**109/109** — a clean sweep, and one better than raku, whose remaining failure
there is a `# TODO`-marked `rows()` capability check. With
[the class-in-module fix](class-in-module-sees-module-subs.md) that puts mutsu at
5 of the 9 `DBIish` files passing, up from 1.

Pinned by `t/computed-declarator-adverb.t` (+ `t/lib/ComputedVerAdverb.rakumod`),
14 tests, verified identical under raku.
