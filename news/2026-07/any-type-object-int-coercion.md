# `.Int` on Any/Mu type objects warns and returns 0 (Mu's coercion)

`my $value = Any; $value.Int` used to throw `No such method 'Int' for invocant
of type 'Any'`. In raku, a bare type object that inherits Mu's coercion warns
"Use of uninitialized value of type Any in numeric context" and returns the
Int `0` — the same treatment `.Numeric`/`.Real` already had in mutsu. This bit
DBIish's row-typing (`$value.$ct` in `DBDish::SQLite::StatementHandle::_row`,
where `$ct` holds a type object like `Int`), and any `.Int` call on a
genuinely-undefined value.

The fix mirrors raku's dispatch shape, verified case by case against raku:

- `Any.Int` / `Mu.Int` / `Cool.Int` / `IntStr.Int` / user-class and role type
  objects → warn + `0`, via the same slow-path fallback arm that already
  handled `.Numeric`/`.Real` (`runtime/methods_instance_ops.rs`), so a
  user-defined `.Int` method still dispatches first. Roles keep their
  existing silent `0` for back-compat, as with `.Numeric`.
- `Num.Int` / `Str.Int` / `Rat.Int` / `FatRat.Int` / `Complex.Int` → these
  types only define `Int` multis with a `:D` invocant, so their type objects
  now throw `X::Parameter::InvalidConcreteness` ("Invocant ... must be an
  object instance ... Did you forget a '.new'?") instead of "No such method"
  (fast path, `builtins/methods_0arg/dispatch_core_coerce.rs`).
- `Int.Int` stays the identity; `UInt.Int` now also returns the invocant
  unchanged (UInt is a subset of Int and inherits `method Int { self }`).
- `Nil.Real` now warns "Use of Nil in numeric context" and resumes with `0`
  like `Nil.Int`/`Nil.Numeric` — it was silently absorbed to `Nil`
  (`vm/vm_call_method_ops.rs`).

`.Num` on an `Any` value was already correct: raku itself throws
`No such method 'Num'` there (Any is not Cool), which mutsu matched — the
original ticket's assumption that `.Num` should warn was wrong.

Pinned by `t/any-type-object-int-coercion.t`. One adjacent gap was split out
and fixed in the stacked follow-up
(news/2026-07/bound-nil-variable-method-dispatch.md): a variable *bound* to
Nil (`my $v := Nil`) dispatched through the named-variable opcode path, which
skipped all Nil special-casing.
