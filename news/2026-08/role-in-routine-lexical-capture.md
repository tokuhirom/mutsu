# A role declared inside a routine captures that routine's lexicals

Completes `todo/tickets/class-in-routine-does-not-capture-routine-lexicals.md`.
Its two repros — a `class` declared in a `sub` body, and a `where` constraint on
such a class's method parameter — were fixed by
`capture_declared_method_envs` (2026-08-31, PR #7169). The **role** twin was
not:

```raku
sub p { my $v = 3; role R { method go { $v } }; class G does R { }; G.new }
say p().go;     # raku: 3    mutsu: Nil
```

The same declaration inside a bare block already worked; only the routine-body
case failed, exactly as the class case had.

## Why the class-side pass could not reach it

`capture_declared_method_envs` walks the class's method table and deliberately
skips every candidate with `role_origin.is_some()`. That skip is right: a
composed copy closes over the **role's** declaration site, not the composing
class's, and the composing class's `method_outer_lexical_slots` describe a
different frame. So a role's methods have to record their capture where they
are declared.

## The fix

- `CompiledRoleDeclPlan` gains `method_outer_lexical_slots`, the role twin of
  the class plan's field, populated the same way in `decl_plan.rs`.
- `capture_declared_role_method_envs` runs at `RegisterRole` and writes the
  capture onto the role's own `MethodDef`s — in `registry.roles` *and* in each
  parameterized `role_candidates` entry, since composition may read either.
  `compose_role_into_class`'s `md.clone()` then carries it into every composing
  class for free, and the dispatch side needs no change: it was already generic
  over `MethodDef::captured_env`.
- The two passes share a new `declared_method_capture_envs` core.
- `substitute_type_params_in_method` was dropping `captured_env` (the only
  field of the ~20 it did not carry over). That is the path a **parameterized**
  role's methods take into a composing class, so `role P[::T] { method go { $v } }`
  still read `Nil` until it was carried through.

### Trap 2: a role must not re-capture what it COMPOSED

`role B does A[:a(1)] { }` holds a *copy* of `A`'s methods, and that copy closes
over `A`'s declaration site — which `A` already recorded. Capturing again at `B`
bound `B`'s own enclosing lexicals over `A`'s parameters, so
`role A [:$a = 1, :$b = $a * 2]` composed into `B` read a file-scope `my $a = 0`
(`roast/S14-roles/parameterized-mixin.t` 27-28). The role pass skips
`role_origin.is_some()` methods for exactly the reason the class-side pass does.

### Trap 1: a role's type parameters are not captures

`my role R[Str:D $s] { method tag { $s } }` reads `$s` from the binding
`does R["x"]` makes. mutsu allocates one local slot per name for a whole chunk,
so an unrelated earlier `my $s` at file scope IS in
`method_outer_lexical_slots` at the role's declaration site — and capturing it
shadowed the real type-parameter binding, making `C.tag` answer `(Any)`
(`t/positional-read-of-a-non-positional.t`). The role pass therefore filters its
own `type_param_defs` names out of the candidate slots first.

## Coverage

`t/class-in-routine-lexical-capture.t` grows from 5 to 12 assertions: a composed
role method's capture, a live capture written after composition and read
through the object, a parameterized role's capture surviving type substitution,
a punned role, a shadowing variant where the routine lexical must beat a
same-named caller lexical, and a role composed into a role keeping the source
role's parameters. The whole file passes under `raku` as well as mutsu.
The bundled-library gate goes 273/297 → **274/297**.
