# A typed attribute is now enforced from inside its own class

`$!attr = v` and `$.attr = v` — the twigil forms a method uses to write its own
object's state — silently ignored the attribute's declared type. Only the
`self.attr = v` accessor form checked it, so a typed attribute was unenforced
from exactly the place it is written most:

```raku
class R { has Int $.n is rw; method f($v) { $!n = $v } }
R.new(n => 1).f("nope");   # was: silently stored "nope"
```

The rw-accessor lvalue path consults `get_attr_type_constraint`, but a twigil
write is compiled as an ordinary name assignment: it lands in a local slot (or
env) and is mirrored into the instance's attribute cell afterwards, and none of
those write paths knew the attribute had a declared type. The per-variable
`var_type_constraints` map could not supply it either — it is keyed by bare name
and would conflate `!n` across unrelated classes.

The fix resolves the constraint from the class registry instead, at the point
where the scalar type check already happens. `scalar_attr_type_constraint`
(`vm_run_loop.rs`) maps a local/env name (`!x` / `.x`) to the declared type of
that attribute on the current `self`, and the three scalar-assignment choke
points — `SetLocal` (`vm_var_assign_set_local.rs`), the name-based assign
(`vm_misc_assign.rs`) and the expression-context local assign
(`vm_var_assign_local.rs`) — fall back to it when the name-keyed map has nothing.

Because the fallback plugs into the *existing* constraint branch rather than
adding a new check, an attribute now gets the whole treatment a typed `my`
scalar gets: `X::TypeCheck::Assignment` before the store, `:D` handling,
constraint coercion and native-integer wrapping. Nil keeps its own path, so
`$!n = Nil` still resets a typed attribute to its type object rather than dying.
`@`/`%` attributes are excluded — their declared type constrains the elements,
which the container paths already check — and `Mu`/`Any` attributes are left
alone so untyped code is untouched.

A `subset` behaves like any other declared type, which is where this surfaced:
`HTTP::Request`'s `subset RequestMethod of Str where any(<GET POST …>)` accepted
`set-method('TEST')`.

Pin: `t/attribute-type-check-in-method.t` (private twigil, public twigil,
accessor form, subset, Nil reset, untyped attribute, typed containers, and a
write from `submethod BUILD`).
