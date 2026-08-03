# An object hash keeps its key type through a `%` parameter

`my %h{Mu}` is an *object hash*: its keys are objects, canonicalised by `.WHICH`.
Passing one to a plain `%`-sigiled parameter used to demote it to an ordinary
string-keyed hash — for the parameter *and* for the caller:

```raku
my class S { }
my %o{Mu};
%o{S} = 7;
sub f(%h) { say %h.raku }
f(%o);
say %o.raku;
```

```
raku:  (my Any %{Mu} = (S) => 7)   /   (my Any %{Mu} = (S) => 7)
before: (my Any % = "S|U553" => 7) /   (my Any % = "S|U553" => 7)
```

Reading `%h{S}` inside the routine took the plain-hash path, stringified the key
object and returned `Any`, warning `Use of uninitialized value of type S in
string context` on the way.

Object-hash-ness lives in two places: `HashData::key_type` on the value, and the
name-keyed `var_hash_key_constraints` / `__mutsu_hash_key_type::<name>` metadata
that every subscript path in the VM consults. Binding a `%` parameter registers
it with `bind_param_type_constraint`, which for a container sigil goes through
the full `set_var_type_constraint` path — and an untyped `%h` arrives there with
the implicit value type `Any` and no key type. That cleared the name-keyed entry
for `%h`, and, because the registration also re-tags the bound value
(`register_var_container_type_metadata` -> `tag_container_metadata`), wrote
`key_type = None` straight onto the shared `HashData`. The entries stay
physically `.WHICH`-keyed either way, so dropping the flag never un-keys them; it
just makes them unreadable.

A parameter imposes no key type of its own, so `bind_param_type_constraint` now
folds the *argument's* key type back into the constraint (`Any` -> `Any{Mu}`)
before registering, and the existing pipeline carries it from there. A parameter
that does declare a key type is left alone, and a plain hash is never promoted.

This is what buried subtests 3 and 4 of the vendored Cro suite's
`http-middleware.rakutest` (`Cro::HTTP::Middleware::Conditional` and
`::RequestResponse`) in `Use of uninitialized value of type SkipPipelineState in
string context` warnings: `Cro::HTTP::Router`'s `!append-middleware` receives the
`my %connection-state{Mu}` its caller declared as a plain `%connection-state`
parameter and keys it by `$comp.connection-state-type`.

Pinned by `t/object-hash-param-binding.t`.
