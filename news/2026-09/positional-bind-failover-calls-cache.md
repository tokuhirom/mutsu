# A class composing `PositionalBindFailover` now binds via its `.cache`

A user class that explicitly composes the `PositionalBindFailover` role
should be treated as positional when bound to an `@`-sigil parameter — the
role exists exactly so a custom sequence-like class can bind there via its
`.cache` method, the same way a real `Seq` does:

```raku
sub f(@x) { @x.join(',') }

class Failover does PositionalBindFailover {
    method cache { (10, 20, 30) }
}
say f(Failover.new);   # rakudo: 10,20,30
```

mutsu instead bound the class instance itself, unmodified (`Failover()`).
`coerce_positional_bind_failover` (`src/runtime/types/binding_signature.rs`)
unconditionally called `.iterator` to drain the value before binding it.
`PositionalBindFailover` lets a composing class implement *either*
`.iterator` or `.cache` (rakudo's role only throws "must be implemented" for
whichever one is actually invoked and wasn't overridden — there's no
compose-time check, and no default `.iterator` synthesized in terms of
`.cache`). A class overriding only `.cache` therefore has no real
`.iterator` at all, so calling it unconditionally dispatched to whatever
generic fallback resolves for an unknown method instead of reaching the
class's real data.

Fixed by checking, via the class's user-method MRO
(`class_has_user_method`), whether the value's class actually declares its
own `.iterator`; if so, drain it exactly as before, otherwise call `.cache`
and flatten its result the same way any other positional source is
flattened (`crate::runtime::value_to_list`). The real `Seq`/`HyperSeq`/
`RaceSeq` builtins always have a genuine native `.iterator` and keep using
it unconditionally.

`t/routines/signature/array-param-positional-bind-failover.t` gains the
`.cache`-only case this pins (it previously documented the gap as
untested); the pre-existing `.iterator`-only case in
`t/lang/issue-7750-runtime-binding-errors.t` still passes unchanged.

Closes #8456.
