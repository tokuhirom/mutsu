# An `is Array` subclass delegates the same way however the call is spelled — and a runtime method name no longer overflows the stack

`todo/tickets/array-subclass-mut-path-does-not-delegate-rendering.md` recorded
one symptom: `my $v = R.new(1,2); $v.Str` answered `R()` where raku answers
`1 2`, while the chained `R.new(1,2).Str` was correct. Widening the probe to a
24-method matrix turned up a much worse one in the same mechanism.

## The crash

```raku
class R is Array {}; my $m = "elems"; my $v = R.new(1,2); say $v."$m"();
```

overflowed the stack. The recursion is four frames wide and self-evident once
seen: `builtin_elems` is defined AS `$x.elems`, `dispatch_elems_method` answers
`.elems` by calling `builtin_elems`, and an `is Array` subclass instance is
something neither can serve — so they bounced off each other until the stack
ran out. `dispatch_elems_method` already carried a guard against exactly this
cycle, but only for the MOP-shaped `$obj.HOW.elems($obj)` spelling.

The literal spelling (`$v."elems"()`) never hit it, because the `CallMethod`
opcode delegates to the backing storage before it probes anything else.

## The mechanism: one decision, made in four different places

An `is Array`/`is List` subclass keeps its elements in a backing
`__mutsu_array_storage` attribute. Whether a method is answered by the instance
or by that storage was decided independently by each dispatch entry:

| entry | before |
|---|---|
| `CallMethod` opcode | delegates by default (minus type identity) |
| `CallMethodMut` opcode | delegates through a short allowlist (`is_array_storage_native_safe`) |
| `CallMethodDynamic` (runtime method name) | probes the native fast path FIRST, so the Instance answered |
| `call_method_with_values` (the interpreter's own entry) | never delegated at all |

So the same call answered differently depending on how its name was spelled, and
the entries that did not delegate handed the Instance to by-name builtins that
answer for a Cool receiver by stringifying it — which is where `R()` came from.

`Interpreter::delegates_to_array_storage` is now that decision, once, consulted
by all of them: the interpreter entry delegates, and the dynamic opcode's native
probe stands down when it says so. The exclusions live with it —
`is_type_identity_method` (which #7305 added for `WHAT`/`WHICH`/`isa`/`does` and
every `^`-prefixed meta-method) grew the construction protocol too, because
`self.new(...)` inside a subclass method, and the `bless`/`BUILD` redispatch
under it, build an instance of the CLASS. Delegating those built an `Array`
instead and `t/array-subclass-vector.t` died with "bless can only be called on a
class or instance". `clone` is there for the same reason: raku's
`R.new(1,2).clone.^name` is `R`.

## What changed for the user

Every one of these now matches raku, in both the chained and the
through-a-variable spelling: `.Str` (`1 2`, was `R()`), `.Numeric` / `.Int`
(`2`, was `X::Method::NotFound`), `.join` (`12`, was `R()`), `.end` (`1`, was
`0`), `.clone.^name` (`R`, was `Array`), and `.elems` under a runtime method
name (`2`, was a stack overflow).

Two divergences in the same matrix are deliberately untouched, and both are
consistent across spellings: `.raku` renders `[1, 2]` where raku itemizes it as
`$[1, 2]`, and `.iterator`'s gist names mutsu's own iterator type.

## Coverage

`t/array-subclass-delegation-parity.t` — 27 assertions, all dual-oracled against
raku: the crash repro, a 12-method chained-vs-variable parity sweep, the
element-answering methods, construction and identity staying with the class, and
mutators still mutating the instance through both spellings.
`t/array-subclass-vector.t` (20), `t/builtin-subclass-type-identity.t` (20),
`make test` (3645 files) and a full local `make roast` (1436 files, 218962
tests) are green.
