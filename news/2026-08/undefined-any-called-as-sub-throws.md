# Calling an undefined value as a function returns its arguments instead of throwing

Found by the doc-diff harness (`Language/structures.rakudoc:233`).

## What was wrong

```raku
my $x;
say $x("hi");     # raku: hi        mutsu: died
say $x(1, 2, 3);  # raku: (1 2 3)   mutsu: died
```

mutsu raised `Impossible coercion from 'Str' into 'Any': no acceptable
coercion method found`. (The ticket recorded an older spelling of the same
failure, `No such method 'CALL-ME' for invocant of type 'Nil'`.)

The ticket's hypothesis was that this is a `CALL-ME` fallback. It is not —
and checking `raku` first is what showed that, because rakudo's behaviour has
a shape no "return the args" special case would produce:

| call | raku |
|---|---|
| `$x("hi")` | `hi` |
| `$x(1, 2, 3)` | `(1 2 3)` |
| `$x()` | **dies**: `No such method 'CALL-ME' for invocant of type 'Any'` |

Those are not a fallback's answers; they are a *coercion's*. `$x` is the `Any`
type object, so `$x(...)` is `Any(...)`, and coercing a value to a type it
already conforms to is the identity. With no argument there is nothing to
coerce, so it really does fall through to `CALL-ME` and die — which mutsu was
already getting right.

The same rule shows up without any undefined variable at all:
`Mu("z")` is `"z"` and `Any(1, 2)` is `(1 2)`.

## The fix

The coercion branch in `runtime/builtins_operators_fallback.rs` now answers a
value that already satisfies the target type with the value itself. It is
checked **last**, immediately before the `X::Coerce::Impossible` it replaces,
so a real `COERCE` / `new` / `.<Target>` still wins for a value that happens
to conform. The whole branch is gated on a non-empty argument list, so the
zero-argument case keeps falling through to `CALL-ME` and dying, as rakudo
does.

All three shapes above now match `raku` exactly, including the error text of
the one that throws.
