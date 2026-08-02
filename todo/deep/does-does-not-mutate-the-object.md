# `does` copies instead of mutating the object

Raku's `does` operator mixes a role into *the object*, so every reference to it
sees the mixin. (`but` is the copying one.) mutsu instead builds a fresh
`ValueRepr::Mixin` wrapper and rebinds the left-hand variable, so any other
reference to the same object is unaffected:

```raku
role Marker { }
class C { }
my $x = C.new;
my $y = $x;
$y does Marker;
say $y ~~ Marker;   # raku: True    mutsu: True
say $x ~~ Marker;   # raku: True    mutsu: False   <-- the divergence
```

Passing through a routine loses it entirely, because the callee rebinds only its
own parameter:

```raku
sub apply($p) { $p does Marker }
my $c = C.new;
apply($c);
say $c ~~ Marker;   # raku: True    mutsu: False
```

## Why it matters

This is the root blocker under
[`todo/tickets/parameter-trait-mixin-does-not-persist.md`](../tickets/parameter-trait-mixin-does-not-persist.md),
and therefore under Cro::HTTP's router. Every custom parameter trait in
`Cro::HTTP::Router` is written exactly like the routine case above:

```raku
multi trait_mod:<is>(Parameter:D $param, :$query! --> Nil) is export {
    $param does Cro::HTTP::Router::Query;
}
```

`check_param_custom_traits` (`src/vm/vm_register_sub_ops.rs`) now calls that
candidate with a real `Parameter` at declaration time, but the mixin dies with
the callee's binding, so mutsu has no way to learn *which* roles a trait
applied. The declared return type is `--> Nil`, so reading the call's result
does not help either. Until `does` mutates, there is nothing to record onto the
`SigParam`, and a route declared `get -> 'search', :$min-price is query = 0 { }`
cannot be told apart from an ordinary named parameter.

## Why it is deep

`ValueRepr::Mixin(Arc<Value>, Arc<HashMap<String, Value>>)` is a *wrapper*: the
mixin lives outside the instance, so it cannot be shared by other references to
the same `Instance`. Making `does` mutate means moving the mixin state inside
the instance — a role list (and attribute overrides) reachable through the
instance's own `Gc`, updated in place — and then teaching every consumer to read
it there: `~~` / `type_matches_value`, `.^name` (`C+{Marker}`), `.^roles`,
`.^mro`, method dispatch (mixin methods must win over the class's), `.raku` /
`.gist`, serialization, and the `but` operator, which must keep copying.

Two shapes to consider:

1. Keep `Mixin` for non-`Instance` values (an `Int` or `Str` genuinely cannot be
   mutated in place) and add in-instance role state used only for `Instance` /
   `CustomTypeInstance`. Smaller blast radius, but two mechanisms to keep in
   sync — the kind of dual mechanism the working agreements warn about.
2. Give every mixable value an identity that `does` can mutate. Cleaner, much
   larger, and interacts with the NaN-boxed representation and ADR-0001's
   container/scalar type filter.

Whichever shape wins, the parameter-trait consumer above needs a second step
after it: record the roles a trait applied onto the `SigParam`, so the
`Parameter` objects `.signature.params` re-materializes carry them.

## Related, found while investigating

`$c does Q;` — where `Q` is an ordinary role name — mis-lexes as the `Q` quoting
language and swallows the rest of the statement; see
[`todo/tickets/bareword-Q-after-does-lexes-as-quote.md`](../tickets/bareword-Q-after-does-lexes-as-quote.md).
Unrelated to the semantics above, but it makes any `does Q` repro lie.
