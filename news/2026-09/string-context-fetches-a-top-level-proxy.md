# A string context FETCHes a top-level `Proxy`

Every other value context already FETCHed a `Proxy` — arithmetic
(`eval_binary_with_junctions`), `say`/`print`/`note`, method dispatch, the
numeric/`Str` coercion ops — but the two string contexts did not:

```
my $n = 5;
my $p := Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v });
say "x$p";      # raku: x5   mutsu (before): xProxy
say 'x' ~ $p;   # raku: x5   mutsu (before): xProxy
say $p eq '5';  # raku: True mutsu (before): False
```

so a `Proxy` that read correctly everywhere else rendered as the literal word
`Proxy` the moment it was interpolated or concatenated.

Two hooks, both at the existing chokepoint for their context:
`coerce_stringy_operand` (shared by infix `~` and the string comparators
`eq`/`lt`/…) and the `StringConcat` interpolation loop. Both are top-level
FETCHes of one operand — the same shape `say` already used — with no traversal
added to any render path.

This is the *top-level* half of
`todo/tickets/list-element-proxy-not-rendered-through-fetch.md`. That ticket's
own subject — a `Proxy` sitting **inside** a rendered container, where ADR-0040
§9 deliberately keeps it (`my $l = (1, $p, 3)`; a `List`'s elements are not
containers) — is untouched and stays open: the remaining renderers (`.gist`,
`.raku`, `Value::to_display_string`) are pure `Value` methods with no
`&mut Interpreter`, and a FETCH is a call into user code, so that half still
needs the design decision the ticket describes.
