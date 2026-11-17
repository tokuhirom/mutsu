# An operator in a block-local multi's own body resolves to the outer operator

Rakudo installs the proto it derives for a scope's `multi` candidates only once
the first candidate is complete, after its body is parsed. So in

```raku
{
    multi infix:<+>(Str $l, Int $r) { 100 + $r }
    say "a" + 1;
}
```

the `+` inside the candidate's body resolves (`.resolution` in the unit's
RakuAST) to the setting's `infix:<+>`, while `"a" + 1` after it resolves to the
block's own dispatcher. mutsu's CHECK-time resolver hoisted every routine of a
scope before walking it, so the body's `+` resolved to the routine being
declared — wrong for a CHECK-time analysis such as Red's `red-compile`.

The resolver (`src/rakuast/resolve/dispatcher.rs`) now tracks, per scope, which
routine names already have a dispatcher: a `proto` installs it before its body,
and the first `multi` of a scope with none hides its own name from operator
lookups inside its body and installs the derived dispatcher after it. Later
candidates and code after the declaration see the scope's routine as before.
Run time was never affected. Closes #17793; the regression test is
`t/rakuast/rakuast-multi-op-body-resolution.t`.
