# `"${:a}"` is a hash composer, not a Perl 5 dereference

`${...}` is two different constructs wearing the same spelling. One is Perl 5's
scalar dereference, which Raku diagnoses with `X::Obsolete`; the other is Raku's
own contextualizer — rakudo's grammar has

```
token contextualizer {
    [ <sigil> '(' ~ ')'    <coercee=sequence>
    | <sigil> <?[ \[ \{ ]> <coercee=circumfix>
    ]
}
```

so `${a => 1}` is the `{...}` circumfix (a hash composer) in item context, and
`@{:a, :b}` is the same circumfix in list context. Rakudo keeps the P5 diagnosis
from swallowing them with an explicit guard on `special_variable:sym<${ }>`:

```
<!{ $<text> ~~ / '=>' || ':'<:alpha> || '|%' / }>
<!{ $<text> ~~ / ^ \s* $ / }>
```

where `$<text>` is the source between the brace and the *first* `}`.

mutsu applied a rough version of that guard outside strings — `${a => 1}` worked
because the parse produced an `Expr::Hash` — but the string-interpolation path in
`parser/primary/string/interp_var.rs` had none of it: any `${` in a double-quoted
string was rewritten into a thrown `X::Obsolete`. So

```raku
say "${:group("MyGroup")}";   # rakudo: group<TAB>MyGroup
```

died mid-file with *Unsupported use of ${:group("MyGroup")}*. That is line 31 of
vCard::Parser 0.0.2's `t/03-actions.rakutest`, which is why that file stopped at
7 of its 9 planned tests.

The guard is now rakudo's, spelled once as `var::perl5::is_brace_contextualizer`
and consulted from all three sites that had to choose between the two readings:
the bare `${...}` (`container/sigil_context.rs`), the bare `@{...}`
(`var/sigil_vars.rs`), and string interpolation. The bare `@{...}` gained the
list contextualizer it never had — `@{:a, :b}` used to parse as a subscript and
quietly evaluate to `(Any, Any)`.

Interpolation is deliberately asymmetric between the sigils, because rakudo is:
`"${:a}"` interpolates the itemized hash, while in `"@{:a}"` the `@` is not an
interpolation trigger at all — it stays a literal `@` and the `{...}` is an
ordinary block interpolation. Both spellings are pinned, along with the P5
forms (`${$scalar}`, `@{@array}`, `"${1}"`) that must still be `X::Obsolete`, in
`t/lang/quoting/interp-brace-contextualizer.t`.

vCard::Parser goes from `partial` (3 of 4 baseline files, 32 of 34 assertions) to
`green`: 4 of 4 files, 34 of 34 assertions, matching rakudo exactly.
