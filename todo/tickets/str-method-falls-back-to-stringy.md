# `.Str` falls back to `Stringy` for a class that defines only `Stringy`

```raku
class D { has $.t; method Stringy { "S:" ~ $!t } }
my $d = D.new(t => "q");
say ($d,).join("");   # raku: D<6169564731160>   mutsu: S:q
say ~$d;              # raku: S:q                mutsu: S:q     (agree)
```

Raku's `prefix:<~>` calls `Stringy`, which defaults to `Str`. The reverse is
not true: `.Str` on a class that defines only `Stringy` gets `Mu.Str`, so it
renders the default `D<address>`. mutsu's `.Str` dispatch tries `Stringy`
first, so anything that stringifies *by calling `.Str`* — `.join`, and now the
list-stringification path of `news/2026-09/list-str-calls-element-str.md` —
answers the `Stringy` result where raku answers the default.

Found 2026-09-02 while making list stringification dispatch each element's
`Str`. The new path deliberately reuses `.join`'s exact call
(`call_method_with_values(elem, "Str")`), so the two agree with each other;
`t/list-str-calls-element-str.t` pins that agreement rather than a literal, and
passes under `raku` too.

## Why it is not a one-line fix

The `Str`→`Stringy` fallback is deliberate in several places (the prefix-`~`
opcode, `coerce_stringy_operand`, `stringify_test_value` all try `Stringy`
before `Str`, and that IS correct for those — they implement `~`, not `.Str`).
Only the sites that implement the `.Str` *method* should stop falling back.
Separating them needs an audit of every `has_user_method(cn, "Stringy")` /
`try_compiled_method_or_interpret(.., "Stringy")` caller against which Raku
operation it implements.

Low impact on its own: a class defining `Stringy` without `Str` is rare (all of
`modules/` has none). Worth doing when the string-coercion sites are touched
for another reason.

## Repro

```
raku  -e 'class D { has $.t; method Stringy { "S:" ~ $!t } }; say (D.new(t=>"q"),).join("")'
mutsu -e 'class D { has $.t; method Stringy { "S:" ~ $!t } }; say (D.new(t=>"q"),).join("")'
```
