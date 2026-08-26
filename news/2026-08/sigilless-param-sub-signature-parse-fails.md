# A sigilless parameter can carry an attached sub-signature

`sub foo(\p(Int, Str)) { ... }` was a parse error ("Confused. expected statement:
expected ')'") even though the sigiled equivalent `sub foo(@p (Int, Str))` had
worked for a long time.

## Root cause

`parse_param_inner`'s sigilless (`\name`) branch in
`src/parser/stmt/sub_param/param_inner.rs` parsed the name, then went straight
on to `is …` traits, a `where` clause and a default value. It never checked for
the optional `( … )` sub-signature that every sigiled branch in the same
function checks for, so the `(` after `\p` fell through to the enclosing
signature parser, which expected the parameter list to end there.

The fix parses the sub-signature right after the name (whitespace-tolerant, so
both `\p(Int, Str)` and `\p (Int, Str)` work) and stores it in the same
`ParamDef::sub_signature` slot the sigiled forms use. The binder needed no
change: its generic positional path already calls
`bind_sub_signature_from_value` whenever `pd.sub_signature` is set.

## A second, deeper bug the ticket's own repro exposed

The ticket's second repro,

```raku
sub bar(\p(Int $y where * > 5, Str $s?, *%h)) { put p.raku; put $s // "undefined" }
bar((42, life => 40, universe => 41));
```

still failed after the parse fix, with `Type check failed for s: expected Str,
got Pair` — and so did the *sigiled* twin `sub bar(@p (Int $y, Str $s?, *%h))`,
which had never worked either.

Binding a sub-signature against a list goes through that list's `Capture`, and
rakudo's `List.Capture` files **every** `Pair` element under the named lane,
whatever its flavour: `(1, x => 2).Capture` is `\(1, :x(2))`, and so is
`(1, $p).Capture` for a variable-held pair. Rakudo is strict about it —
`sub f(@p ($a, $b)) { }; f((1, x => 2))` reports "Too few positionals … got 1",
and `f(@p ($a, $b?))` reports "Unexpected named argument 'x'".

mutsu's `bind_sub_signature_from_value` (`src/runtime/types/signature.rs`)
offered those pairs *both* positionally and by name, so a positional parameter
grabbed the first pair before the named/slurpy-hash parameters ever saw it. It
now drops `Pair`/`ValuePair` elements from the positional lane when the
destructure target is a list (`Array`/`Seq`/`Slip`) — `named_values_from_unpack_target`
already surfaced them by name. A target that *is* itself a pair keeps its single
positional slot, because destructuring a `Pair` by its own key/value parts
(`-> (:$key, :$value)`) is a different rule.

Pinned by `t/signature-binding-gaps.t`.
