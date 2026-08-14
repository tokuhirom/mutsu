# `trait_mod:<does>` is not a callable sub; no `Variable` MOP object to apply it to

## Symptom

`Hash::Restricted`'s test suite (`roast/`-style dist sweep, un-triaged
`test_die` row in
[todo/tickets/dist-test-suite-failures-batch.md](../tickets/dist-test-suite-failures-batch.md))
dies immediately on `use Hash::Restricted;`:

```
Unknown function: trait_mod:<does>
  in sub trait_mod:<is> at lib/Hash/Restricted.rakumod line 75
```

Raku: loads and all 32 subtests pass.

## What the dist needs

`lib/Hash/Restricted.rakumod` defines a custom `is restricted` trait that
dynamically mixes a role into the DECLARED VARIABLE's type (not an instance)
at `my %h is restricted = ...` declaration time:

```raku
multi sub trait_mod:<is>(Variable:D \v, Bool:D :$restricted!) is export {
    die "..." unless v.var.WHAT ~~ Map;
    my $name = v.var.^name;
    if $restricted {
        trait_mod:<does>(v, restrict-current);   # <-- calls trait_mod:<does> as a plain sub
        v.var.WHAT.^set_name("$name\(restricted)");
    }
}
```

This requires, as CORE-provided (not this dist's own) machinery:

1. A real `Variable` MOP object type, produced when a `\v` capture parameter
   is typed `Variable:D` in a `trait_mod:<is>` candidate — mutsu already has
   a partial notion of this (`type_matches_value`/`type_matching_static.rs`
   match `Variable` as a type CONSTRAINT against `varref_from_value`), but
   there is no actual `Variable` instance with a `.var` accessor exposing the
   underlying container.
2. `trait_mod:<does>` as a genuinely **callable multi sub** (not just special
   parser/compiler handling for the `does` trait keyword) that mixes a role
   into a `Variable`'s declared type at runtime.
3. `.WHAT.^set_name(...)` on the resulting (possibly type-object) value,
   renaming the class for introspection (`.^name` becomes `"Hash(restricted)"`).

## Why this needs a design pass

This is dynamic-MOP territory of a similar shape to the retired native
`monitor` declarator stopgap (see `news/2026-08/exporthow-declare-mop.md`) —
it needs real `Variable` reflection plus a working `does`-as-a-sub entry
point, not a hardcoded trait handler. Per `CLAUDE.md`'s BATTERIES.md rung-3
ban, this should grow the interpreter's MOP rather than be special-cased for
this one dist. Before starting: check whether any OTHER un-triaged/triaged
dist in the batch sweep needs `Variable:D \v` + `trait_mod:<does>` (grep the
fez sample corpus) to judge whether it is worth the investment now or should
wait for a broader MOP campaign.

## Repro

```raku
class Foo { }
multi sub trait_mod:<does>(Mu \v, Mu \r) is export {
    say "would mix {r.^name} into {v.VAR.name}";
}
my $x;
trait_mod:<does>($x, Foo);
```

mutsu: `Unknown function: trait_mod:<does>`. Not yet verified against raku
(this synthetic repro is illustrative of the missing-callable-sub gap only;
the real dist's `Variable:D \v` signature is the actual blocker — see above).
