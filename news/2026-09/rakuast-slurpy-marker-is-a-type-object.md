# A RakuAST slurpy marker is a type object, not a node

Rakudo builds **no node** for a slurpy parameter's marker: the
`RakuAST::Parameter::Slurpy::*` type object itself is what `$!slurpy` holds.
mutsu normalized the other way round — it took the type object on the way in
and stored an empty node of that class. The two render identically inside the
parent's gist, so `.gist` of a whole tree matched byte for byte and the
difference hid on the field itself:

```
my $p = Q[sub f(*@c) { }].AST.statements[0].expression.signature.parameters[0];
say $p.slurpy.defined;   # mutsu: True    raku: False
say $p.slurpy.gist;       # mutsu: RakuAST::Parameter::Slurpy::Flattened
                          #  raku: (Flattened)
say $p.slurpy === RakuAST::Parameter::Slurpy::Flattened;
                          # mutsu: False   raku: True
```

The `.defined` answer was the damaging one. On rakudo the question "is this
parameter slurpy?" is *which class of type object the field holds*
(`$p.slurpy !=== RakuAST::Parameter::Slurpy`), never definedness — so mutsu
answered `True` for every parameter carrying a marker where rakudo answers
`False` for all of them, which is the exact opposite of useful. The identity
comparison that rakudo's own idiom relies on could not work at all, because a
node is never identical to a class.

## The fix

`normalize_slurpy_marker` now normalizes **to** the type object rather than away
from it, and the two constructing sites (`convert.rs`'s `slurpy_parameter`, and
`formatter.rs`'s synthesised `-> *@args` block) store that value directly. Both
input spellings are still accepted: rakudo's `.new` takes only the type object,
which is what `t/rakuast/rakuast-construct-rich-parameters.t` already passes,
but a node of the same class names the same marker unambiguously and there was
nothing to gain from rejecting it. `slurpy_marker_class` is the one place that
reads either spelling, so the lowerer no longer matches on a node class.

Because the field's value is now the same `Package` the bareword
`RakuAST::Parameter::Slurpy::Flattened` already evaluated to, `.defined`,
`.gist`, `.raku`, `===`, `=:=` and `~~` all came out right with no per-method
work — mutsu's type-object surface was already correct, the field just was not
holding one.

One renderer change went with it: a type-object field value renders as its bare
class name. That is genuinely different from how a type object gists alone —
rakudo's `Parameter` gist shows
`slurpy => RakuAST::Parameter::Slurpy::Flattened` while `.slurpy.gist` is
`(Flattened)` — so the two spellings had to be kept apart rather than unified.

Pinned by `t/rakuast/rakuast-slurpy-marker-type-object.t`: 19 assertions
measured against rakudo, covering both markers and the base class, `.defined`,
`.gist`, `.raku`, `===`/`=:=` in both directions, the documented base-class
idiom, `~~` membership, the parent's gist rendering, and a hand-built parameter
keeping the type object it was given.

Closes [#8157](https://github.com/tokuhirom/mutsu/issues/8157).
