# Imported string- and set-operator candidates bind lexically, and `~=` asks user `infix:<~>`

A `use` inside a block that imports user candidates of `~`, `eq`, `ne`,
`lt`, `gt`, `le`, `ge`, `leg` or an ASCII set operator (`(elem)`, `(cont)`,
`(|)`, `(&)`, `(+)`, `(.)`, `(-)`, `(^)`) now adds them to the operator inside that
block only (ADR-8751 stage 2, slice 6). Before, the import put them in the
routine registry for the block's dynamic extent, so a reduction, a hyper or
`&infix:<leg>` in a routine written outside the block, but called from
inside it, picked them up:

```raku
sub outside($a, $b) { [~] $a, $b }
{ use M; outside($x, $y) }   # rakudo: the built-in `~`; mutsu ran M's candidate
```

The ops behind these operators (`infix_concat`, the string comparisons and
`leg`) now ask the user candidates before stringifying their operands, the
way the arithmetic ops already did. That also fixes `$x ~= $y` ignoring a
user `infix:<~>` candidate the program declares itself (#13791), and the
`X`/`Z`/`R` meta-ops and `~=` read the block's imports too.

`.sort(&infix:<leg>)` and `.sort(&infix:<cmp>)` inside such a block treated
the operator value as a key mapper, because its signature is a capture; the
dispatcher now reports the operator's arity to `sort`.

An imported `(elem)` or `(cont)` family was also bound without the built-in
behind it, so `{ use M; 1 (elem) (1, 2) }` died with "Cannot resolve caller";
the built-in now answers what the candidates do not take.
