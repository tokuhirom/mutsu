# `Parameter.sub_signature` and `Parameter.modifier` methods are unimplemented

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Parameter.rakudoc:347` and
`:395`).

## Repro 1 — `.sub_signature`

```raku
my Signature $sig = :(@array ($first, *@rest), @other);
say $sig.params[0].sub_signature;     # OUTPUT: «($first, *@rest)␤»
say $sig.params[1].sub_signature;     # OUTPUT: «(Signature)␤»
```

- raku: `($first, *@rest)` then `(Signature)`
- mutsu: `No such method 'sub_signature' for invocant of type 'Parameter'`

## Repro 2 — `.modifier`

```raku
my Signature $sig = :(Str:U $a, UInt:D $b, $c);
say $sig.params[0].modifier; # OUTPUT: «:U␤»
say $sig.params[1].modifier; # OUTPUT: «:D␤»
say $sig.params[2].modifier; # OUTPUT: «␤»
```

- raku: `:U` then `:D` (then empty)
- mutsu: `No such method 'modifier' for invocant of type 'Parameter'`

## Root cause / extension point

`Parameter` reflection objects are built by `build_parameter_attrs()` in
`src/value/signature.rs` (function starts at line 497) — it fills a `HashMap<String, Value>` of
attribute name to value from a `SigParam`, wrapped as a `Value::Instance` of class `Parameter` by
`sig_param_to_parameter_instance[_with_owner]` (lines 433-450). Because `Parameter` is a built-in
(not in `user_declared_classes`), method dispatch in `dispatch_instance_and_fallback()`
(`src/runtime/methods_instance_ops.rs`, fn starts line 192, fallback-accessor block around
lines 1345-1377) auto-generates a zero-arg accessor for any key present in that attrs map — so
adding `sub_signature`/`modifier` support is purely a matter of populating those two keys in
`build_parameter_attrs`, not adding new dispatch machinery.

Currently populated keys (for reference): `name` (509), `type` + `type_captures` (544-545),
`named` (553), `slurpy` (554), `sigil` (555), `multi-invocant` (556), `readonly`/`rw`/`raw`/`copy`
(563-566), `optional` (569), `invocant` (572), `positional` (575-578), `capture` (581).

- `sub_signature`: `ParamDef` already carries `sub_signature: Option<Vec<ParamDef>>` on the AST
  side (`src/ast.rs` line 73) but it is never read into `build_parameter_attrs`. Needs conversion
  to a `Signature` value (or `(Signature)` type object when absent, per repro 1's second line).
- `modifier`: needs to derive the `:U`/`:D`/(empty) string from the parameter's type-smiley
  definedness constraint (the same information that already drives type checking for `Str:U`/
  `UInt:D`-style parameters).

## Affected files

- `src/value/signature.rs` — `build_parameter_attrs()`
- `src/ast.rs` — `ParamDef` (confirm `sub_signature` and definedness-smiley fields already carry
  the needed data)
