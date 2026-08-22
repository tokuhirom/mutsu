# Sigilless parameter with an attached sub-signature (`\p(Int, Str)`) fails to parse

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Language/signatures.rakudoc:1151` and `:1159`).

## Repro 1

```raku
sub foo(\p(Int, Str)){
   put "called with {p.raku}"
};
foo((42, "answer"));
```

- raku: `called with (42, "answer")`
- mutsu: parse error —
  ```
  ===SORRY!=== Error while compiling
  Confused. expected statement: expected ')'
  at ...:1
  ------>sub foo(\p(Int, Str)){
         ^
  ```

## Repro 2

```raku
sub bar(\p(Int $y where * > 5, Str $s?, *%h)) { put p.raku; put $s // "undefined"; }
bar((42, life => 40, universe => 41));
```

- raku: `(42, :life(40), :universe(41))` then `undefined`
- mutsu: same parse error shape as above.

## Root cause hypothesis

A parameter's attached sub-signature (destructuring the argument, e.g. `@p (Int, Str)` for a
sigiled positional) already parses correctly for sigiled parameters — confirmed working:

```raku
sub foo(@p (Int, Str)){ put "called with {@p.raku}" };
foo((42, "answer"));   # mutsu: called with (42, "answer")  -- OK
```

So sub-signature parsing itself exists (`ParamDef.sub_signature` per
`todo/tickets/parameter-sub-signature-and-modifier-attrs-missing.md`, which covers a *different*
gap — the `.sub_signature`/`.modifier` reflection accessors on `Parameter`, not this parse
failure). The bug here is narrower: the parser doesn't recognize a sub-signature immediately
following a **sigilless** (`\name`) parameter — `\p(Int, Str)` — even though the sigiled form
`@p (...)` works. Likely the sigilless-parameter parsing path doesn't call into the same
"check for a following `(` sub-signature" logic that the sigiled-parameter path uses.

## Affected files (starting point)

- Parameter/signature parsing in `src/parser/` — locate where a sigiled parameter's optional
  sub-signature `(...)` is recognized after the parameter name, and where sigilless (`\name`)
  parameters are parsed, to see why the latter doesn't check for the same trailing `(`.
