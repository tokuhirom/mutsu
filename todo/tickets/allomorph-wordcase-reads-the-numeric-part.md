# `.wordcase` on an allomorph reads its numeric part, and rakudo's return type is the allomorph itself

Found while fixing `.ord`/`.ords` on allomorphs (see
[news/2026-08/main-allomorph-arg-name-corrupts-later-intstr-new.md](../../news/2026-08/main-allomorph-arg-name-corrupts-later-intstr-new.md)).
A sweep of the 0-arg string methods on `IntStr.new(0, "zero one")` found three
divergences; `ord` and `ords` were fixed, `wordcase` was left because its
correct *return type* needs a decision.

## Minimal repro

```raku
my $a = IntStr.new(0, "zero one");
say $a.wordcase.raku;
# raku : IntStr.new(0, "Zero One")
# mutsu: "0"
```

## Two separate questions

1. **Which component does it read?** mutsu reads the inner *number* (hence
   `"0"`), because `wordcase` is missing from the allomorph string-method list
   in `src/builtins/methods_0arg/mod.rs` (the `if let Some(str_val) =
   mixins.get("Str")` match arm) and so falls through to the generic mixin
   delegation, which hands the method the inner `Int`. Adding it to that list
   makes mutsu return `"Zero One"` — the right characters.

2. **What type comes back?** This is the part that needs deciding. Every other
   member of that list returns a plain `Str` in rakudo *and* in mutsu — `$a.uc`
   is `"ZERO ONE"`, `$a.trim` is `"zero one"`, `$a.flip` is `"eno orez"`. But
   rakudo's `wordcase` alone hands back `IntStr.new(0, "Zero One")`, i.e. it
   preserves the allomorph. Simply adding `wordcase` to the shared list would
   therefore trade a wrong *value* (`"0"`) for a wrong *type* (`Str` instead of
   `IntStr`) — better, but still divergent.

Check rakudo's `Cool.wordcase` before implementing to see whether the
allomorph preservation is deliberate or an artefact of its `Str` candidate, and
whether any other `Cool` method shares it.

## Affected files

- `src/builtins/methods_0arg/mod.rs` — the allomorph string-method list (the
  `"comb" | "chars" | ... | "ord" | "ords" | ...` arm), where `ord`/`ords` were
  just added.

## Pin

`t/numeric-coercion-gaps.t` pins the fixed `.ords` behaviour next to where a
`wordcase` assertion would go.
