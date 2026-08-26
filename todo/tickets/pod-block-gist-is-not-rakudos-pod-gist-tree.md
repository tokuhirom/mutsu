# `Pod::Block.gist` renders the instance repr, not rakudo's indented pod-gist tree

Left open by the 2026-08-26 Pod object-model round
(`news/2026-08/dollar-equals-pod-item-not-iterable-block-object.md` and its two
siblings), which fixed `.raku`, the block types, the attribute set and
`Pod::Block::Declarator.gist`, but deliberately did not touch the *block* gist.

## Current state

`say $=pod[0]` in mutsu prints the generic instance repr, now that the Pod
classes declare their attributes:

```
Pod::Block::Named.new(name => "pod", config => {}, contents => [Pod::Heading.new(level => 1, ...)])
```

rakudo prints an indented tree instead (`Pod::Block.pod-gist`, two spaces per
level, the block's class name followed by a Hash `.raku` of its non-empty
`config`/`name`/`level`/`caption`/`type`/`term`, and plain-string contents
indented in place):

```
Pod::Block::Named{:name("pod")}
  Pod::Heading{:level("1")}
    Pod::Block::Para
      Head one
  Pod::Block::Para
    Plain para with 
    Pod::FormattingCode{:type("C")}
      code
    .
```

`$=pod.gist` is the same tree wrapped in `[`/`]`.

## Why it was not done

Measured against `raku` v2026.06, the format is only partly reproducible:

- The `config` entry is stringified through an *itemized* `.raku`, so
  `=begin pod :nested` renders as `:config("\$\{:nested(Bool::True)}")` — a
  rakudo internal artifact rather than a specified shape.
- `Pod::Config` is not a `Pod::Block` subclass in rakudo, so it has no
  `pod-gist` at all and falls back to `Mu.gist`:
  `Pod::Config<6084526099248>` — a per-run object address, which no test could
  assert against.
- `Pod::Block::Table`'s rows render via `.raku` (`["a", "b"]`), not the
  `.gist` the surrounding code suggests.

Nothing consumes it: the vendored `Pod::To::Text` never calls `.gist`, and no
roast `S26-documentation/` test does either (checked — the whole directory
contains no `gist`/`.raku`/`.perl` call on a Pod object).

## If picked up

Implement `pod-gist` in Rust as a `Pod::*`-instance arm of
`default_instance_repr`'s `gist` path, mirroring rakudo `src/core.c/Pod.pm6`:
`' ' x $level`, the class name, the non-empty subset of
`<config name level caption type term>` collected into a Hash whose values are
`$thing.raku` when `Iterable` and `$thing.Str` otherwise, then `"\n"`, then
each content either recursed at `$level + 2` (when it is a Pod block) or
`.indent($level + 2)`. Decide explicitly what to do about the three
non-reproducible details above before writing the test.
