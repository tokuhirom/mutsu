# `$=pod` items aren't proper `Pod::*` block objects — `.contents` fails, wrong type

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Language/pod.rakudoc:908`).

## Minimal repro

```raku
=begin pod

=head1 This is a head1 title

This is a paragraph.

=end pod

for $=pod -> $pod-item {
    for $pod-item.contents -> $pod-block {
      $pod-block.raku.say;
    }
}
```

- `raku`:
  ```
  Pod::Heading.new(level => 1, config => {}, contents => [Pod::Block::Para.new(config => {}, contents => ["This is a head1 title"])])
  Pod::Block::Para.new(config => {}, contents => ["This is a paragraph."])
  ```
- `mutsu` (`target/debug/mutsu`): dies with
  `No such method 'contents' for invocant of type 'List'`.

## Root cause hypothesis

Iterating `$=pod` in mutsu yields elements that are plain `List`s (each wrapping a single
`Pod::Block::Named` instance), not the flattened top-level sequence of `Pod::*` block
objects real Raku produces. A follow-up probe:

```raku
for $=pod -> $pod-item {
    say $pod-item.WHAT;   # mutsu: (List)  -- should be a Pod::* type, e.g. (Pod::Heading)
    say $pod-item.raku;    # mutsu: $(Pod::Block::Named.new,)
}
```

confirms two separate problems in mutsu's `$=pod` construction:

1. Each top-level pod document element is wrapped an extra level deep in a 1-element `List`
   instead of being the block object itself.
2. `=head1 ...` (and presumably other `=headN` directives) produces a generic
   `Pod::Block::Named` instead of the specific `Pod::Heading` type (with its `level`
   attribute) that real Raku's Pod parser emits, and that block object has no `.contents`
   method at all (since it's really a plain `List`, not a Pod block instance).

## Affected files (starting point)

- Wherever `$=pod` / the compile-time Pod-tree construction happens (grep for `Pod::Block`,
  `"=pod"`, or the Pod-parsing pass in the parser/compiler) — needs to (a) not double-wrap
  each top-level element in a `List`, and (b) special-case `=headN` blocks into a proper
  `Pod::Heading` object carrying `level`/`config`/`contents`, matching the other already-
  working `Pod::Block::*` types.
