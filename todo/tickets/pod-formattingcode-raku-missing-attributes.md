# `Pod::FormattingCode.raku` prints only the bare class name, omitting all attributes

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Language/glossary.rakudoc:1024`).

## Root cause

`Pod::FormattingCode`'s `.raku` representation should round-trip its constructor call, showing
`type`, `meta`, `config`, and `contents` (mirroring how other Pod:: block objects' `.raku` already
work). mutsu's `.raku` for `Pod::FormattingCode` prints only `Pod::FormattingCode.new` with no
attribute list at all — the generic default-`.raku`-for-an-instance fallback is presumably being
used instead of a type-specific (or attribute-introspecting) `.raku` for this Pod class.

## Minimal repro

```raku
=begin pod
C<foo>
=end pod
say $=pod[0].contents[0].contents.raku;
```
- `raku`: `[Pod::FormattingCode.new(type => "C", meta => [], config => {}, contents => ["foo"])]`
- `mutsu`: `(Pod::FormattingCode.new,)`

(Note the outer container also differs — `[...]` vs `(...)` — but the primary bug is the missing
attribute list; investigate the outer-container mismatch too if it turns out not to be a separate
existing issue.)

## Affected files (starting point)

- Wherever `Pod::FormattingCode` (and sibling `Pod::*` block classes) are defined/registered —
  search for `Pod::FormattingCode` in `src/runtime/` (pod parsing/AST-to-Pod-object construction)
  to find where its attributes (`type`/`meta`/`config`/`contents`) are stored, then check whether
  the generic instance `.raku` builtin (`src/builtins/methods_0arg/raku_repr.rs`) fails to
  enumerate them because they're stored in some non-standard internal representation rather than
  ordinary public attributes.
