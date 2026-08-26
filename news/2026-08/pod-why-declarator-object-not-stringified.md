# `say &foo.WHY` prints the documentation text

`say &cast.WHY` printed `Pod::Block::Declarator.new` where rakudo prints the
`#|` leading and `#=` trailing comment text joined by a newline. Fixing it
turned up two further declarator-collection bugs that the unimplemented
stringification had been hiding.

## Root cause 1 — `.gist` was not wired to the doc text

`.Str` on a `Pod::Block::Declarator` already rendered the doc text (there is a
dedicated arm in `value/display.rs`), but `say` uses `.gist`, which fell
through to the generic `TypeName.new` instance repr. `default_instance_repr`
returns the doc text for `gist` on a `Pod::Block::Declarator` now, matching
rakudo, where `.Str` and `.gist` agree. `.raku` keeps the instance repr — its
`WHEREFORE` is the documented routine, whose own repr embeds an object address.

## Root cause 2 — a trailing `#=` after a multi-line body attached to nothing

```raku
#| Initiate a specified spell normally
sub cast(Spell $s) {
  do-raw-magic($s);
}
#= (do not use for class 7 spells)
```

produced only the leading half. The declarator scanner
(`Interpreter::collect_doc_comments`) is line-based: a routine's body
statements are not declarations, so they cleared `last_declarant`, and the
closing `}` explicitly cleared it again. Only the single-line
`sub b() { 1 }` shape ever worked. The scanner now keeps a brace-depth-keyed
stack of declarations whose block body is open, so a closing brace restores
the declaration its body belonged to. Nested non-declaration blocks (an `if`
inside the body) push nothing, so their braces restore nothing and cannot
steal the attachment.

## Root cause 3 — the bracketed block form ignored the required no-space rule

`#= (do not use for class 7 spells)` came out as
`do not use for class 7 spells`: both `parse_doc_comment` and
`extract_inline_trailing` trimmed the text before probing for an opening
bracket, so a *spaced* `#| (text)` was misread as the bracketed block form and
had its parentheses eaten. In Raku the bracket must follow the `#|`/`#=`
immediately — `#|(text)` is a block whose payload is `text`, while
`#| (text)` is a one-line comment whose payload keeps its parentheses. Both
sites probe the untrimmed remainder now.

## Result

The ticket's repro reproduces rakudo's output exactly, and so do the spaced /
tight variants of every bracket pair. All 27 `roast/S26-documentation/` files
still pass (`block-trailing.t` pins `#={...}` / `#=(...)` tight forms, and
asserts the exact `$=pod` entry count, so a spurious attachment would have
shown up there). Pinned by `t/pod-object-model.t`, which passes under both
`raku` and `mutsu`.
