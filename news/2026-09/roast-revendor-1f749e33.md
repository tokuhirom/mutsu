# roast re-vendored at 1f749e33; leading `#|` inside an initializer

`roast/` was re-vendored from `85a87909` (2026-09-07) to `1f749e33`
(2026-09-29). Upstream:

- removed every `:P5`/`:Perl5` regex test (`S05-modifier/Perl_0.t` ..
  `Perl_10.t`, and the `$~P5Regex` check in `S28-named-variables/slangs.t`),
  following Rakudo dropping the Perl 5 regex slang once RakuAST became the
  default compiler. The eleven deleted files left `roast-whitelist.txt` and
  `TODO_roast/raku-baseline.tsv`;
- added 17 longest-token-matching cases for character-class methods
  (`<alnum>`, `<digit>`, ...) to `S05-metasyntax/longest-alternative.t` —
  mutsu already passes them;
- moved the leading declarator docs of the anonymous sub and block tests in
  `S26-documentation/{why-leading,why-both,block-leading}.t` into the
  initializer (`my $anon-sub = #| Anonymous` then `anon Str sub {};` on the
  next line), because a `#|` above `my $x = ...` documents the variable.

The last change broke three whitelisted files: the declarator-doc scanner in
`src/runtime/io_doc.rs` only recognized a `#|` that starts a line, so the
inline doc was dropped and the anonymous-sub numbering after it shifted. A
new normalization step (`src/runtime/io_doc_hoist.rs`) moves a `#|` that
directly follows `=`, `:=` or `::=` onto its own line and joins the
initializer's code onto the next one, which the scanner already attaches to
the anonymous sub or block. Regression test:
`t/lang/pod-declarator-leading-in-initializer.t`.
