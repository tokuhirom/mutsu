# `:P5` / `:Perl5` regexes are gone, and so is the pcre2 dependency

Rakudo 2026.09 and roast dropped Perl 5 regexes (Raku/problem-solving #378; rakudo #6708,
roast `41679797`), and mutsu now follows them (#10225, ADR-0138). `m:P5/.../`, `rx:Perl5/.../`,
`s:P5/.../.../` and `S:Perl5/.../.../` are compile-time errors, `X::Syntax::Regex::Adverb` with
the message "Adverb P5 not allowed on m" — the same error Rakudo gives any adverb a construct does
not take. mutsu's own "Unsupported regex adverb :foo" message for unknown adverbs is replaced by
that Rakudo form too, with `adverb` and `construct` set.

A scan of the latest version of all 2558 fez and REA distributions found six that use `:P5`
(≈0.23%), and their `:P5` test files already fail on Rakudo 2026.09, so the ecosystem parity
ledger loses nothing.

The PCRE2-backed engine is deleted with everything that only it used: the P5 pattern rewriting
and interpolation, the P5 delimiter scanner mode, the `perl5` flag on the substitution AST nodes,
opcodes and `RegexAdverbs`, the legacy `:P5` substitution lowering, and the separate
`positional_slots` capture axis. The `pcre2` crate — the only C library mutsu built through `cc`
apart from the vendored libffi — is gone from `Cargo.toml`, and `libpcre2` from the release
workflow and the Docker image.
