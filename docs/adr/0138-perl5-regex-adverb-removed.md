# ADR-0138: The Perl 5 regex adverb (`:P5` / `:Perl5`) is removed, following Rakudo

- **Status**: Accepted (implemented)
- **Date**: 2026-10-01
- **Related**: [#10225](https://github.com/tokuhirom/mutsu/issues/10225);
  Raku/problem-solving [#133](https://github.com/Raku/problem-solving/issues/133) and
  [#378](https://github.com/Raku/problem-solving/issues/378); rakudo
  [#6534](https://github.com/rakudo/rakudo/pull/6534) (superseded) and
  [#6708](https://github.com/rakudo/rakudo/pull/6708) (`1cdf64d61`), `0dd07126b`, `f0a240414`;
  roast `41679797`, `1f521d79`

## Context

mutsu ran `m:P5/.../`, `rx:Perl5/.../`, `s:P5/.../.../` and `S:P5/.../.../` on a second regex
engine, PCRE2 (the `pcre2` crate, behind the `native` feature). That engine was the only C library
mutsu built from source through `cc` (`pcre2-sys`), so it needed `libpcre2-dev` on every build host
(CI release jobs, the Dockerfile) and `libpcre2-8-0` at run time in the container image. It also
carried its own pattern rewriting (`regex_transform.rs`), interpolation (`regex_interpolation.rs`),
a capture-numbering axis kept apart from the Raku one (`RareCaps::positional_slots`, ADR-0016), a
`perl5` flag threaded through the AST, the `Subst`/`NonDestructiveSubst` opcodes and
`RegexAdverbs`, and a legacy `$_ = $_.subst(...)` lowering for `:P5` assignment-form substitutions.

Upstream has dropped the feature:

- Raku/problem-solving #133 (vrurg, 2019) proposed deprecating and removing Perl 5 regexes, since
  the Perl 5 → Raku migration they were added for is no longer a reason to keep them.
- Raku/problem-solving #378 (lizmat, 2023-07-30) — "The fate of P5 regexes in a RakuAST world":
  porting `:P5` to the RakuAST grammar would take significant effort for very little ecosystem use;
  P5 syntax already lacked modern features such as `\p{..}`; converting P5 patterns to Raku regexes
  is straightforward; and a public API for regex slangs (e.g. PolyglotRegexen) would be the better
  home for other regex dialects.
- rakudo #6534 (ugexe, opened 2026-08-10) first proposed a dedicated `X::Syntax::Regex::P5` error.
  It was superseded by #6708 (merged 2026-09-21), which instead drops `:P5`/`:Perl5` from the known
  adverbs, so they get the generic "Adverb P5 not allowed on m" — explicitly because "a message like
  that is itself something we would then have to keep supporting". Before that change RakuAST
  silently compiled `m:P5/o+/` to a regex that never matched. `0dd07126b` then removed the
  `P5Regex` grammar, actions and slang (`$~P5Regex` no longer exists), and `f0a240414` made RakuAST
  the default for core and programs. Rakudo 2026.09 ships this.
- roast `41679797` (lizmat) deleted `S05-modifier/Perl_0.t` .. `Perl_10.t` — "This will not be
  supported in any language level for now" — and `1f521d79` dropped the `$~P5Regex` check from
  `S28-named-variables/slangs.t`. Nothing in roast pins the feature any more.

Ecosystem usage, measured for #10225 by scanning the sources of the latest version of every
distribution (1638 fez distributions plus the 920 REA-only ones) for `:P5`/`:Perl5` used as a regex
adverb: **6 of 2558 (≈0.23%)** — Pastebin::Shadowcat, RegexUtils, Router::Right (tests only),
CI::Gen, Ini::Storage, LIVR. The ecosystem ledger's denominator (Rakudo 2026.09) already rejects
`:P5`, so the affected test files are `no_baseline` there (RegexUtils `t/020-compile.rakutest`,
Router::Right `t/03.t`): keeping the engine bought no parity at all.

## Decision

1. **Follow Rakudo.** `:P5` and `:Perl5` are not regex adverbs. They are not special-cased: the
   adverb parser (`src/parser/primary/regex/adverbs.rs`) reports *every* adverb a construct does not
   take as Rakudo does, `X::Syntax::Regex::Adverb` with `adverb`, `construct` and the message
   "Adverb NAME not allowed on CONSTRUCT" (`construct` is `m`, `rx`, `s` or `S`; `ms`/`ss` report as
   `m`/`s`). This replaced mutsu's own "Unsupported regex adverb :NAME". It is a parse-time error, so
   nothing in the compilation unit runs.
2. **Delete the engine**, with no language-version or pragma gate: the PCRE2 matching paths, the P5
   pattern rewriting and interpolation, the P5 delimiter scanner mode, the `perl5` field on
   `Expr::Subst`/`NonDestructiveSubst`, `OpCode::Subst`/`NonDestructiveSubst` and `RegexAdverbs`,
   the legacy `:P5` substitution lowering, and `RareCaps::positional_slots`, which only the PCRE2
   path wrote. The precomp `CACHE_FORMAT_VERSION` is bumped for the changed serialized shapes.
3. **Drop the `pcre2` dependency** and the `libpcre2` build/runtime packages (release workflow,
   Dockerfile). A C compiler is still needed for the vendored libffi (NativeCall).
4. The Perl 5 *syntax* diagnostics (`X::Syntax::P5`, `X::Syntax::Perl5Var`, `X::Worry::P5*`,
   obsolete trailing modifiers such as `m/x/i`) are a different feature and stay; roast still tests
   them.

## Rejected alternatives

- **Keep `:P5` as a mutsu extension**, with local tests as its only spec. It would diverge from the
  reference implementation on a construct Rakudo now rejects, keep the heaviest build dependency
  and a second regex engine with its own capture semantics, and serve ≈0.23% of distributions whose
  `:P5` code already fails on current Rakudo.
- **Gate it behind a language version or pragma.** Rakudo rejects it on every language level, and
  roast says it "will not be supported in any language level for now". A gate would keep all of the
  above costs for a mode no reference implementation has.
- **A dedicated "Perl 5 regexes are no longer supported" error** (rakudo #6534's
  `X::Syntax::Regex::P5`). Rakudo chose the generic unknown-adverb error instead, to avoid a message
  it would have to keep supporting; matching that keeps mutsu's behaviour identical to the oracle.

## Consequences

- Code using `:P5` must be ported to Raku regex syntax, exactly as on Rakudo 2026.09.
- If a regex-dialect slang API (problem-solving #378's suggestion) appears upstream, P5 regexes
  could return as an ordinary module running on that API. That would be new work under a new ADR,
  not a revival of the removed engine.
- `$~P5Regex` has no special support. mutsu still answers any `$~NAME` with a placeholder where
  Rakudo reports "No grammar is known for slang 'NAME'" for unknown slangs; that general gap is
  [#10455](https://github.com/tokuhirom/mutsu/issues/10455).
