# Fix `:my $var = $/;` failing to parse inside a slash-delimited regex

An embedded declarator inside a regex literal, like `:my $c = $/;`, referencing the
in-progress match object (`$/`) failed to parse:

```raku
"aba" ~~ / (a) b {} :my $c = $/; /;
say $c;
```

mutsu raised `Runtime error: Regex not terminated.` where `raku` parses and matches
fine, printing `｢ab｣` with `0 => ｢a｣` for the match.

The root cause was in the outer delimiter-scanning phase of the regex-literal
parser (`scan_to_delim_inner` in `src/parser/primary/regex/scan.rs`), which finds
where a `/pattern/` (or `s/pattern/.../`) literal's raw text ends *before* any
regex-specific parsing of that text happens. That scanner already special-cased
embedded `{ ... }` code blocks — skipping the whole balanced brace block so a `/`
inside it (most commonly the `/` of `$/`) does not end the regex early — but it had
no equivalent awareness of the embedded declarator clause `:my $var = EXPR;` (and
its siblings `:our`, `:constant`, `:let`, `:temp`). A `$/` appearing directly as a
declarator's RHS, with no wrapping `{ ... }`, hit only the pre-existing (and
necessarily ambiguous) `$` + close-delimiter heuristic used to distinguish the
end-of-string anchor `$` from the `$/` match variable — a heuristic that only
recognized `$/` when immediately followed by a postfix (`.`, `[`, `<`), not by a
bare statement terminator like `;`. So the `/` of `$/;` was misread as the regex's
own closing delimiter, truncating the pattern text mid-clause and leaving the rest
of the source to fail as "Regex not terminated."

The fix adds `starts_regex_decl`/`skip_regex_decl_clause` to `scan.rs`, mirroring
the existing `regex_parse_ltm::leading_regex_decl_end` helper (which already does
equivalent bracket/quote-depth tracking for the *already extracted* pattern text,
to hold a declaration clause aside during `%`-separator expansion). When the
scanner sees `:my `, `:our `, `:constant `, `:let `, or `:temp `, it now skips the
whole clause as opaque Main-slang code rather than falling through to plain
delimiter matching, so a bare `/` inside it — typically the `/` of `$/` — is never
treated as a delimiter.

An embedded declaration actually comes in two distinct shapes, and the fix has to
handle both:
- **scalar form** — `:my $c = EXPR;` — ends at the first unescaped, unnested `;`
  (honoring backslash escapes, quoted strings, and balanced `()`/`[]`/`{}`
  nesting).
- **block form** — `:my token NAME { BODY }` / `:my rule NAME { BODY }` / `:my
  regex NAME { BODY }` (and the `our`/`constant`/`let`/`temp` equivalents) —
  declares a lexically-scoped named sub-rule and is terminated by BODY's own
  closing `}`, with **no trailing `;` at all**
  (`roast/S05-modifier/my.t`'s `:my token SIGN { <[+-]> }` has none).

A first version of this fix only implemented the scalar form's `;`-search and
assumed every `:my ...` clause used it, which broke the block form: it scanned
past the rest of the file looking for a `;` that never came, regressing
`roast/S05-modifier/my.t` in CI (`"Regex not terminated"` on
`:my token SIGN { <[+-]> }`). `block_form_decl_prefix_len` now recognizes the
block form up front (by peeking for `token `/`rule `/`regex ` plus a name and `{`
right after the declarator keyword) and dispatches to the existing balanced-brace
skipper instead. Both scalar and block forms apply uniformly to `scan_to_delim`
(regex/`m//` patterns) and `scan_to_delim_subst_pattern` (`s///`/`S///` pattern
halves), since both share the same underlying `scan_to_delim_inner`.

Verified no regression to the existing `$`-anchor disambiguation (`/foo$/;` still
parses as end-of-string anchor followed by the real closing delimiter), no
regression to `roast/S05-modifier/my.t` (24/24) or the other roast files using
in-regex declarations (`S05-grammar/protoregex.t`,
`S05-interpolation/regex-in-variable.t`, `S05-metasyntax/interpolating-closure.t`,
`integration/advent2013-day18.t`), and added `t/regex-decl-self-referential-slash.t`
(16 assertions, cross-checked against real `raku`) covering: `:my`/`:our`
declarators whose RHS is `$/` or `$/.Str`, with and without a preceding `{}` block,
a `$/` reference inside a genuine `{ ... }` code block, the `$`-anchor regression
check, the same construct inside an `s///` substitution pattern, and the
block-form `:my token NAME { ... }` declarator (both a multi-declaration bracket-
delimited `rx { ... }` and a scalar-form declarator inside a bracket-delimited
`token { ... }` body).

Note: this fix is scoped to the *parse* failure only. A related but distinct bug —
`:my $c = ~$0;` parsing fine but capturing an empty value instead of the
match-so-far text — is tracked separately in
`todo/tickets/regex-embedded-my-decl-value-not-captured.md`; its root cause is in
how the declarator's RHS is *evaluated* during matching (a runtime timing issue),
not in delimiter scanning, so it was left untouched here.
