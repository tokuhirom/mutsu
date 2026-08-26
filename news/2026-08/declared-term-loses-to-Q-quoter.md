# A declared symbol now shadows every quote language spelled the same way

`enum E <P Q>; say Q, "x", 2;` printed `Qx2` in rakudo — `Q` is a declared term,
so it wins over the generic `Q//` quote language — but mutsu answered
`Confused. Two terms in a row`, having read `Q, "x",` as a `Q`-quote delimited by
`,`. A sibling investigation found the identical shape for `S` (`enum P (S => 1);
say 5 ~~ S;` silently swallowed the next statement as an `S///` substitution),
which is what made it clear this was a family, not a one-off.

## Root cause

mutsu decided "quote construct or term?" lexically, per quote name, and then had
a handful of one-off guards bolted on after the fact: `big_q_string` and
`q_string` consulted `is_user_declared_type`, the `S///` branch consulted
`is_user_declared_type("S")`, the `s///` and `m//` branches consulted
`is_user_declared_sub`, and `qx`/`qqx`/`tr`/`TR`/`rx` consulted nothing at all.
Each guard therefore asked about a *different* subset of the declared-symbol
registries, and most quote names had no guard whatsoever.

A survey of the whole quoter family against `raku` v2026.06 (`Q q qq qw qww qqw
qqx m s S tr TR rx`, each declared as an enum value / `constant` / `sub` /
sigilless term) showed eleven of the sixteen cells diverging — some as a parse
error, some silently: with `tr` declared, `say tr, 'x', 2` printed a
`StrDistance`, and with `qqx` declared it ran a shell command.

Rakudo's rule is unconditional and needs no lookahead heuristics: **once a symbol
of that name is declared, the quote language spelled that way is gone.** `enum E
<P Q>; say Q/2/;` is the division `Q / 2 /` ("Missing required term after
infix"), not a `Q`-quote. The one exception, which rakudo shares, is an explicit
**adverb**: `s:g/…/…/` is a substitution even with `sub s` in scope, and so are
`m:i/…/` and `q:w/…/` — an adverb makes the construct unambiguously the quote
language.

## Fix

`src/parser/quote_shadow.rs` is the single, name-agnostic implementation of that
rule: extract the identifier the construct is spelled with, let an explicit
adverb win, and otherwise let a declaration of that identifier win. The
declaration test is `is_declared_symbol_name`
(`src/parser/stmt/simple/user_ops.rs`), the union of every registry that makes a
bare name a term — routines, types, enum values, sigilless term symbols and
imported value terms. Every named quote entry point now calls it
(`big_q_string`, `q_string`, `qx_string`, and `regex_lit` once at the top, for
`m` / `s` / `S` / `tr` / `TR` / `rx` alike), and the old per-letter guards are
gone.

All sixteen quoter names now match rakudo in every declaration form, the
undeclared controls still parse as quoters, and `m:i//` — which mutsu used to
reject outright once `sub m` existed — works again.

One local test encoded the old behaviour and was corrected:
`t/subset-name-vs-subst-operator.t` used `S/b/X/` *after* declaring a
package-scoped `subset S`. rakudo rejects that too (it parses as `S / b / X /`
and reports `b` as an undeclared routine), so the `S///` block moved above the
subset declaration.

Pinned by `t/custom-operator-and-term-parsing.t` sections 1, 7 and 8.
