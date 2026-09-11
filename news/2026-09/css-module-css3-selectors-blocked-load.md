# CSS::Module::CSS3::Selectors: from `blocked_load` to 80 of 92 assertions

`CSS::Module::CSS3::Selectors` 0.0.6 was drawn at random from the `ecosystem/`
ledger (lock board [#7884](https://github.com/tokuhirom/mutsu/issues/7884)) with
status `blocked_load`: its provided module did not even `use`, so none of its
suite could run. Seven interpreter bugs later its single baseline test file runs
end to end and passes 80 of its 92 assertions.

None of the seven is specific to CSS. Each was found by reducing a distribution
failure to a few lines that run from this repo, checked against the `raku`
oracle, and pinned by a `t/` test.

## `also is Base` on a grammar replaces the implicit `Grammar` parent

A `grammar` declarator with no `is` clause carries an implicit `Grammar` parent.
mutsu kept that parent when the body later added one with `also is`, so
`unit grammar CSS::Grammar::CSS21; also is CSS::Grammar;` asked for multiple
inheritance from both `Grammar` and a grammar that itself inherits `Grammar` —
a C3 merge with no linearization, reported as "Inconsistent class hierarchy for
CSS::Grammar::CSS21". Rakudo linearizes `grammar G { also is Base }` exactly
like `grammar G is Base { }`, so the implicit parent is now dropped when an
`also is` supplies a real one. The block form `grammar G { also is Base; }` was
not extracting its parent from the body at all (it reached the runtime as a bare
`is(also, Base)` infix and died with "Two terms in a row"); it now shares the
same extraction the `class` and unit forms use, including the named-grammar
expression path that `grammar G { … }.parse($s)` takes.

## `require Foo:ver(…)` / `:ver<…>` distribution selectors

`require` accepted no version/auth/api adverb on its target, so
`require CSS::Grammar:ver(v0.3.3+)` parsed as a call to an undeclared routine
`CSS::Grammar:ver`. Both spellings are now consumed. The angle-bracket form is
carried through to `use_module`, where `split_dist_selectors` and
`select_dist_candidate` already refine which installed distribution is loaded;
the parenthesized form holds an arbitrary expression and is discarded, exactly
as `use` already discards it. The module's own name stays bare, so the stub
`require` installs and the package it returns are unaffected.

## `::` inside a `:sym<…>` is part of the name, not a package qualifier

`rule pseudo:sym<::element>` is real Raku — it is how CSS::Grammar::CSS3 spells
the CSS3 pseudo-element selector — and its action method is
`method pseudo:sym<::element>($/)`. Method dispatch split every name on its last
`::`, inventing a package qualifier `pseudo:sym<` and a method `element>`, so the
action never fired ("Cannot dispatch to method element> on pseudo:sym<"). The
four qualified-dispatch entry points now split only at a `::` that lies outside
an extended-name adverb.

## `method build handles<…>` needs no space before the angle-word list

The `handles` trait on a *method* required whitespace after the keyword, while
the attribute form had always accepted the tight spelling. `method build
handles<token node list at-rule> { … }` — the spelling Raku's own documentation
and `CSS::Grammar::Actions` use — therefore failed to parse, taking the `build`
method with it.

## A `Str enum`'s values are `Str`s

mutsu hardcoded `Int` as the base type of every enum value: `dispatch_mro` put
`Int` behind the enum type, the VM's two fast parameter type checks knew only
the five scalar shapes, and string context answered the key. So
`our Str enum CSSSelector «:PseudoElement<pseudo-elem> …>` — how
`CSS::Grammar::Defs` declares every CSS selector and property type — produced
"Type check failed in binding to parameter '$type'; expected Str but got Int"
at the first action that passed one on. The base type an enum's values carry now
decides its ancestry, its type checks and its string context, while `.gist`
keeps answering the key (`say S::A` prints `A`, `~S::A` is `a-val`).

## `:i'literal'` with no space

A short inline regex adverb (`:i`, `:s`, `:r`, `:m`) only ended at a space, a
`:` or a `/`. `/:i'not('/` — how CSS::Grammar spells every case-insensitive CSS
keyword, and `:i` followed by a newline as well — therefore left the adverb
unrecognized and the whole pattern failed to match. A short adverb now ends
wherever an identifier would, which still rejects `:my`, `:sym<…>` and `:ss`
because their next character continues the name.

## Two calls to the same subrule at one position are siblings, not left recursion

The streamed subrule path held its left-recursion activation across the
*continuation* — the caller's remaining pattern — so a second call to the same
rule at the same position read that activation's empty seed and failed. Every
`rule` containing a bracketed group compiles to exactly that shape, because
sigspace puts a `<.ws>` at the end of the group and another right after it
(`'[' <.ws> [ <id> <.ws> ] <.ws> ']'`). The effect was invisible with the
built-in `ws` and total with a user-defined one: a grammar that declares its own
`ws`, as CSS::Grammar does, stopped matching *any* rule with a group in it,
which took out `attrib`, `pseudo-function` and most of the CSS3 selector
grammar. The activation is now lifted across the continuation and restored for
the rest of the body walk, so the code-block re-entry it exists to catch is
still covered.

## What is left

Twelve assertions still fail, each with an open issue and a reduction:

- [#7909](https://github.com/tokuhirom/mutsu/issues/7909) — `Grammar.parse(:rule<proto>)` commits to one proto candidate and never falls through. Reaching the same proto as a *subrule* already falls through; only the top-level entry does not.
- [#7910](https://github.com/tokuhirom/mutsu/issues/7910) — `$<name>=<.subrule>` does not run the subrule's action, so the aliased capture's `.ast` is `Nil`. This is what drops every integer out of the `nth-child(3n+1)` AST (8 of the 12).
- [#7912](https://github.com/tokuhirom/mutsu/issues/7912) — a `(` inside a `<?before '…'>` literal makes a quantified `||` group stop matching, so the nested-`:not()` guard never fires.
- [#7913](https://github.com/tokuhirom/mutsu/issues/7913) — `Pair.raku` renders a `Str`-enum key as `Enum::Key => value` rather than `:key(value)` (diagnostic only; `to-json` already agrees).

One finding is unrelated to this suite's result but was too sharp to leave
unrecorded: [#7914](https://github.com/tokuhirom/mutsu/issues/7914), an imported
enum key overwriting a same-named lexical scalar, because enum keys and scalars
share mutsu's sigil-less env namespace. `CSS::Grammar::Defs` exports `:s<time>`,
so any script with a `my $s` that calls into it has that variable replaced
mid-call.

## Pins

`t/grammar/grammar-also-is-replaces-implicit-parent.t`,
`t/grammar/grammar-action-sym-with-double-colon.t`,
`t/modules/import-export/require-dist-selectors.t`,
`t/oo/trait/method-handles-no-space.t`,
`t/types/enum-subset/enum-declared-base-type.t`,
`t/regex/regex-short-adverb-tight-atom.t`,
`t/regex/regex-sibling-subrule-same-position.t`.
