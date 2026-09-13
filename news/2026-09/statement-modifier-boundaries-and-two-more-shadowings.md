# Where a statement ends: seven parse gaps from the ecosystem's biggest cluster

Continuing [#7988](https://github.com/tokuhirom/mutsu/issues/7988) after
[#8194](https://github.com/tokuhirom/mutsu/pull/8194). Same method: re-probe the cluster's
distributions with a real `use`, group the failures by the source line the parser stopped on (which
the location fix from #8065 made possible), minimise each, fix.

Five of the seven gaps fixed here are about the same question — **where does a statement end?** — and
every one of the seven was a rule mutsu already knew, applied by a site that kept its own weaker copy
of it.

## `}` ends a statement only when it ends the line

Raku needs no `;` after a statement whose `}` ends its line. mutsu had that rule twice, both times
slightly wrong:

- **The `do BLOCK while` check.** `do { ... } while ...` on one line is the Perl 5 idiom rakudo
  rejects with X::Obsolete, and the same goes for `until`/`for`/`given`. mutsu skipped *all*
  whitespace before looking for the keyword, so it also rejected

      my $x = do { 1 }
      for @list { ... }

  which is two statements. `MetamodelX::Red::Model` (Red), `PDF::Font::Loader::FontObj::CID` and
  `App::MoarVM::HeapAnalyzer::Model` each failed to load on exactly that shape. The check now fires
  only when no newline separates the `}` from the keyword.

- **The "a statement ending in a block takes no modifier across a newline" rule.** That one was
  decided from the AST — "does this statement's last expression *contain* a trailing block" — which
  is equally true of `@a = @a.grep({ ... })`, where the line ends in `)`, the statement is
  unfinished, and the next line's `if` really is its modifier:

      @envelopes = @envelopes.grep({ ($_.id // -1) != $exclude-id })
          if $exclude-id.defined;

  App::Moneymoor writes ten of its modules that way. The rule now reads the source text to see
  whether the `}` actually ends the line, which is what its own doc comment already claimed.

## An extra modifier belongs to the enclosing statement, not to this one

A statement takes at most one conditional modifier and then at most one loop modifier. mutsu raised
X::Syntax::Confused ("Missing semicolon") the moment it saw a third, which reads the rule as "nobody
may take this keyword" when it means "*this* statement may not". `do STMT` is the construct that can
take it:

    do return False unless %h<auth> ~~ $!auth if $!auth;

(Pakku::Spec — ten more Pakku compunits behind it) is a `do`-wrapped `return … unless …` whose `if …`
modifies the `do` statement. The chain now simply ends at the extra keyword and records the error for
whoever finds the keyword unconsumed — the statement list, which is where rakudo raises it too. The
diagnostic is unchanged for every illegal chain, and now carries a source location it did not have
before (`Missing semicolon at -e:1`, matching rakudo).

The record is deliberately not consumed when read: a block body is parsed more than once
(speculatively as a hash composer first, then as a block, the second time as a pure memo hit that
re-runs no modifier loop), and a record taken by the discarded attempt was missing from the one that
survived.

## A comment is whitespace, and a trailing comma is an empty list slot

A trailing comma may be followed by a statement modifier (`die "x", if @c`, pinned since
`t/control/trailing-comma-before-statement-modifier.t`). That rule was stated three times with
different terminator sets: the paren-less listop path knew about modifiers, the two colon-argument
paths (`obj.m: a, b` and `$x .= m: a`) only knew `;` `}` `)` `]`. And the modifier lookahead trimmed
*spaces* rather than whitespace, so a comment in the gap hid the keyword from every path:

    self.set-from-file: $!browser, #`[ $.debug ] unless $driver;

(WebDriver2's driver provider.) The lookahead now uses `ws`, and both colon-argument paths accept a
modifier after a trailing comma.

## An anonymous destructure in a later pointy parameter

    has &.set-content = -> $_, $content, (:$label, :$screen, |) { ... }

A *first* parameter spelled as a bare sub-signature is unpacked by the pointy header itself; a later
one goes through the per-parameter parser, which had a branch for `:(…)`, one for `Type (…)`, one for
`$var (…)` — and none for a bare `(…)`. The whole block failed with "Malformed initializer". It now
delegates to the parameter parser that already builds this shape for `sub ($c, (:$label))`, the third
such delegation in that file.

## An enum value shadows a core routine of the same name

    my enum LogLevel <nothing error warn info now debug all>;
    ...
    ) if  all   ≤ $!verbose;

`all` here is an enum value, so it is a complete nullary term. mutsu treated a core listop head as
one unconditionally and demanded `≤ $!verbose` as its argument, which is why Pakku::Log — and the nine
compunits that load it — failed to parse. Two sites now consult the declared-enum-value registry that
`given`/`when` and the ternary parser already used: the listop-argument gate, and the zero-arg-callable
gate that made `sort.value` read as `sort().value`.

## `.<>` — the zen slice on the topic

    prepare-param($_, %pars).() for .<>     # Test::Describe::It
    self!run-test: |.<> for $test.subs      # Test::Describe::Expect

The topic parser keeps its own copy of each subscript (`.<key>`, `.[i]`, `.{k}`), and the angle one
demanded at least one key, so the zero-width spelling reached no branch at all. It now yields the bare
topic with the subscript unconsumed and lets the undotted postfix branches parse it — the same rewind
that gave `$x.[0; 1]` and `$x.<>` their full set of spellings in #8194 — which brings the
`:k`/`:v`/`:kv`/`:p` adverbs along for free.

## Measured

Load probes over the cluster's cached checkouts, all provided compunits per distribution:

| distribution | before | after |
| --- | --- | --- |
| App::Moneymoor | 44/54 | **54/54** |
| Grammar::Editor | 2/9 | **9/9** |
| Selkie::UI | 43/45 | **45/45** |
| Pakku | 1/26 | **20/26** |
| Test::Describe | 4/9 | 6/9 |
| WebDriver2 | 48/194 | 51/194 |

Test::Describe's two `.<>` modules parse; its `Root` stops on an unrelated "Two terms in a row".
Pakku's remaining six stop at unrelated clusters (`CompUnit::Repository::Staging` is not
implemented); Red's `do…for` site is fixed but the distribution stops later, on a `class Red:ver<…>`
that only fails once `Red::Operators` has been imported — recorded on the issue rather than guessed
at here.
