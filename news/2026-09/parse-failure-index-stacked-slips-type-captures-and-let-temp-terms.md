# Parse-failure index: stacked Slips, type captures written after a type, and `let`/`temp` as a term

Another batch off the `expected statement …` parse-failure index ([#7954](https://github.com/tokuhirom/mutsu/issues/7954)),
re-derived from the current `ecosystem/dists/` records rather than the issue's snapshot table.
Six constructs, two distributions: `Data::Translators` now parses every module its META6 `provides`
names, and `Data::Record` loses four of the five files it was failing on.

## The contextualizers stack for `|` as well as `&`

`&&(0, 1)` became `&(&(0, 1))` in #8298; the `|` half was never done, in either the bare or the
parenthesized form, so a term-position `||` was not a term in any reading. `Data::Translators`
opens an assignment with one:

```raku
my $isHTML =
        || $_.trim.starts-with('<math') && $_.trim.ends-with('</math>')
        || $_.trim.starts-with('<table') && $_.trim.ends-with('</table>');
```

The trap is what that looks like. It reads as a leading-`||` boolean chain and **is not one**:
rakudo parses it as `(||(A && B)) || (C && D)` — a one-element Slip on the left, which the
distribution only gets the answer it wants from because a one-element list is truthy.
`|| False || True` is `slip(Bool::False,)`, not `True`. Implementing the idiom it resembles would be
a private dialect that silently disagrees on the value, so what landed is rakudo's own reading: the
prefix `|` takes another contextualizer as its operand, exactly as `&` does.

There is no ambiguity with the infix `||`. An infix is only ever looked for once a term has been
parsed, so a `||` reached in term position has no left operand and cannot be one — which is why the
`!input.starts_with("||")` guard the prefix carried was never protecting anything.

It *was* protecting something else, though, by accident: a **subscript** opening with `||` is the
6.e dimension splat (`%h{||@dims}` — the list supplies the semicolon dimensions), which the postfix
parser lowers to a `MultiDimIndex`. The assignment-lvalue parser has its own subscript reader that
parses the body as an ordinary expression, and it used to fail on `||` and hand the subscript back
to the postfix path. Once `||` became a term that reader started succeeding, silently degrading
`(%h{|| @dims} = 42, 666)` to a one-dimensional slice — so it now declines a `||` subscript body
explicitly, which is the rule that was being relied on all along.

## A parameter's type capture may be written after its nominal type

`::T Int:D $x` parsed; `Int:D ::T $x` did not. The branch that recognizes a variable after a type
read the capture's **first colon** as the named-parameter marker and was left with `:T $x`, so the
whole parameter list failed. `Data::Record` writes both orders in one signature:

```raku
multi method new(::?CLASS:_ ::THIS: List:D ::T $original is raw, …)
```

`ParamDef` has carried the two halves separately since #7984 (`type_constraint` for the nominal
type, `type_capture` for the capture), so only the grammar was missing. Rather than duplicate the
capture branch, the type-first order delegates to it and hangs the nominal half back on the result —
one branch, so every shape that branch already knows (a bare capture, an invocant marker, a default,
traits, a `where` clause) works in this order too, for free.

## `let` and `temp` are statement prefixes over an assignment — three spellings were missing

`Data::Record::Map` writes all three in one line:

```raku
MapIterator.new(THIS, 'bounded', WRAP, 'push', let %!record .= push: @values).sink-all;
```

**The variable may be an attribute.** `let $!x` was not recognized at all, so it came back as the
bareword `let` followed by an ordinary assignment — the save silently dropped — and the term form
`(let %!record)` was a hard parse error. `temp $!x` was worse: `$!` was read as the *error variable*,
so `temp $!x = 2` was a `temp $!` followed by an assignment to a stray lexical `x` and the attribute
was never written at all (`say $!x` answered `1` where rakudo answers `2`).

Fixing the parse exposed the half below it. An attribute's source of truth is `self`'s shared
attribute cell — `exec_get_local_op` re-reads the cell on every `$!x` — so restoring the method
frame's mirror slot and the `env` entry left the temporized value standing past the scope. The
restore writes the cell too now; the helper is a no-op when `self` is not an instance, so no other
`temp`/`let` target pays for it.

**The assignment may be a compound one.** Only plain `=` had a branch, so the `.= push` that followed
became a *topic* dot-assign: the push landed on `$_` and the container was left untouched
(`let %!r .= push: (a => 1); say %!r` answered `{}`). A compound assignment is parsed as an ordinary
expression from the variable and run after the save, the same lowering `temp $s[1]<k> = 23` already
used.

**The whole thing may stand as a term, including as an argument.** Only the parenthesized form
`(let $x = 5)` was a term, and that path demands the `)` immediately after, so an argument-position
one was unparsable. `let`/`temp` join the control keywords that `identifier_call` already admits as
terms; the `ws1` guard keeps a user-defined `sub let` callable as `let()`.

## A bare `+` is the anonymous one-arg-rule slurpy

`proto MAKEOP(Str:D, +) {*}` (`Data::Record::Test`). Only the sigilled (`+@a`) and sigilless-name
(`+foo`) forms were parameters at all, so a signature holding a bare `+` failed at its closing paren.
It binds the same anonymous array the `+@` path already mints.

## A named parameter's alias is a key, not a variable — and the key has no sigil

Two separate copies of that rule had drifted, and `Data::Translators` trips the first one:

```raku
multi sub html-table-highlight(Str:D $s, :h(:@highlight)!,
                               Str:D :c(:$color) = 'Orange',
                               :s(:$font-size) = Whatever, …)
```

The duplicate-**variable** check counted the alias's outer name (`s`) as a declared `$s`, so the
ordinary positional `$s` in the same signature was rejected as
`X::Redeclaration: Redeclaration of symbol '$s'`. The variable an alias really binds is its inner
sub-signature parameter, which the same function already collected separately — the outer name only
ever names an external argument key, whose uniqueness is the `X::Signature::NameClash` check that
follows. That is also the error rakudo reports for a genuinely repeated key, which mutsu was
reporting as a Redeclaration for every spelling.

The second copy is the nested-alias key collector, which stripped a leading `:` and never a sigil.
So `:h(:@highlight)` answered to `h` but not to `highlight`, and the call died with "Unexpected named
argument"; `:h(:$hi)`, whose inner name carries no sigil, worked, which is what hid it. Both sites
share one `named_param_external_key` now.

## Writing the attribute cell on restore needed the save to read it too

Making the restore write `self`'s attribute cell is what the `temp $!x` fix above requires, and it
unmasked two things the old, ineffective restore had been hiding. The bundled-battery gate found
them: `Template::Mustache`'s `render` died with "No such method 'log'", because
`temp $!logger.level = $_ with $log-level` destroyed `$!logger` and the next line delegates through
it (`has $.logger handles <log>`).

**The save was reading a different store from the restore.** `temp $!o.attr = v` emits a `LetSave`
naming `!o`, and `exec_let_save_op` read the method frame's mirror slot and then `env`. In a body
that has not otherwise touched `$!o` both are empty, so the snapshot was `Nil` — and the restore
duly wrote that `Nil` over a live object. The save reads the cell now, so the two agree.

**A statement modifier is not a block, so it opens no `let`/`temp` scope.** The save belongs to the
enclosing routine and must still stand on the next statement: `temp $x = 2 with $c; say $x` says 2
in rakudo and said 1 here — for a plain lexical, with no attribute involved, and on `main` as well.
The modifier's branch was given its own `OpCode::LetBlock` frame and restored there. That was
invisible for as long as the restore wrote only the mirror slot and `env`, which nothing reads back
for an attribute, and became a destroyed attribute the moment the cell was in play. Both the
statement-position and the value-position branch emitters skip the frame for a modifier now, beside
the topic rebind, block-local scope and dynamic scope those same functions already excluded it from.

Worth carrying forward: a restore that starts writing a *shared* store is not a local change. Every
save feeding it has to be re-checked for reading the same store, and every scope that brackets it
has to be re-checked for being a real scope — neither was observable while the restore was writing
somewhere nothing read.

## Pins

`t/lang/sigil-contextualizer-stacked-slip.t`, `t/routines/signature/param-type-then-type-capture.t`,
`t/oo/attribute/let-temp-attribute-and-term-forms.t`,
`t/routines/signature/anon-onearg-param-marker.t`,
`t/routines/signature/named-arg-alias-external-key.t` — all five green under rakudo itself, so they
pin rakudo's behaviour rather than mutsu's. The `let`/`temp` file grew four more of the same kind:
a `temp` reaching through an attribute leaves the attribute defined and the same object (so
delegation through it still resolves), and an `if`/`with` modifier holds its save to the end of the
enclosing routine. `t/routines/dispatch/multidim-splat-lazy.t` grows the
parenthesized-lvalue case of the dimension splat (6.e behaviour, which mutsu implements
unconditionally; verified against rakudo under `use v6.e.PREVIEW`).
