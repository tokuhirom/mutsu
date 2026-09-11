# `Str.AST($slang)` parses localized Raku: the L10N slang vocabulary

The `L10N::*` distributions (`L10N::JA`, `L10N::DE`, `L10N::FR`, ... — fifteen
of them in the `ecosystem/` corpus) translate Raku's surface syntax. `L10N::JA`
lets you write

```raku
私の $a = 42;
もしも $a == 42 {
    言う "こんにちは世界"
}
```

and its entire test suite is one `Q:to/CODE/.AST("JA").EVAL` of exactly that.
mutsu had no one-argument `Str.AST`, so the suite died on
`No such method 'AST' for invocant of type 'Str'` before it reached its only
assertion.

## What an L10N distribution actually is

A generated role, one member per translatable piece of Raku's surface syntax:

```raku
role L10N::JA {
    token block-if { もしも}
    token scope-my { 私の}
    ...
    method core2ast {
        my constant %mapping = ..., "言う", "say", ...;
        ...
    }
}
my sub EXPORT($dontslang?) {
    $*LANG.define_slang('MAIN', $*LANG.slang_grammar('MAIN').^mixin(L10N::JA));
    BEGIN Map.new
}
```

Rakudo mixes that role into the MAIN slang grammar, so each `token` overrides
the grammar production that recognizes one keyword. mutsu's parser is a
hand-written recursive descent with no grammar to mix into — which is exactly
the situation [ADR-0026](../../docs/adr/0026-slang-activation-architecture.md)
already answered for `Slang::Tuxic`: run the module verbatim through the
activation sub-interpreter, then *interpret* the overrides it registers rather
than executing their Rakudo-internal bodies.

The L10N roles are the second kind of override that machinery can carry. A
Tuxic-style role overrides *productions*, and its token bodies are Rakudo
internals mutsu never reads. An L10N role overrides *spellings*, and its token
bodies are the spellings themselves — data. So the token key's category names
the parser production, and the body supplies the word.

## The two halves, measured against rakudo

`raku -I lib -e '"...".AST("JA")'` settles what each category means, and the two
turn out to differ:

- **Replacement** (`block-`, `scope-`, `routine-`, `package-`, `phaser-`,
  `modifier-`, ...): the localized spelling *takes the place of* the ASCII
  keyword. Under a JA vocabulary `if 1 { }` no longer parses and `もしも 1 { }`
  does. These all land on `parser::stmt::keyword`, the single place the parser
  recognizes a keyword by spelling.
- **Alias** (`core-`, `enum-`, `term-`, ...): the localized spelling is an
  *extra* name. Both `say 42` and `言う 42` parse. These land on the bareword
  term production and on the term-keyword literal table. `core-` and
  `trait-is-` do not appear as tokens at all — the generated roles keep them in
  a `method <category>2ast` whose `%mapping` constant is a flat list of
  `localized, canonical` string pairs, so those maps are read too.

A vocabulary is unit-scoped parser state alongside `SlangModes`, snapshot and
restored around nested module scans and preseeded through the sub-parse's own
state reset. Nothing leaks into the enclosing program: `'my $y = 5; $y'.AST`
still parses stock Raku on the next line.

`Str.AST($slang)` itself sits on the interpreter rather than in the pure arity
cascade, as `EVAL` does and for the same reason: resolving `L10N::<$slang>`
needs this interpreter's module search path, which the parser only holds around
a parse. The no-argument `.AST` is untouched.

## Result

`L10N::JA` 0.0.3 goes from `red` to `green`: 1 of 1 baseline file, matching
rakudo. `t/lang/parsing/slang-l10n-vocabulary.t` pins the mechanism against a
miniature of the generated role shape (`t/lib/L10N/Testish.rakumod`) — and
passes under **rakudo** as well as mutsu, all twelve assertions, which is what
establishes that the replacement/alias split is rakudo's and not an invention.

## What is deliberately still inert

The L10N schema has categories whose grammatical position mutsu's parser does
not consult yet: word infix operators (`infix-and` → `と`), metaoperators,
quote-language names, named arguments and adverbs. A role declaring them is
still a valid vocabulary — the entries are recorded and unused. That is safe in
a way an unrecognized *production* override is not: source written with an
inert spelling fails to parse, loudly, where a missed production override would
leave existing syntax silently meaning something else. The residue, together
with method-name aliases (`.文字` for `.chars`), the three `term-` spellings
mutsu recognizes through hand-rolled text matches (`now`, `time`, `rand`), and
compile-time activation of a plain `use L10N::JA` in a unit, is tracked in
[#7990](https://github.com/tokuhirom/mutsu/issues/7990).
