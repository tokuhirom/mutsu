# Eight RakuAST constructs the L10N family needs

`Str.AST($language)` started working when the L10N slang vocabulary landed, and
twelve of the thirteen `L10N::*` distributions went green with it. The
thirteenth, `L10N::ZH`, is the only member with a real test suite rather than a
single smoke test — nine files, forty-one rakudo assertions — and it kept
failing, because each of its files is a
`Q:to/CODE/.AST("ZH").EVAL` of Chinese-spelled Raku and therefore fails whole
the moment *any* construct in it is missing from the RakuAST boundary. The
interpreter ran all of that code fine; it was the model layer that could not
carry it across.

Eight constructs are now carried, each measured against rakudo 2026.07 in both
directions:

- **`Nil`** renders as `RakuAST::Type::Simple.new(Name.from-identifier("Nil"))`
  — a type object written as a bareword, not a literal value. mutsu's parser
  resolves the bareword to the value eagerly, so the check has to precede the
  literal dispatch rather than live inside it.
- **`self`** is `RakuAST::Term::Self`, a node with no fields at all, printed as
  a bare `.new` without empty parentheses. It is a `Term` and an `Expression`.
  Any method body mentioning `self` previously stopped `.AST` dead, so this is
  not specific to the localized case.
- **`our` / `state` declarations** already *rendered* their `scope` field; the
  lowerer refused to read it back, so `EVAL` of any tree containing one died.
  It now accepts the two scopes the converter emits and keeps refusing the rest.
- **`redo`** joins `last` and `next` as a bare call
  (`Call::Name::WithoutParentheses`), which is how rakudo models all three.
- **`CATCH { … }`** is `RakuAST::Statement::Catch`, whose body is a topic block
  that additionally carries `exception => 1` — the field that distinguishes it
  from a `given` body.
- **`subset S of T where P`** is `RakuAST::Type::Subset`: the base type is a
  `Trait::Of` entry in `traits`, not a field of its own, exactly as a routine's
  `of` return type is. A subset that writes no `of` renders **no `traits` field
  at all** — `subset S where * > 0` and `subset S of Any where * > 0` are
  different nodes to rakudo even though the implied base *is* `Any`. mutsu's
  parser defaulted both to `base: "Any"`, so `Stmt::SubsetDecl` grew a
  `base_is_explicit` flag; inventing a `Trait::Of(Any)` the source never wrote
  would have been exactly the kind of reconstruction the RakuAST workflow
  forbids.
- **`module` / `package`** are `RakuAST::Module` / `RakuAST::Package`. rakudo
  gives each declarator keyword its own class rather than one node with a
  `kind` field, and a later bareword naming the package resolves at parse time
  to a `Type::Simple` just as a class name does — so the converter's
  declared-name scan had to learn about packages too.
- **`submethod`** is `RakuAST::Submethod`, a sibling of `RakuAST::Method`
  carrying an identical shape. mutsu's parser marks every submethod `is_my` as
  its internal "not inherited" flag rather than because the source said `my`,
  which was what made the converter refuse it.

One of the nine files needed a fix on the other side of the boundary as well.
`stmt-prefix-try` is a *replacement* category in the L10N schema, so `试试` is
what `try` is spelled under `L10N::ZH` — but mutsu recognizes `try` (and `do`,
`gather`, `if`, `for`, `last`, `redo`, …) by matching the parsed identifier
against a fixed string in the bareword-term production, not through the
`keyword()` seam every other replacement category hooks. `试试 { 10 / 2 }`
therefore read as a bare word. The term production now consults the vocabulary
too. The ASCII spelling keeps working there even where the vocabulary replaces
it, which over-accepts relative to rakudo but cannot mis-parse a program rakudo
accepts.

`L10N::ZH` goes from one green file (and 17 of 41 assertions) to four green
files and 29 assertions. The five that remain are blocked on constructs that
are their own slices, each needing something this one deliberately did not
start:

- `t/15-modifier`, `t/13-package`, `t/14-routine` need `RakuAST::QuotedRegex`
  and the `RakuAST::Regex::*` node tree (`Sequence`, `Literal`, `WithWhitespace`,
  `CharClass::*`, `Quote`), which `token` / `rule` / `regex` declarations inside
  a grammar are built out of.
- `t/16-enum-subset` needs the `term` field of `RakuAST::Type::Enum`, which is
  the *unevaluated* variant list — a word-quoted `QuotedString` carrying
  `processors => <words val>` for `enum C <A B>` and a parenthesized pair list
  for `enum C (A => 1)`. mutsu's `Stmt::EnumDecl` has already normalized both
  into `Vec<(String, Option<Expr>)>`, and `processors` is not modelled on
  `QuotedString` at all, so rendering it faithfully needs a parser change first
  rather than a guess in the converter.
- `t/11-use-import` needs `use` to render as `RakuAST::Pragma`, plus `import`.
- The `with` / `without` statement modifiers (in `t/10-block`, a file rakudo
  itself cannot run) are desugared by mutsu's parser into `given` plus an
  `if $_.defined`, so the modifier kind is gone before conversion sees it.

`t/rakuast/rakuast-l10n-constructs.t` pins all eight constructs in both
directions and passes under rakudo as well as mutsu, which is what makes the
measured shapes rakudo's rather than an invention; the statement-prefix fix is
pinned by two new cases in `t/lang/parsing/slang-l10n-vocabulary.t` against the
`L10N::Testish` fixture, likewise green under both.
