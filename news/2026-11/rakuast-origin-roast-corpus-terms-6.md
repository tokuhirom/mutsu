# RakuAST: a sixth set of origins from the roast corpus

The `.origin` differential (#15780) over a further sample of whitelisted
roast files showed more unplaced nodes, now placed as Rakudo places them:

- a hash composer right after the item sigil (`${ a => 1 }`);
- a cross or zip metaop list (`1 X~ 2 X~ 3`, `(1 Z+ 2)`): the
  `MetaInfix::Cross` / `MetaInfix::Zip` sits on the last operator, its
  `Infix` after the one-letter prefix;
- the `Call::Term` of a call on the topic (`.(1 + 2)`) and its `ArgList`;
- an initializer written right after a name's adverb (`my $t:foo<a>= 1`);
- a parenthesized package declaration used as an operand
  (`(class Q { }).^name`);
- the positions inside a fragment the frontend re-parses, which now records
  the unit's offsets directly instead of shifting them afterwards.

Two names now have Rakudo's shape as well: a sigilless term declared under
its operator name (`my \term:<ℵ₀> = 5`) and a package-qualified operator name
used as a term (`Foo::term:<ab>`) both carry their `<...>` as a colonpair of
the `Name`, with its `QuotedString` placed, and the latter is a `Call::Name`
rather than a construct `.AST` refused.
