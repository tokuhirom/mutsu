# A declared type beats the quoting language

A role or class named `Q` is ordinary Raku, and mutsu accepted it as a bareword
type everywhere except on the right of `does`, where the `Q` quoting language
won and ate the rest of the statement list:

```raku
role Q { }
class C { }
my $c = C.new;
$c does Q;
say "MARK1";
say "MARK2";
```

raku prints `MARK1` then `MARK2`; mutsu printed only `MARK2`. The AST showed
why —

```
Expr(Binary { left: Var("c"), op: Ident("does"),
              right: Literal(Str("\nsay \"MARK1\"")) })
```

— `Q;` was read as a `Q`-quote delimited by `;`, swallowing everything up to the
next `;`. The swallowed statement still *executed* when the operand was
evaluated, so a one-liner like `$c does Q; say $c ~~ Q` looked correct while
being parsed completely wrong. That made it an unusually good liar: it only
showed up as a missing line when two statements followed.

raku's rule is that a declared symbol wins over the quoting construct. With
nothing declared, `raku -e 'say Q;abc;'` prints `abc` (a `Q`-quote); adding
`role Q { }` makes the same source complain that the routine `abc` is
undeclared, because `Q` is now the role and `abc;` is a separate statement.

`big_q_string` and `q_string` now apply that rule: they take the leading
alphabetic word and bail out when it is a user-declared type
(`is_user_declared_type`), letting the term parser resolve the name. The guard
is uniform across the whole family, so `class qw { }` behaves the same way,
and an undeclared `q`/`qq`/`Q`/`qw` is still the quoting language.

Only the `does` right-hand side actually mis-parsed — a type constraint,
`Q.new`, `my Q $v` and a smartmatch RHS all resolved `Q` correctly already — but
the fix belongs in the quote parsers rather than in `does`, because that is
where the ambiguity is decided.

Pinned by `t/declared-type-beats-quote-lang.t`, which passes identically under
raku.
