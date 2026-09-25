# `has $.x:` takes a colon arglist, as in rakudo

[#9321](https://github.com/tokuhirom/mutsu/issues/9321), split out of the
[#7988](https://github.com/tokuhirom/mutsu/issues/7988) parse-gap cluster.
POSIX::PWDENT's `lib/POSIX/GRPENT.rakumod` has

```raku
has Int $.gid:
has Str @.members;
```

almost certainly a typo for `;`, but rakudo accepts it and the module works.
mutsu stopped at the colon with `===SORRY!=== Confused`.

`raku --target=parse` shows the grammar path: rakudo's `variable` token lets a
`.`-twigil variable take a colon arglist (the `$.meth: args` colon-call form),
and a `has` declarator reuses that token. So the second `has` declaration is
parsed *as the first one's arglist*. The arglist's value is thrown away
(`has $.x: 42` gives `$.x` no default, and `has $.x: say("hi")` prints
nothing), but a declaration inside it still declares, so both attributes
exist, the nested one first in `.^attributes`.

`has_decl` now reproduces that rule: after a `.`-twigil attribute name, a `:`
that is adjacent to the name and followed by whitespace starts an arglist.
A nested `has`/`HAS` there is parsed as its own declaration and spliced in
front of the outer one in the same flat block the class-body attribute walk
reads; any other arglist is parsed and discarded. `has $!x: 3`, `has $.x : 1`
and `has $.x:1` stay errors, as in rakudo.

One known gap is left as a `TODO` in the parser: a `my` inside such an arglist
(`has $.x: my $q = 5`) is discarded with the rest of the expression, where
rakudo declares `$q` (unassigned).

Pinned by `t/oo/attribute/attribute-colon-arglist.t`.
