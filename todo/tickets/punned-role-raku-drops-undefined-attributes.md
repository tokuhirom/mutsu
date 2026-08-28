# `.raku` on a punned-role instance drops its attributes

An instance built by punning a role (`role R { has $.a }; R.new(:a($x))`) renders
its `.raku` without any attribute, while rakudo renders every attribute the way
it does for a class.

## Repro

```
$ raku  -e 'my $a; role Tc { has $.a }; my Tc $c .= new(:$a); say $c.raku'
Tc.new(a => Any)
$ mutsu -e 'my $a; role Tc { has $.a }; my Tc $c .= new(:$a); say $c.raku'
Tc.new
```

The same holds inside an `EVAL`, and it is not about the value being undefined —
`my $a = 5` gives rakudo `Tc.new(a => 5)` and mutsu still `Tc.new`. The class
form is already correct: `class Tc { has $.a }` renders `Tc.new(a => Any)` in
both. So the gap is specific to the *punned role* instance: whatever `.raku`
uses to enumerate an instance's attributes finds the class's `has` declarations
but not the ones a punned role contributed.

## Where to look

The `.raku`/`.gist` renderer for `ValueView::Instance` enumerates attributes from
the class registry entry for `class_name`. A punned role creates its class shell
from the `RoleDef`, so the question is whether that shell carries the role's
attribute list at the point `.raku` reads it (and whether it should be read from
the role instead).

## Why it is filed rather than fixed

Found while fixing
`news/2026-08/package-type-short-name-vs-same-named-lexical.md` (the
`S12-construction/autopairs.t` real-`Test` regression). That file's role
assertions only use `eval-lives-ok`, so this divergence gates nothing there and
folding it in would have mixed two unrelated root causes into one PR.
