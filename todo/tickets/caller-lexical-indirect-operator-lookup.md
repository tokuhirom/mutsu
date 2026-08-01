# `&CALLER::LEXICAL::("infix:<+>")` — indirect symbol lookup of an operator

An indirect (computed-name) symbol lookup through the `CALLER::LEXICAL::`
pseudo-package throws instead of resolving, and throws instead of returning a
`Failure` when the symbol genuinely does not exist.

```
$ mutsu -e 'my $m = &CALLER::LEXICAL::("infix:<+>"); say $m.WHAT; say $m(2,3)'
No such symbol 'LEXICAL::infix:<+>'
  in block <unit> at -e line 1

$ raku -e 'my $m = &CALLER::LEXICAL::("infix:<+>"); say $m.WHAT; say $m(2,3)'
(Sub+{is-pure})
5
```

Two separate things are missing:

1. **Resolution.** `&CALLER::LEXICAL::("<string>")` should look the name up in
   the caller's lexical scope, and an operator's canonical name
   (`infix:<+>`, `infix:«le»`, …) has to resolve to the routine that implements
   it — including mutsu's built-in operators, which are not ordinary registered
   subs.
2. **Absence is a `Failure`, not a throw.** raku returns an undefined `Failure`
   for a name that is not there, which is what makes the `//` fallback chain
   below work at all. mutsu raises immediately, so the chain never gets to try
   its second and third forms.

## Why it matters

It is how rakudo's `Test.rakumod` turns `cmp-ok`'s string operator into a
callable:

```raku
my $matcher = nqp::istype($op, Callable) ?? $op
    !! &CALLER::LEXICAL::("infix:<$op>")                                     #1
        // &CALLER::LEXICAL::("infix:«$op»")                                 #2
        // &CALLER::LEXICAL::("infix:<$op.subst(/<?before <[<>]>>/, "\\", :g)>"); #3
```

The three forms exist precisely because #1 cannot express `<` and `>`; each is
expected to *fail softly* so the next is tried. On mutsu the first one throws:

```
$ mutsu -I tmp/core -e 'use Test2; plan 1; cmp-ok 3, "<", 5, "cmp-ok"'
No such symbol 'LEXICAL::infix:<\<>'
  in sub cmp-ok at tmp/core/Test2.rakumod line 266
```

`cmp-ok` is the only assertion of the genuine upstream module that is blocked on
this — everything else in the happy path already runs
(`todo/tickets/vendor-real-test-module.md`).

## Note on scope

The pseudo-package machinery mutsu already has (`CALLER::`, `OUTER::`,
`SETTING::`, `DYNAMIC::` are recognized in `is_interpreter_handled_function`)
handles *static* names. This ticket is about the `::("...")` indirect form and
about operator names specifically; a general `&CALLER::LEXICAL::($name)` for
ordinary sub names is the easier half and is worth doing first.
