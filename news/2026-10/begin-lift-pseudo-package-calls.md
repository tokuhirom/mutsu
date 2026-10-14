# A BEGIN calling through a pseudo-package is lifted

A nested `BEGIN` whose body called a routine qualified by a pseudo-package
(`GLOBAL::helper()`, `MY::helper()`, `OUTER::helper()`) was not lifted into the
unit's BEGIN prologue (ADR-0134), and through `Lifted::halted` neither was any
later nested `BEGIN`, so they never ran:

```raku
sub f { our sub helper { 'h' }; BEGIN say GLOBAL::helper() }
sub g { BEGIN say 'b' }
say 'm';   # raku and now mutsu: h, b, m — mutsu printed only m
```

Such a body already reaches every name of its scopes, so the rule that refuses
a body calling an unknown routine no longer applies to it (#13096). `MY::h()`
and `OUTER::h()` find no routine on rakudo either; they now fail at BEGIN time
on both, as they do at run time.
