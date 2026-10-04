# A class-body enum key is undeclared outside the class

`class CC { enum E <bar> }; say bar` printed `bar`: once #11685 stopped the
key's bare binding from outliving the class body, the read fell through to the
bareword fallback, which answers the name itself as a `Str`. The class body now
records each enum key it scoped out with no outer binding of the same name, and
a bareword that nothing else resolves dies with rakudo's "Undeclared routine"
(`X::Undeclared::Symbols`) instead (#11719). The record is read only by that
last-resort fallback, so a key named like a native type or builtin routine
(`enum F <array cos>`) still hides neither, and any later declaration of the
name wins. The unrelated `my $bar = 3; say bar` → `(Any)` probe from the same
issue is #11898.
