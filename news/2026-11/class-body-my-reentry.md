# A class body run again reads its own `my`, not the previous run's

A `my class` declared in a routine runs its body on every entry of the
routine. A statement of the body that read a `my` declared earlier in the
same body got the value the *previous* entry had persisted:

```raku
sub f($i) { my class C { my $z = $i; say "z=$z" }; C }
f(1); f(2);   # was z=1 z=1, now z=1 z=2 as in Rakudo
```

A class body's `my` statics are persisted into `package_lexicals` when the
body ends, where the class's methods read them, and a read inside the body
consults that store before the env the current run's declaration wrote. A
body now drops the statics a previous run persisted for the names it declares
before it runs; its exit persists them again (#18088).
