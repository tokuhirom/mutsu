# A class declared in a routine writes outer variables directly

```raku
my $log = '';
sub f { my class K { $log ~= 'body;' } }
f(); f();
say $log;   # body;body;
```

The body of a `my class` declared inside a routine runs each time the routine
does. A variable of an outer scope that the body writes is now captured by the
routine like any other variable it uses, so the write reaches the outer
scope at once instead of only when the routine returns (ADR-12529 §3,
slice 35).
