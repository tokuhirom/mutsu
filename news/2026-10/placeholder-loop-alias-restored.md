# A loop body's placeholder no longer leaks its `$b` alias

A `for` body's placeholder `$^b` also binds a bare `$b` for the body. The loop
restored the parameter's prior binding when it ended, but not the alias. Inside
a closure, the alias then replaced the captured outer `$b` for the rest of the
closure, and the frame's merge carried it into the caller:

```raku
my $b = 99;
my $g = { for 1 { $^b }; $b };
say $g(42);   # was 1, now 99
say $b;       # was 1, now 99
```

The saved binding is now a `ForParamRestore` that also carries the alias's
prior binding, and all three loop paths (the general body, the integer range,
and the deferred `RestoreForParam`) restore both through one function (#13377).
