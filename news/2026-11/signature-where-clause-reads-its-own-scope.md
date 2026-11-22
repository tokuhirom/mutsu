# A signature's `where` clause reads its own scope

```raku
my &good = { True };
my $sig = :($x where &good);
sub check($c) { my &good = { False }; $c ~~ $sig }
say check(\(42));    # True
```

A `:(...)` signature literal was a constant the parser built, with no scope.
`Capture ~~ Signature` therefore ran its `where` clauses in the scope of
whoever smartmatched. A caller's own `&good` answered instead of the one next
to the literal, and a name declared only beside the literal was not found at
all. A literal whose parameters carry code, meaning a `where` clause or a
computed default, is now built at run time from an empty routine.
`Signature.ACCEPTS` binds a capture by calling that routine, so its clauses run
in the routine's own scope, the way rakudo's trial bind does.

A closure also captures the names that only its signature reads now.
`sub mk { my &ok = ...; sub ($x where &ok) { } }` used to lose `&ok` once
`mk` returned, because only the body's reads were captured.

Both bugs had been hidden by the caller walk that ADR-12529 retires (#18248).
This fix is what allows that walk to go.
