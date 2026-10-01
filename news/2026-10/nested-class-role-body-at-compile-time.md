# A class nested in code runs its roles' bodies at compile time

`our $n; role R { $n++; method m { } }; sub f { class C does R { } }; say $n`
printed `(Any)`: the compile-time shell of a class declared inside a routine
or block (#10470) composed the role's methods but ran no role body, so the
body only ran when the enclosing code first did. Rakudo composes the class at
compile time, so it prints `1` whether or not `f` is ever called (#10494).

The shells are now a BEGIN-time effect of the unit's prologue (ADR-0134)
in a unit where a nested class composes a role (elsewhere they run no user
code and stay at the head of the unit; #10524 tracks the partition bugs that
keep it that way). The prologue collects each top-level statement's nested type declarations into
a `Stmt::NestedTypeShells` marker at that statement's place, after the
declarations that precede it, so a role body sees the unit's lexicals in
their static state: `my $n;` keeps the bump, while `my $n = 5;` overwrites it
at run time, as in Rakudo. The shell's composition goes through the
composition memo, so the in-place registration that repeats on every entry
of the enclosing code runs the body no more.

That registration rebuilds the composed methods, so a method a role body's
nested block declares (`role R[$n] { do { my $q = $n; method m { $q } } }`)
would have lost the capture the body's one run filed. Each composition now
keeps those captures, keyed by class and role, and a memo-skipped
re-registration gives them back. A class redeclared on every pass of a loop
gets the same fix.

Resolving a parameterized role's candidate binds its parameters in a trial
scope, which restored the env but not the parameters' readonly marks, so
`role A[:$a = 1]` composed as `A[:a(7)]` left a same-named outer `my $a`
unassignable. The trial binding now opens and closes its own readonly frame.
