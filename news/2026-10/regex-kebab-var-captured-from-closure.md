# A regex keeps a kebab-case variable the enclosing closure captured

`sub a($x) { my $cont-name = $x; subtest 'k', { cmp-ok $got, &[~~], /$cont-name \s container/ } }` failed
because the regex literal captured a kebab-case name only from outer-scope stores, never from the closure's
env, so `Test::cmp-ok` matched it without `$cont-name`. A name that is one of the creating chunk's free
variables is now read from the closure env, like a short name. Fixes #14609.
