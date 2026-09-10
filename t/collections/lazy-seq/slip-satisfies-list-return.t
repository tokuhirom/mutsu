use v6;
use Test;

# A Slip is-a List (does Positional, is-a Cool) in Raku's type hierarchy, so a
# `|(...)` slip must satisfy a `--> List` / `--> Positional` return constraint and
# a `List`/`Positional`/`Cool` parameter. Regression: mutsu's static type table
# omitted Slip, so zef's `method !slurp-package-list(--> List) { ... try |from-json(...) }`
# died with "expected List but got Slip".

plan 8;

sub ret-list(--> List) { return |(1, 2, 3) }
is ret-list().join(','), '1,2,3', 'Slip satisfies a --> List return constraint';

sub ret-positional(--> Positional) { return |(4, 5) }
is ret-positional().join(','), '4,5', 'Slip satisfies a --> Positional return constraint';

sub ret-cool(--> Cool) { return |(6, 7) }
is ret-cool().elems, 2, 'Slip satisfies a --> Cool return constraint';

my $s = |(8, 9, 10);
ok $s ~~ List, 'Slip ~~ List';
ok $s ~~ Positional, 'Slip ~~ Positional';
ok $s ~~ Cool, 'Slip ~~ Cool';
ok $s.isa(List), 'Slip.isa(List)';

# Empty slip return still satisfies List.
sub ret-empty(--> List) { return Empty }
is ret-empty().elems, 0, 'empty Slip (Empty) satisfies --> List';
