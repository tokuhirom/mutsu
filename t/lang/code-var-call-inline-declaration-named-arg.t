use Test;

# `f :p(my $x), { ... $x ... }` through a code variable: the declaration in
# the named argument gets a lexical slot, so the closure beside it shares the
# variable an `is raw` parameter writes (#16121). Checked against rakudo.

plan 4;

my &wr = -> &b, :$deps is raw { $deps = 8; b() };
my $seen;
wr :deps(my $n), { $seen = $n };
is $seen, 8, 'the closure sees the write through a named is-raw argument';
is $n, 8, 'and the declared variable holds it after the call';

my &wp = -> :$deps is raw { $deps = 9 };
wp :deps(my $q);
is $q, 9, 'without a closure';

sub wsub(&b, :$deps is raw) { $deps = 7; b() }
my $s2;
wsub :deps(my $s), { $s2 = $s };
is $s2, 7, 'the named-sub form still works';
