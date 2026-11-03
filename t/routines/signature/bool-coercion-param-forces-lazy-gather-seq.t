use Test;

# A `Bool()` / `Bool(Mu)` parameter is a boolean chokepoint: a not-yet-run
# `gather` Seq must have its first element read, or it coerces to False.
# Found via Distribution::Extension::Updater t/01-basic.rakutest, where
# `ok $distro.ext<pod6>` (a Seq from File::Find's `gather while`) failed
# while `so` of the same Seq passed.

plan 9;

sub g { my @t = 1..4; gather while @t { my $e = @t.shift; take $e if $e == 3 } }
sub none { my @t = 1..3; gather while @t { my $e = @t.shift; take $e if $e == 9 } }

sub c-mu(Bool(Mu) $c) { $c }
sub c-bare(Bool() $c) { $c }
sub c-any(Bool(Any) $c) { $c }

is c-mu(g()), True, 'Bool(Mu) of a gather Seq with an element';
is c-bare(g()), True, 'Bool() of a gather Seq with an element';
is c-any(g()), True, 'Bool(Any) of a gather Seq with an element';
is c-mu(none()), False, 'Bool(Mu) of a gather Seq that yields nothing';
is c-mu((1, 2).Seq), True, 'a plain Seq';
is c-mu(().Seq), False, 'an empty Seq';

my $s = g();
is c-mu($s), True, 'a gather Seq in a variable';
is $s.elems, 1, 'and it still has its element afterwards';

ok g(), 'ok on a gather Seq';
