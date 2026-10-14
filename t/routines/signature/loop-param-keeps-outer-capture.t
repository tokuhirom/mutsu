use Test;

# A `for` body's placeholder `$^b` binds a bare `$b` alias for that body only.
# The alias is restored with the parameter when the loop ends, so the
# enclosing closure reads the outer `$b` after the loop and the outer
# variable is never touched (#13377).

plan 5;

my $b = 99;
my $placeholder = { for 1 { $^b }; $b };
is $placeholder(42), 99, 'a placeholder of the loop body does not leak into the closure';
is $b, 99, '... nor into the outer variable';

my $twice = { for 1, 2 { $^b }; for 3 { $^b }; $b };
is $twice(), 99, 'two placeholder loops in a row restore the alias each time';

sub in-sub { for 1 { $^b }; $b }
is in-sub(), 99, 'a named sub keeps reading the outer variable too';
is $b, 99, 'the outer variable is untouched after all of them';
