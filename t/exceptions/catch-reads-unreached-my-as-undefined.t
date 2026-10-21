use Test;

# A `my` the block never reached (the `die` comes first) is still a declared
# variable: a CATCH reading it sees an undefined value, not X::Undeclared
# (#14325).
plan 3;

my $seen;
do { die "foo"; my $x; CATCH { default { $seen = $x.defined } } };
is $seen, False, 'a CATCH reads an unreached my as undefined';

my $kind;
do { die "foo"; my @a; CATCH { default { $kind = @a.elems } } };
is $kind, 0, 'an unreached my array reads as empty';

my $ok = True;
do { die "foo"; my Int $n; CATCH { default { $ok = $n.defined } } };
is $ok, False, 'an unreached typed my reads as undefined';
