use Test;

# `my $x := $(%h)` / `:= $(@a)` / `:= (1, 2).item` binds the name to the
# readonly Scalar that `.item` puts the aggregate in: `.VAR.^name` is `Scalar`,
# `=:=` sees a container, and an assignment is "Cannot assign to a readonly
# variable or a value". An unitemized aggregate bound the same way has no
# Scalar (#11129). Every expectation was measured on rakudo.

plan 10;

my %h = a => 1;
my @a = 1, 2;

my $x := $(%h);
is $x.VAR.^name, 'Scalar', ':= $(%h) binds a Scalar';
my $y := $(@a);
is $y.VAR.^name, 'Scalar', ':= $(@a) binds a Scalar';
my $q := (1, 2).item;
is $q.VAR.^name, 'Scalar', ':= (1, 2).item binds a Scalar';
ok $x =:= $x, 'and it is a container';

throws-like { $x = 3 }, X::AdHoc, message => 'Cannot assign to a readonly variable or a value',
    'the Scalar is readonly';
throws-like { $q = 3 }, X::AdHoc, message => 'Cannot assign to a readonly variable or a value',
    'likewise for an itemized List';
is $x.raku, '${:a(1)}', 'the binding is unchanged after the failed assignment';

my @r = $x;
is @r.elems, 1, 'it stays one item in list assignment';

my $z := {a => 1};
is $z.VAR.^name, 'Hash', ':= of an unitemized hash literal has no Scalar';
throws-like { $z = 3 }, X::AdHoc, message => 'Cannot assign to an immutable value',
    'and assigning to it is the immutable-value error';
