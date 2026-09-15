use Test;

# A `Slip` is the one value that flattens out of whatever container it was
# read from -- that is its whole purpose. `exec_make_array_op`
# (`src/vm/vm_data_ops.rs`), the list-construction path, handled a
# `WrapVarRef`-tagged scalar-variable element and a hash/array-element
# `ContainerRef` cell FIRST -- to alias the source container so a later
# mutation is visible through the list -- and `continue`d before reaching
# the `ValueView::Slip(items) => elems.extend(...)` arm below. So a Slip read
# out of a container (a bare scalar variable, or a hash/array element) never
# got the chance to flatten; every other way of producing the same Slip
# (inline, returned from a sub, wrapped in parens, read via `.self`, or
# consumed by a `for` loop) already worked (issue #8465).
plan 9;

my $x = slip(5, 6);
is $x.^name, 'Slip', 'slip() returns a Slip';
is (1, $x, 2).elems, 4, 'a Slip read out of a scalar variable flattens in a List literal';

my @a = 1, $x, 2;
is @a.raku, '[1, 5, 6, 2]',
    'a Slip read out of a scalar variable flattens into an @ array store';

my %h;
%h<a> = slip(5, 6);
is (1, %h<a>, 2).elems, 4,
    'a Slip read out of a hash element flattens in a List literal';

my @arr;
@arr[0] = slip(7, 8);
is (1, @arr[0], 2).elems, 4,
    'a Slip read out of an array element flattens in a List literal';

# Every other way of producing the same Slip already worked -- pin them
# too, so a fix to the container-read gap cannot regress them.
my @ok = 1, (|(5, 6)), 2;
is @ok.raku, '[1, 5, 6, 2]', 'an inline Slip still flattens';

sub f() { slip(5, 6) };
my @ok2 = 1, f(), 2;
is @ok2.raku, '[1, 5, 6, 2]', 'a Slip returned from a sub still flattens';

my @ok3 = 1, ($x), 2;
is @ok3.raku, '[1, 5, 6, 2]', 'a parenthesized Slip variable read still flattens';

my @ok4 = 1, $x.self, 2;
is @ok4.raku, '[1, 5, 6, 2]', 'a Slip read via .self still flattens';
