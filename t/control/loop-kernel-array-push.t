use Test;

# `@a.push($v)` inside a counted loop may run in a loop kernel. Every shape
# must leave what the generic push leaves, including the ones a kernel must
# refuse: a typed array, a read of the same array, a non-numeric value, an
# immutable List, a bound alias.

plan 11;

sub ints() {
    my @a;
    for 1..5000 -> $i { @a.push($i) }
    @a.elems ~ ' ' ~ @a[0] ~ ' ' ~ @a[*-1] ~ ' ' ~ [+] @a
}
is ints(), '5000 1 5000 12502500', 'Int pushes';

sub nums() {
    my @a;
    for ^2000 -> $i { @a.push($i / 4e0) }
    @a.elems ~ ' ' ~ @a[*-1]
}
is nums(), '2000 499.75', 'Num pushes';

sub onto-existing() {
    my @a = <x y>;
    for ^3000 -> $i { @a.push($i * 2) }
    @a.elems ~ ' ' ~ @a[0] ~ ' ' ~ @a[2] ~ ' ' ~ @a[*-1]
}
is onto-existing(), '3002 x 0 5998', 'onto an array with other elements';

sub typed() {
    my Int @a;
    for ^3000 -> $i { @a.push($i) }
    @a.elems ~ ' ' ~ @a.of.^name
}
is typed(), '3000 Int', 'a typed array';

sub typed-fails() {
    my Str @a;
    for ^3000 -> $i { @a.push($i) }
    CATCH { default { return 'died' } }
    'lived'
}
is typed-fails(), 'died', 'a typed array still checks the pushed value';

sub reads-itself() {
    my @a = 1;
    for ^3000 -> $i { @a.push(@a[$i] + 1) }
    @a[*-1]
}
is reads-itself(), 3001, 'pushing while reading the same array';

sub aliased() {
    my @b;
    my @a := @b;
    for ^3000 -> $i { @a.push($i) }
    @b.elems
}
is aliased(), 3000, 'through a := alias';

sub two-arrays() {
    my (@even, @odd);
    for ^3000 -> $i { @even.push($i * 2); @odd.push($i * 2 + 1) }
    @even[*-1] + @odd[*-1]
}
is two-arrays(), 11997, 'two arrays in one loop';

sub list-dies() {
    my @a := (1, 2).List;
    for ^3000 -> $i { @a.push($i) }
    CATCH { default { return 'died' } }
    'lived'
}
is list-dies(), 'died', 'pushing onto a List still dies';

sub containers() {
    my @a;
    for ^3000 -> $i { @a.push($i) }
    @a[5] = 'changed';
    @a[5] ~ ' ' ~ @a[6]
}
is containers(), 'changed 6', 'pushed elements are assignable';

sub the-value() {
    my @a;
    my $r;
    for ^3000 -> $i { $r = @a.push($i) }
    $r.elems
}
is the-value(), 3000, 'the push result as a value';
