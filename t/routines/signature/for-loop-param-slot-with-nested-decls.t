use Test;

# A pointy `for` parameter is a local slot of the loop body even when the
# body declares the same name in a nested block or declares a lexical `sub`
# (ADR-12529). The nested `my` shadows it only inside its own block, and the
# `sub` reads the current iteration's binding. Every expectation was checked
# against rakudo.

plan 8;

my @r;
for 1, 2 -> $y { { my $y = 'inner'; @r.push: $y }; @r.push: $y }
is-deeply @r, ['inner', 1, 'inner', 2], "a nested block's `my` shadows the parameter only in its block";

@r = ();
for 1, 2 -> $y { { my $y = 'inner' }; @r.push: $y }
is-deeply @r, [1, 2], 'the parameter is intact after the nested block';

@r = ();
for 1, 2 -> $v { sub ms { $v }; @r.push: ms() }
is-deeply @r, [1, 2], 'a lexical sub in the body reads the current iteration';

@r = ();
for 1, 2 -> $w { our sub os { $w }; @r.push: os() }
is-deeply @r, [1, 2], 'an `our` sub in the body reads the current iteration';

my @c;
for 1, 2 -> $x { { my $x = 9; @c.push: { $x } }; @c.push: { $x } }
is-deeply @c.map({ .() }).List, (9, 1, 9, 2), 'closures over the shadow and over the parameter';

my $q = 'outer';
for 1, 2 -> $q { @r.push: $q }
is $q, 'outer', 'the enclosing same-named variable is untouched after the loop';

@r = ();
for 1, 2 -> $x { my $f = -> { $x * 2 }; @r.push: $f() }
is-deeply @r, [2, 4], 'a pointy block in the body reads the parameter';

sub in-routine() {
    my @o;
    for <a b> -> $k { { my $k = 'z'; @o.push: $k }; @o.push: $k }
    @o.join(',')
}
is in-routine(), 'z,a,z,b', 'the same inside a routine';
