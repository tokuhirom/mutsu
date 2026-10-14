use Test;
plan 4;

# #13474: a `for` pointy sigilless parameter is not overwritten by a later
# `my $a` in the same block body.
for [[1, 2],] -> \a {
    my $a = 45;
    is a.WHAT.gist, '(Array)', 'sigilless loop param keeps its value';
    is $a, 45, 'the scalar is its own variable';
}

for [[1, 2],], Any -> \a, \T {
    my $a = 45;
    is a.elems, 2, 'multi-param sigilless loop param survives a later my';
}

my @r;
for 1..2 -> \n { my $n = 10; @r.push(n + $n) }
is @r, (11, 12), 'every iteration';
