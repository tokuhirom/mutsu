use Test;

plan 6;

# A routine whose `$/` is a parameter keeps that match: a bare block inside it
# that matches or substitutes binds its own `$/` (the parameter is readonly).
sub subst-in-map($/) {
    my $emph = '**';
    my @res = ('**a', '**b').map({ $_.subst(/ ^ $emph /, '') });
    is $/.Str, 'y', 'a .subst in a map block leaves the parameter $/';
    is @res.join(','), 'a,b', 'the substitutions still happen';
}
"xyz" ~~ /y/;
subst-in-map($/);

sub smartmatch-in-block($/) {
    my &f = { "q" ~~ /q/ };
    f();
    is $/.Str, 'y', 'a ~~ in a called block leaves the parameter $/';
}
"xyz" ~~ /y/;
smartmatch-in-block($/);

# Without a `$/` parameter the block still shares the routine's `$/`.
sub shared() {
    "xyz" ~~ /y/;
    (1).map({ "q" ~~ /q/ }).eager;
    is $/.Str, 'q', 'a block in a routine without a $/ parameter shares its $/';
}
shared();

# `make` in a block inside an action still reaches the Match.
grammar G { token TOP { <w> } token w { \w+ } }
class A {
    method TOP($/) { make 1 }
    method w($/) { my @x = (1).map({ $/.Str }); make @x[0] }
}
my $m = G.parse('abc', :actions(A.new));
is $m<w>.made, 'abc', 'a block reads the parameter $/';
is $m.made, 1, 'make sets .made from the action';
