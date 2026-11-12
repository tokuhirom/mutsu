use v6;
use Test;

# A method called on a parameterized role type object (`R[Int].m`, or the
# `$M` that `.^parameterize` returns) runs the method of the candidate the
# arguments select, on that parameterization's pun. mutsu remembers which pun
# a call went to; the calls below repeat and interleave parameterizations,
# candidates and roles so that a remembered pun served to the wrong one would
# show up as a wrong answer. Verified against Rakudo.

plan 9;

role R[::T] {
    method m() { "generic-{T.^name}" }
    method twice($x) { "{T.^name}:{$x * 2}" }
}
role R[Str] {
    method m() { 'str' }
}
role V[Int $n] {
    method m() { "value-$n" }
}
role S[::T = Any] {
    method kind() { T.^name }
    method self-type() { self.^name }
}

my $RI = R[Int];
my $RS = R[Str];
my $SA = S.^parameterize(Rat);
my @got;
for ^25 -> $i {
    @got.push: $RI.m eq 'generic-Int';
    @got.push: $RS.m eq 'str';
    @got.push: R[Num].m eq 'generic-Num';
    @got.push: V[$i].m eq "value-$i";
    @got.push: $RI.twice($i) eq "Int:{$i * 2}";
    @got.push: $SA.kind eq 'Rat';
    @got.push: S.kind eq 'Any';
    @got.push: S[Int].kind eq 'Int';
}
ok @got.all, 'every repeated call runs the selected candidate';
is $RI.m, 'generic-Int', 'a type-object parameterization after the loop';
is $RS.m, 'str', 'a more specific candidate after the loop';
is V[7].m, 'value-7', 'a value parameterization is selected afresh';
is $SA.kind, 'Rat', 'a .^parameterize result keeps its argument';
is S.kind, 'Any', 'the bare role uses its default';
is $SA.self-type, 'S[Rat]', 'self is the parameterization in the method';

# A class composing the role after the calls above still sees its own pun.
class C does S[Str] { }
is C.kind, 'Str', 'a class composed afterwards reads its own argument';
is S[Str].kind, 'Str', 'and the role parameterization agrees';
