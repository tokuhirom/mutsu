use Test;

# A method called on a parameterized role type object runs on the role's pun,
# the same way on the first call and on every later one (mutsu dispatches a
# repeat call to the memoized pun directly, #17331).

plan 14;

role R[::T = Any] {
    method kind { T.^name }
    method me { self.^name }
    method add($a, $b) { $a + $b }
    method gist { "R-gist" }
    method defined-self { self.defined }
}

for ^3 {
    is R[Int].kind, 'Int', "R[Int].kind (call {$_ + 1})";
}
is R[Str].kind, 'Str', 'another parameterization keeps its own T';
is R[Int].me, 'R[Int]', 'self is the pun type object';
is R[Int].me, 'R[Int]', 'and stays so on a repeat call';
is R[Int].add(2, 3), 5, 'arguments are passed through';
is R[Int].add(4, 5), 9, 'on a repeat call too';
is R[Int].gist, '(R[Int])', 'gist on the role type object is the built-in one';
is R[Int].gist, '(R[Int])', 'also on a repeat call';
nok R[Int].defined-self, 'self is a type object';

my \M = R.^parameterize(Rat);
is M.kind, 'Rat', 'a ^parameterize result dispatches the same way';
is M.kind, 'Rat', 'repeatedly';
is (R[Int], R[Str]).map(*.kind).join(','), 'Int,Str', 'alternating receivers';
