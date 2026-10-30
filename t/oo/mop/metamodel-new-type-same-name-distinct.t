use Test;

# Two types minted by `new_type` under one name are two types: methods, HOW and
# identity stay apart, and an instance of one does not satisfy the other
# (#14711). Checked against rakudo.
plan 14;

my \A = Metamodel::ClassHOW.new_type(:name<Foo>);
A.^add_method("who", method () { "A" });
A.^compose;
my \B = Metamodel::ClassHOW.new_type(:name<Foo>);
B.^add_method("who", method () { "B" });
B.^compose;

is A.who, 'A', 'the first type keeps its method';
is B.who, 'B', 'the second type has its own';
nok A.HOW === B.HOW, 'the HOWs are two objects';
nok A === B, 'the type objects are two objects';
is A.^name, B.^name, 'they share the name';
nok A.WHICH eq B.WHICH, 'but not the identity';
is A.new.who, 'A', 'an instance of the first';
is B.new.who, 'B', 'an instance of the second';
nok B.new ~~ A, 'an instance of the second does not satisfy the first';
nok A.new ~~ B, 'nor the other way round';
ok B.new ~~ B, 'but satisfies itself';
nok B.new.does(A), '.does answers the same';
nok A.ACCEPTS(B.new), '.ACCEPTS too';

# Red's shape: a second type named after one still in use.
my \C = Metamodel::ClassHOW.new_type(:name<Bar>);
C.^add_method("all", method () { "C-all" });
C.^add_method("spec", method () {
    my \N = Metamodel::ClassHOW.new_type(:name(self.^name));
    N.^add_method("all", method () { "N-all" });
    N.^compose;
    N
});
C.^compose;
my $n = C.spec;
is-deeply (C.all, $n.all, C.all), ('C-all', 'N-all', 'C-all'),
    'the original type still answers after a same-named type was minted';
