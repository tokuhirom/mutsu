use Test;

plan 2;

# A colonpair argument of a parameterized type spelling (`NT[Int, :coerce(&cf)]`,
# as in PDF::Names) must survive the RakuAST round trip of the declaration.
role NT[::T, :&coerce] { method tag { 'nt' } }
sub cf($x) { $x }

class A {
    has NT[Int, :coerce(&cf)] $.d;
}

lives-ok { A.new }, 'an attribute typed with a colonpair type argument declares';
is NT[Int, :coerce(&cf)].new.tag, 'nt', 'the parameterized role with a named argument composes';

done-testing;
