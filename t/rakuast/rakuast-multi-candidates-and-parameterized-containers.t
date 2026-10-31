use Test;
use nqp;

# static::Analyzer matches a multi's Sub nodes with the dispatcher's candidates
# and reads parameterized container types (#15610). Expected values are
# rakudo 2026.09's.

plan 8;

multi sub f(Int $x) { 1 }
multi sub f(Any $x) { "s" }
sub g(Array[Int] $x) { 1 }
my Int @a;
my Str:D %h;
my @b;

my (@subs, @decls, @params);
CHECK {
    sub walk($n) {
        return unless $n ~~ RakuAST::Node;
        @subs.push: $n if $n ~~ RakuAST::Sub && $n.name.defined && $n.name.canonicalize eq 'f';
        @decls.push: $n if $n ~~ RakuAST::VarDeclaration::Simple && $n.name eq any('@a', '%h', '@b');
        @params.push: $n if $n ~~ RakuAST::Parameter && $n.target.name eq '$x';
        $n.visit-children({ walk($_) });
    }
    walk($*CU);
}

is @subs.elems, 2, 'both multi candidates are in the tree';
my @candidates = &f.candidates;
ok nqp::objectid(@subs[0].compile-time-value) == nqp::objectid(@candidates[0]), 'the first node is the first candidate';
ok nqp::objectid(@subs[1].compile-time-value) == nqp::objectid(@candidates[1]), 'the second node is the second candidate';
is @subs[0].compile-time-value.signature.raku, ':(Int $x)', 'with its own signature';
ok nqp::objectid(&f.candidates[0]) == nqp::objectid(&f.candidates[0]), 'a candidate keeps its object id';
ok @params[0].meta-object === @candidates[0].signature.params[0], 'a multi parameter is its candidate\'s';
is @decls.map({ .container-type.raku }).join(' '), 'Array[Int] Hash[Str:D] Array', 'container types keep the element type';
is @decls.map({ .return-type.raku }).join(' '), 'Positional[Int] Associative[Str:D] Positional', 'return types too';
