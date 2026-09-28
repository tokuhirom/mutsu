use Test;

plan 13;

# A `my subset` has declaration-site identity (ADR-0047 P1, #9894): two
# same-named lexical subsets in different scopes are two different types.
# They used to share one registry entry keyed by the bare name, so the last
# declaration won everywhere. Found via the Java::Generate distribution, whose
# PrefixOp / PostfixOp / InfixOp classes each declare their own `my subset Op`.

class PrefixOp {
    my constant %known := set '++', '--', '!';
    my subset Op of Str where %known{$_}:exists;
    has Op $.op;
    method ok-op($x) { $x ~~ Op }
}
class PostfixOp {
    my constant %known := set '++', '--';
    my subset Op of Str where %known{$_}:exists;
    has Op $.op;
}
class InfixOp {
    my constant %known := set '+', '-';
    my subset Op of Str where %known{$_}:exists;
    has Op $.op;
}

is PrefixOp.new(:op<!>).op, '!', 'first class checks its attribute against its own subset';
is PostfixOp.new(:op<++>).op, '++', 'second class checks against its own subset';
is InfixOp.new(:op<+>).op, '+', 'last class checks against its own subset';
dies-ok { PostfixOp.new(:op<+>) }, 'a value valid only for a sibling subset is rejected';
dies-ok { InfixOp.new(:op<++>) }, 'and the other way around';
ok PrefixOp.ok-op('!'), 'a method sees its own class\'s subset';
nok PrefixOp.ok-op('+'), 'and not a sibling\'s';

# Type objects that escape their block keep their own identity.
my ($a, $b);
{ my subset S of Int where * > 5; $a = S }
{ my subset S of Int where * < 0; $b = S }
ok 7 ~~ $a, 'first escaped subset keeps its predicate';
nok 7 ~~ $b, 'second escaped subset keeps its own';
is $a.^name, 'S', 'a mainline-block subset is named by its short name';

# Still lexical: no package-qualified alias.
throws-like 'module M { my subset F where 5 }; my M::F $f = 5', Exception,
    'a `my subset` in a module is not reachable by its qualified name';

# A lexical subset's name is qualified by the package it is declared in.
class Holder { my subset Small of Int where * < 10; method name { Small.^name } }
is Holder.name, 'Holder::Small', '.^name of a class-body `my subset`';

# A return constraint naming a lexical subset reports that same type object.
{
    my subset ofTest where True;
    ok (-> () --> ofTest {}).of =:= ofTest, '.of of `--> my-subset` is the subset itself';
}
