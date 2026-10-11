use Test;
use nqp;

plan 10;

# nqp::setparameterizer / parameterizetype / typeparameterized /
# typeparameters / typeparameterat (#11499)
my $made = 0;
package Cache {
    sub make(Mu $root, Mu $args) is raw {
        $made++;
        Metamodel::Primitives.create_type(Metamodel::ClassHOW.new_type(name => "Made"), 'Uninstantiable')
    }
    BEGIN nqp::setparameterizer($?PACKAGE, nqp::getattr(&make, Code, q[$!do]));
    our sub par(*@a) is raw {
        my $l := nqp::list();
        nqp::push($l, $_) for @a;
        nqp::parameterizetype($?PACKAGE, $l)
    }
    our sub root(Mu $t) is raw { nqp::typeparameterized($t) }
    our sub params(Mu $t) is raw { nqp::typeparameters($t) }
    our sub at(Mu $t, $i) is raw { nqp::typeparameterat($t, $i) }
}

my $p1 := Cache::par(Int, Str);
my $p2 := Cache::par(Int, Str);
ok $p1 =:= $p2, 'the same arguments share one parameterization';
is $made, 1, 'the parameterizer ran once';
my $p3 := Cache::par(Num);
nok $p3 =:= $p1, 'different arguments build a new parameterization';
is $made, 2, 'and the parameterizer ran again';

is Cache::root($p1).^name, 'Cache', 'typeparameterized answers the root';
ok nqp::isnull(Cache::root(Cache)), 'a root is not parameterized: null';
is nqp::elems(Cache::params($p1)), 2, 'typeparameters answers the argument list';
is Cache::at($p1, 1).^name, 'Str', 'typeparameterat indexes the arguments';
ok nqp::isnull(Cache::at($p1, 5)), 'typeparameterat past the end is null';
dies-ok { Cache::params(Cache) }, 'typeparameters of a non-parameterization dies';
