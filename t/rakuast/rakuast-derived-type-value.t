use experimental :rakuast;
use Test;

# The compile-time value of a derived type node (#14916), as rakudo 2026.09:
# `Type::Definedness` (`Int:D`), `Type::Parameterized` (`Array[Int]`) and
# `Type::Coercion` (`Str(Int)`) in `$*CU` answer the type they name through
# `compile-time-value` and `maybe-compile-time-value`. A hand-built one whose
# type names are not resolved dies on the first unresolved name: the
# arguments of a parameterization before its base, a coercion's base before its
# constraint.

plan 12;

my %seen;
CHECK {
    sub walk($n) {
        if $n ~~ RakuAST::Type::Definedness | RakuAST::Type::Parameterized | RakuAST::Type::Coercion {
            my $v = $n.compile-time-value;
            %seen{$v.^name} //= [$v, $n.maybe-compile-time-value, $n.has-compile-time-value];
        }
        $n.visit-children(&walk);
    }
    walk($*CU.statement-list);
}

my Int:D $a = 1;
my Array[Int] $b;
sub f(Str(Int) $c, Hash[Str, Int] $e) { }

ok 1 ~~ %seen<Int:D>[0] && Int !~~ %seen<Int:D>[0], 'Int:D is the definite Int type';
is %seen<Int:D>[1].^name, 'Int:D', 'maybe-compile-time-value answers it too';
ok %seen<Int:D>[2], 'it has a compile-time value';
ok %seen<Array[Int]>[0].of === Int, 'Array[Int] is the parameterized type';
ok %seen<Hash[Str,Int]>[0].keyof === Int, 'Hash[Str,Int] too';
is %seen<Str(Int)>[0].^name, 'Str(Int)', 'Str(Int) is the coercion type';
ok 42 ~~ %seen<Str(Int)>[0], 'which accepts its constraint';

sub simple($name) { RakuAST::Type::Simple.new(RakuAST::Name.from-identifier($name)) }
my $definite = RakuAST::Type::Definedness.new(base-type => simple('Int'), :definite);
throws-like { $definite.compile-time-value }, Exception,
    message => /"'Int' has not been resolved."/, 'a hand-built Int:D is unresolved';
my $param = RakuAST::Type::Parameterized.new(base-type => simple('Array'),
    args => RakuAST::ArgList.new(simple('Str')));
throws-like { $param.compile-time-value }, Exception,
    message => /"'Str' has not been resolved."/, "a parameterization's arguments first";
my $coerce = RakuAST::Type::Coercion.new(base-type => simple('Str'), constraint => simple('Int'));
throws-like { $coerce.maybe-compile-time-value }, Exception,
    message => /"'Str' has not been resolved."/, "a coercion's base first";

ok RakuAST::Term::Name.new(RakuAST::Name.from-identifier('pi')).^can('maybe-compile-time-value'),
    'Term::Name can maybe-compile-time-value';
is $a, 1, 'the probed declarations still run';
