use Test;

# A package-qualified type name is a valid routine trait (`is Path::Map(...)`,
# the Path::Map distribution's idiom): it dispatches `trait_mod:<is>` with the
# type object as a positional, exactly like an unqualified one. The
# parenthesized argument is ONE positional value after the type object.

plan 7;

class Foo::Bar { }
my %seen;
multi trait_mod:<is>(Code:D $h, Foo::Bar, Pair $p) { %seen{$h.name || 'anon'} = "pair {$p.key}={$p.value}" }
multi trait_mod:<is>(Code:D $h, Foo::Bar) { %seen{$h.name || 'anon'} = "bare" }

my $anon = sub (:$foo!) is Foo::Bar(:test<x>) { "received $foo" };
is $anon(foo => 1), 'received 1', 'anonymous sub with a qualified type trait parses and runs';
is %seen<anon>, 'pair test=x', 'qualified trait with a colonpair argument binds `Pair $p` (anon sub)';

sub named($x) is Foo::Bar(:k<v>) { $x * 2 }
is named(21), 42, 'named sub with a qualified type trait parses and runs';
is %seen<named>, 'pair k=v', 'colonpair trait argument is positional, not named (named sub)';

sub bare() is Foo::Bar { 'b' }
is bare(), 'b', 'qualified type trait without an argument';
is %seen<bare>, 'bare', 'argument-less qualified trait dispatches the two-arg candidate';

class Plain { }
my $got;
multi trait_mod:<is>(Routine:D $h, Plain, $arg) { $got = $arg.raku }
sub plain() is Plain(:a<b>) { }
is $got, ':a("b")', 'unqualified type trait: colonpair argument arrives as a positional Pair';
