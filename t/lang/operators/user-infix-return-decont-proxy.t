use Test;

plan 7;

# A routine that is neither `is rw` nor `is raw` returns its result
# decontainerized; a user `infix:<op>` is no exception, whether it is called
# as an infix or through a metaoperator.
my $p := Proxy.new(FETCH => -> $ { "k1" }, STORE => -> $, $ { });

sub snd(Mu \a, Mu \b) { b }
is snd(1, $p).VAR.^name, 'Str', 'named call FETCHes a returned Proxy';

sub infix:<snd>(Mu \a, Mu \b) { b }
is (1 snd $p).VAR.^name, 'Str', 'infix call FETCHes a returned Proxy';

my @l := ($p,);
is ((0,) Z[snd] @l)[0].VAR.^name, 'Str', 'Z[op] FETCHes a returned Proxy';
is ((0 xx *) Z[snd] (gather { take-rw $p })).eager[0].VAR.^name, 'Str',
    'Z[op] over a lazy left operand FETCHes a returned Proxy';

sub infix:<rsnd>(Mu \a, Mu \b) is raw { b }
is (1 rsnd $p).VAR.^name, 'Proxy', 'an `is raw` infix keeps the Proxy';

# A Proxy subclass whose FETCH is raw returns the Proxy bound in its
# attribute: decontainerizing reads to the end of that chain.
my class Binder is Proxy {
    has $!value;
    method new is raw { callwith :&FETCH, :&STORE }
    my method FETCH is raw { $!value }
    my method STORE(Mu $topic is raw --> Nil) { $!value := $topic }
}
my $inner := Proxy.new(FETCH => -> $ { IntStr.new(2, "k1") }, STORE => -> $, $ { });
my $b := Binder.new;
$b = $inner;
is (1 snd $b).VAR.^name, 'IntStr', 'a Proxy chain is FETCHed to its value';
is ((0,) Z[snd] ($b,))[0].VAR.^name, 'IntStr', 'Z[op] reads a Proxy chain too';
