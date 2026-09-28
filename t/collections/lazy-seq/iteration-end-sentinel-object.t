use Test;

plan 20;

# `IterationEnd` is a unique `Mu` object compared by identity, not the string
# "IterationEnd", and a `for` loop stops at it wherever it sits (#9809).

is IterationEnd.raku, 'IterationEnd', '.raku is the bare name';
is IterationEnd.^name, 'Mu', 'it is a Mu instance';
is IterationEnd.gist, 'IterationEnd', '.gist is the bare name';
is IterationEnd.Str, 'IterationEnd', '.Str is the bare name';
ok IterationEnd.defined, 'it is defined';
ok IterationEnd.WHAT =:= Mu, '.WHAT is Mu';
is [1, IterationEnd].raku, '[1, IterationEnd]', 'renders by name inside a container';

ok IterationEnd =:= IterationEnd, 'identical to itself';
nok 'IterationEnd' =:= IterationEnd, 'a Str spelling its name is not the sentinel';
ok ::('IterationEnd') =:= IterationEnd, 'indirect lookup finds the same object';
my $p;
$p := IterationEnd;
ok $p =:= IterationEnd, 'a bound variable is identical to it';

my @seen;
@seen.push($_) for ['foo', IterationEnd, 'baz'];
is-deeply @seen, ['foo'], 'for over an Array stops at the sentinel';
@seen = ();
@seen.push($_) for 1, IterationEnd, 3;
is-deeply @seen, [1], 'for over a List stops at the sentinel';
@seen = ();
@seen.push($_) for gather { take 1; take IterationEnd; take 3 };
is-deeply @seen, [1], 'for over a lazy gather stops at the sentinel';
@seen = ();
for 1, 2, IterationEnd, 4 -> $a, $b? { @seen.push("$a$b") }
is-deeply @seen, ['12'], 'a chunked for loop stops at the sentinel';
@seen = ();
@seen.push($_) for 'a', 'IterationEnd', 'b';
is-deeply @seen, ['a', 'IterationEnd', 'b'], 'the Str "IterationEnd" does not stop iteration';

is [1, IterationEnd, 3].elems, 3, 'an Array still stores it as an element';

class I does Iterator {
    has $.n = 0;
    method pull-one { $!n < 3 ?? $!n++ !! IterationEnd }
}
is-deeply Seq.new(I.new).list, (0, 1, 2), 'a user iterator returning IterationEnd ends a Seq';
my @b;
ok I.new.push-exactly(@b, 5) =:= IterationEnd, 'push-exactly past the end answers the sentinel';
my $it = (1, 2).iterator;
$it.pull-one for ^2;
ok $it.pull-one =:= IterationEnd, 'a built-in iterator answers the same sentinel';
