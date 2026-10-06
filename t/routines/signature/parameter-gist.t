use v6;
use Test;

plan 5;

sub f(Int $, @, %, Str:D $y, $?, :$x) { }
my @params = &f.signature.params;

is @params.map(*.gist).join(" | "), 'Int $ | @ | % | Str:D $y | $? | :$x',
    'Parameter.gist is the same text as .raku';
is @params[3].gist, @params[3].raku, '.gist agrees with .raku';

class K { method m(K:D: Int $c) { } }
my @m = K.^find_method('m').signature.params;
is @m[0].gist, 'K:D $:', 'invocant parameter gist';
is @m[1].gist, 'Int $c', 'positional parameter gist';
is @m[1].Str.chars > 0, True, '.Str still produces text';
