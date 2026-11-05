use Test;
use experimental :rakuast;

plan 6;

sub rng($n) { my $o = $n.origin; $o ?? "{$o.from}-{$o.to}" !! "none" }

sub shape(Str $src) {
    $src.AST.statements[0].expression.signature.parameters.map({ rng($_) ~ '/' ~ rng(.target) }).join(' ')
}

is shape('sub (|c) { 1 }'),         '5-7/6-7',               'a capture parameter';
is shape('sub ($a) { 1 }'),         '5-7/5-7',               'a scalar parameter';
is shape('sub (\x) { 1 }'),         '5-7/6-7',               'a sigilless parameter';
is shape('sub (Int $a, :$b) { 1 }'), '5-11/9-11 13-16/14-16', 'a typed and a named parameter';
is shape('sub ($a, |c) { 1 }'),     '5-7/5-7 9-11/10-11',    'two parameters';
is shape('method ($a, $b) { 1 }'),  '8-10/8-10 12-14/12-14', 'a method literal';
