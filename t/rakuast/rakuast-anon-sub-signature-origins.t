use Test;

# The parameters and signature of an anonymous `sub` / `method` carry the
# ranges rakudo 2026.09 gives them (#16643): each `Parameter` and its target,
# and the `Signature` from the first parameter to the end of the last.

plan 12;

use experimental :rakuast;

sub rng($n) { my $o = $n.origin; $o ?? "{$o.from}-{$o.to}" !! "none" }
sub shape(Str $src) {
    my $sig = $src.AST.statements[0].expression.signature;
    rng($sig) ~ " " ~ $sig.parameters.map({ rng($_) ~ "/" ~ rng(.target) }).join(",")
}

is shape('sub ($a, |c) { 1 }'), "5-11 5-7/5-7,9-11/10-11", 'a scalar and a capture';
is shape('sub ( $a ) { 1 }'), "6-8 6-8/6-8", 'spaces inside the parentheses';
is shape('sub ($x, $y?) { 1 }'), "5-12 5-7/5-7,9-12/9-11", 'an optional parameter';
is shape('sub ($x, $y = 2) { 1 }'), "5-15 5-7/5-7,9-15/9-11", 'a default';
is shape('sub (Int $x where * > 1) { 1 }'), "5-23 5-23/9-11", 'a type and a where clause';
is shape('sub ($a) returns Int { 1 }'), "5-7 5-7/5-7", 'a return type after the signature';
is shape('sub (\x) { 1 }'), "5-7 5-7/6-7", 'a sigilless parameter';
is shape('method ($x, $y?) { 1 }'), "8-15 8-10/8-10,12-15/12-14", 'a method literal';
is shape('method ($x) { 1 }'), "8-10 8-10/8-10", 'a method literal with one parameter';
is shape('sub (@a, %h) { 1 }'), "5-11 5-7/5-7,9-11/9-11", 'array and hash';
is shape('sub (:$n) { 1 }'), "5-8 5-8/6-8", 'a named parameter';
is shape('sub ($a, *@rest) { 1 }'), "5-15 5-7/5-7,9-15/10-15", 'a slurpy';
