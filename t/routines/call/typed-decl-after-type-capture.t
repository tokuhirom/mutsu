use v6;
use Test;

# Once a `::T` capture has been bound anywhere, a typed declaration of a plain
# builtin type (`my Int $x`) still answers from its per-site memo, while a
# capture bound under the constraint's own name (`::Str`) still substitutes.

plan 6;

sub h(::T $x) { my T $y = $x; my Int $i = 3; $y.^name ~ $i }
is h(1.5), 'Rat3', 'a capture and a plain builtin declaration in one routine';

my @seen;
for ^3 { my Int $z = $_; @seen.push: $z }
is @seen.join, '012', 'a typed declaration in a loop after a capture was bound';
throws-like { my Int $w = 'x' }, X::TypeCheck::Assignment,
    'the constraint is still enforced';

sub g(::Str $x) { my Str $y = $x; $y.^name }
is g(42), 'Int', 'a capture under a builtin name substitutes for it';
is g(42), 'Int', 'again, from the same site';
is (my Str $s = 'ok'), 'ok', 'outside the capture the builtin name is the builtin';
