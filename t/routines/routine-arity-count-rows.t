use Test;

# Code.arity and Code.count are rows of the method table (ADR-12523, slice 2).

plan 22;

sub f($a, $b?, :$c) { }
sub s(*@r) { }
my &g = -> $x, $y { };
multi mm(Int $a) { }
multi mm(Str $a, $b) { }
class C { method m($x, $y?) { } }

is &f.arity, 1, 'optional and named parameters are not required';
is &f.count, 2, 'count is every positional';
is &s.arity, 0, 'a slurpy is not required';
is &s.count, Inf, 'a slurpy makes count Inf';
is &g.arity, 2, 'pointy block arity';
is &g.count, 2, 'pointy block count';
is { $_ }.arity, 0, 'a block with the implicit topic has arity 0';
is { $_ }.count, 1, '... and count 1';
is { 1 }.arity, 0, 'block without parameters';
is { 1 }.count, 1, '... still takes the implicit topic';
is -> { }.arity, 0, 'an empty pointy block';
is -> { }.count, 0, '... takes no argument';
is &mm.arity, 0, 'a multi dispatcher answers its generated proto (|)';
is &mm.count, Inf, '... which takes anything';
is C.^lookup('m').arity, 2, 'a method counts its invocant';
is C.^lookup('m').count, 3, '... in count too';
is /a/.arity, 1, 'a regex takes the cursor';
is /a/.count, 1, '... in count too';
is &chars.arity, 1, 'a builtin sub';
is &prefix:<->.count, 1, 'a builtin prefix operator';
is (sub ($a) { }).arity, 1, 'an anonymous sub';
is (sub ($a) { }).count, 1, '... and its count';
