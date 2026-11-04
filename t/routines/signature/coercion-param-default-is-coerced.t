use Test;

# From the IdClass distribution: `-> Id1() $a, Str() $b = $a { ... }` -- a
# coercion-typed parameter's DEFAULT is coerced like a supplied argument.
plan 7;

class P { method Str { "str-of-P" } }

sub positional(P $a, Str() $b = $a) { $b }
is positional(P.new), 'str-of-P', 'a positional default is coerced';
is positional(P.new).^name, 'Str', 'and is a Str';
is positional(P.new, 'given'), 'given', 'a supplied argument is untouched';

sub named(P :$a, Str() :$b = $a) { $b }
is named(:a(P.new)), 'str-of-P', 'a named default is coerced';

my &pointy := -> P $a, Str() $b = $a { $b };
is pointy(P.new), 'str-of-P', 'a pointy block default is coerced';

sub int-default(Int() $n = "42") { $n }
is int-default().^name, 'Int', 'a literal default is coerced to the target type';
is int-default(), 42, 'with its value';
