use Test;

plan 9;

# `constant term:<$bar> = ...` declares the term `$bar` -- spelled with a
# sigil but not a variable. The term is matched before the variable parser
# sees the `$`, so `$bar` means the bound value (#9566, FixedInt).

constant term:<$bar> = 42;
is $bar, 42, 'the sigiled term reads its value';
is $bar + 1, 43, '... in an expression';

# A value with a user STORE is its own container: assigning to the term
# routes through STORE, in every assignment form.
class Box {
    has $!v handles <Numeric Str gist> = 0;
    method STORE($x) { $!v = $x }
    method plus($n) { $!v + $n }
}

constant term:<$box> = Box.new;
$box += 15;
is +$box, 15, 'statement compound assignment calls STORE';
$box = 3;
is +$box, 3, 'statement plain assignment calls STORE';
is ($box -= 1), 2, 'compound assignment in expression position';
is +$box, 2, '... stored through STORE';
$box .= plus(100);
is +$box, 102, '`.=` on the term calls STORE';

sub two(*@a) { @a.elems }
is two($box -= 2, 'x'), 2, 'compound assignment to the term is one call argument';
is +$box, 100, '... and it stored only its own right operand';
