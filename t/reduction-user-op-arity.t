use Test;

plan 16;

sub infix:<myop>($a, $b) { $a + $b }
sub one($a) { $a * 10 }
sub two($a, $b) { $a ~ $b }
my &opb = &[+];

# A user-supplied operator has no identity element, so a one-element fold is
# not short-circuited: the routine is called and its binder decides.
dies-ok { [myop] 5 }, 'one-element fold with a user infix calls it (and dies on arity)';
dies-ok { [[&two]] 5 }, 'one-element fold with a 2-ary callable dies on arity';
is (try { [[&one]] 5 }), 50, 'one-element fold with a 1-ary callable calls it';

# The zero-element fold is the same rule with no arguments at all.
dies-ok { [myop] }, 'zero-element fold with a user infix dies on arity';
dies-ok { [[&one]] }, 'zero-element fold with a 1-ary callable dies on arity';

# The scan form does NOT call the operator for one element.
is-deeply (try { [\myop] 5 }).List, (5,), 'one-element scan with a user infix yields the element';
is-deeply (try { [\[&one]] 5 }).List, (5,), 'one-element scan with a callable yields the element';

# Multi-element folds are unchanged.
is ([myop] 5, 6), 11, 'two-element fold with a user infix';
is ([[&two]] 'a', 'b'), 'ab', 'two-element fold with a callable';

# A callable that merely NAMES a builtin operator keeps that operator's
# identity and one-element answer.
is ([[&opb]]), 0, 'zero-element fold with &[+] is the + identity';
is ([[&opb]] 5), 5, 'one-element fold with &[+] is the element';
is ([[&opb]] 1, 2, 3), 6, 'multi-element fold with &[+]';

# Identity-bearing builtins must not move.
is ([+] 5), 5, '[+] 5';
is ([+]), 0, '[+] identity';
is ([*]), 1, '[*] identity';
is ([~] 5), '5', '[~] 5 stringifies';
