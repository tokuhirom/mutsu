use Test;

# An uninitialized declaration lowered from RakuAST gets the same default the
# parser gives it: `my @a` is an empty Array and `my %h` an empty Hash, not a
# container holding `Nil` (#9568; the L10N::XX tests EVAL through RakuAST).

plan 5;

is-deeply Q[my @a; @a].AST.EVAL, [], 'my @a is empty';
is-deeply Q[my @a; @a.push(1); @a].AST.EVAL, [1], 'a push onto it is its only element';
is-deeply Q[my %h; %h].AST.EVAL, {}, 'my %h is empty';
is Q[my %h; %h.push((k => 1)); %h.elems].AST.EVAL, 1, 'a pair pushed into it is its only key';
nok Q[my $x; $x].AST.EVAL.defined, 'my $x is still undefined';
