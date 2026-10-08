use v6;
use Test;

# Routine.precedence / Routine.associative (Understitch t/07-properties.t).

plan 11;

use lib 't/lib';
use ExportHookReduceOp;

sub infix:<foo>($a, $b) is equiv(&infix:<~>) is assoc('left') { 1 }
sub infix:<bar>($a, $b) is assoc('right') { 1 }
sub postfix:<zz>($a) { 1 }
sub plain($a) { 1 }

is &infix:<foo>.precedence, &infix:<~>.precedence, 'is equiv copies precedence';
is &infix:<foo>.associative, 'left', 'declared associativity';
is &infix:<bar>.associative, 'right', 'is assoc(right)';
is &infix:<+>.precedence, 't=', 'built-in precedence';
is &infix:<**>.associative, 'right', 'built-in associativity';
is &postfix:<zz>.associative, 'unary', 'postfix default';
is &plain.precedence, '', 'non-operator: empty precedence';
is &plain.associative, '', 'non-operator: empty associativity';
is &infix:<foo>.precedence.WHAT.^name, 'Str', 'precedence is a Str';

# An operator exported through `sub EXPORT` outlives its registry entry
# (Understitch).
is &infix:<__>.precedence, &infix:<~>.precedence, 'exported operator keeps is equiv precedence';
is &infix:<__>.associative, 'left', 'exported operator keeps associativity';
