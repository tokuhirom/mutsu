use v6;
use Test;

# An operator a module exports with a declared precedence carries its
# `Routine.prec` record into the importer: an operator the importer relates to
# it (`is equiv(&infix:<imported>)`) takes that record, not the built-in level
# it was derived from (ADR-17558 slice 3). `is equiv` takes the level but not
# the associativity, as `is tighter` / `is looser` do. Checked against rakudo.

plan 5;

use lib 't/lib';
use ImportedOpPrec;

sub infix:<⊕>($a, $b) is equiv(&infix:<±>) { "($a⊕$b)" }

is &infix:<±>.prec<prec>, 'u@=', 'the imported operator keeps its declared level';
is &infix:<⊕>.prec<prec>, 'u@=', 'is equiv of an imported operator takes its level';
is &infix:<⊕>.prec<assoc>, 'left', 'but not its associativity';
is (1 ± 2 ⊕ 3), '((1±2)⊕3)', 'the two parse at one level';

sub infix:<⊞>($a, $b) is equiv(&infix:<**>) { "($a⊞$b)" }
is &infix:<⊞>.prec<assoc>, 'left', 'is equiv of a built-in takes no associativity either';
