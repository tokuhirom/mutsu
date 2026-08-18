use Test;

plan 8;

# A `with`/`without` statement modifier desugars to `given TOPIC { if
# $_.defined { STMT } }` (see parser::stmt::modifier), where the synthetic
# `Given`/`If` are both tagged `is_statement_modifier`. That tag was set
# correctly at parse time but three separate compiler call sites (the
# statement-position If's compile_if_value, the DoStmt-If's
# compile_do_if_expr_bound, and the DoStmt-Given's own placeholder-binds-topic
# check) never consulted it, so a placeholder in the modified statement
# (`$^a`) was wrongly bound to the synthetic condition/topic value instead of
# staying the enclosing routine's own placeholder argument. Only one sibling
# (the statement-position Given arm) already had the guard.
# See news/2026-08/with-statement-modifier-hides-placeholders.md.

sub w1 { "a=$^a topic=$_" with $^n }
is w1(3, 4), 'a=3 topic=4', '`with` statement modifier does not shadow $^a with its topic';

sub w2 { "a=$^a topic=$_" without $^n }
is w2(3, Nil), 'a=3 topic=', '`without` statement modifier does not shadow $^a with its topic';

# The plain `if`/`unless` statement-modifier siblings (no given/topic
# involved) must keep working too.
sub f { $^a if $^n }
is f(3, 4), 3, '`if` statement modifier does not shadow $^a with its condition';

sub g { $^a unless $^n }
is g(3, 0), 3, '`unless` statement modifier does not shadow $^a with its condition';

# Regression guard: a REAL if/given BLOCK (not a statement modifier) still
# binds its own placeholder to the condition/topic value, exactly as before.
is (if 9 { $^a + 1 }), 10, 'a real `if` BLOCK still binds $^a to the condition value';
is (given 5 { $^a + 1 }), 6, 'a real `given` BLOCK still binds $^a to the topic value';

# Regression guard: the `do`-expression forms of the same shapes.
is do if 9 { $^a + 1 }, 10, '`do if` block still binds $^a to the condition value';
is (do given 5 { $^a + 1 }), 6, '`do given` block still binds $^a to the topic value';
