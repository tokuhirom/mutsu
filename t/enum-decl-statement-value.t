use v6;
use MONKEY-SEE-NO-EVAL;
use Test;

# An `enum` declaration used as a STATEMENT must not leave its own value where
# the enclosing block's result is read from:
#
#     EVAL 'enum E <A B>; my $r = 41 + 1; $r'   # 42, not the enum's Map
#
# `RegisterEnum` pushes the declaration's `Map` because that IS the value of
# `enum` in expression position (`my $e = enum Foo <a b c>`). Nothing popped it
# in statement position, so it parked at the frame's stack base and won over
# the block's real tail value. It is now declared in
# `stmt_nets_a_stack_value` — the same ADR-0052 mechanism that pops a non-final
# `given`/`when`/`default` — so a non-final `enum` is popped and a FINAL one is
# still the block's value. Every expectation was measured against rakudo
# 2026.07.

plan 16;

# --- the ticket's repro ------------------------------------------------
is EVAL(q|enum E1 <A B>; my $r = 41 + 1; $r|), 42,
    'EVAL of a string starting with an enum returns the last statement';
is EVAL(q|enum E2 <C D>; 0|), 0,
    'a falsy tail value survives a leading enum declaration';

# Not only the FIRST statement: a mid-block enum is the same defect.
is EVAL(q|my $x = 1; enum E3 <F G>; 7|), 7,
    'an enum in the middle of a block does not win over the tail';

# --- a FINAL enum IS the block's value ---------------------------------
is EVAL(q|enum E4 <H I>|).^name, 'Map',
    'an enum as the only statement is the unit value';
is EVAL(q|enum E5 <J K>; |).^name, 'Map',
    'a trailing semicolon does not change that';

# --- the expression forms must be untouched ----------------------------
my $e = enum Ea <a b c>;
is $e.^name, 'Map', 'a named enum in expression position yields its Map';
is $e<a>, 0, 'the Map carries the variant values';
is a.Int, 0, 'the variants are installed';
is (do enum Eb <d f>).^name, 'Map', '`do enum` yields the Map';

# --- routine and block bodies -----------------------------------------
is (do { enum Ec <g h>; 9 }), 9, 'a non-final enum in a do-block is popped';
sub with-enum-then-value() { enum Ed <i j>; 3 }
is with-enum-then-value(), 3, 'a non-final enum in a sub body is popped';
sub enum-is-the-tail() { enum Ee <k l> }
is enum-is-the-tail().^name, 'Map', 'a final enum is the sub result';

my $x = do { enum Ef <m n>; 11 };
is $x, 11, 'assignment from a block whose non-final statement is an enum';

# --- `my enum`, and a following declaration ---------------------------
is EVAL(q|my enum E6 <o p>; 8|), 8, 'a `my enum` is popped too';
my $src = 'enum E7 <q r>; class EC { }; 12';
is EVAL($src), 12, 'an enum followed by a class declaration';

# The statement after a bare-block enum still runs.
my $ran = 0;
{ enum Eg <s t>; }
$ran = 1;
is $ran, 1, 'a bare block ending in an enum leaves the outer flow intact';
