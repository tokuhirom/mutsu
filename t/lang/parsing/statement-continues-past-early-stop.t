use Test;

# The statement parser used to return early in front of a loose operator on
# the same line, and the statement list took the leftover as further
# statements: the operator became a bare-word statement and its right-hand
# side ran on its own (#10257). Each of these is one statement.

plan 12;

# --- `{ ... }()` is an infix operand ------------------------------------------

sub f { { 42 }() orelse say "orelse" }
is f(), 42, '{ ... }() orelse ... is one expression';
is (EVAL '{ 0 }() || 7'), 7, '{ ... }() || ... is one expression';
is (EVAL '{ Nil }.() // 5'), 5, '{ ... }.() // ... is one expression';

# --- a regex smartmatch RHS feeds a following comparison ----------------------

is ("a" ~~ /a/ eq "x"), False, 'X ~~ /re/ eq Z is (X ~~ /re/) eq Z';
is ("a" ~~ /a/ eq "a"), True, 'the match result is the left operand of eq';
is ("ab" ~~ /b/ eq "b" eq "b"), True, 'the comparison chain continues after the match';

# --- `temp` over a declaration --------------------------------------------------

our $out = "orig";
sub t1 {
    temp our $out = "t";
    $out ~= "x";
    $out
}
is t1(), "tx", 'temp our $x = ... declares, assigns and temporizes';
is $out, "t", 'the value temp saved is the initializer (as in rakudo)';

sub t2 { temp my $x = 3; $x }
is t2(), 3, 'temp my $x = ... works';

# --- a leftover term on the same line is "Two terms in a row" ----------------

# rakudo reports the word as an undeclared routine; either way it is a
# compile error, not a bare-word statement.
throws-like 'my $x = 5; $x .=new: andthen say "then"', X::Comp,
    'a word infix in argument position does not become its own statement';
throws-like 'my $x = 1 2', X::Syntax::Confused, 'two literal terms in a row',
    reason => 'Two terms in a row';
throws-like 'say 1 "a"', X::Syntax::Confused, 'a string after a listop argument',
    reason => 'Two terms in a row';
