use Test;

# The statement parser used to return early in front of a loose operator on
# the same line, and the statement list took the leftover as further
# statements: the operator became a bare-word statement and its right-hand
# side ran on its own (#10257). Each of these is one statement.

plan 26;

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

# --- adjacent colonpairs ------------------------------------------------------

{
    my @a = :a:!b:42c;
    is-deeply @a, [:a], 'later colonpairs on a colonpair term are adverbs (rakudo drops them)';
    my $ran = False;
    my $x = :a :b($ran = True);
    ok $ran, 'the dropped adverb values are still evaluated';
    is (:a:!b:42c).elems, 3, 'a parenthesized run is a list';
    is [:a :b].elems, 2, 'an array composer run is a list';
    sub named(*%h) { %h.elems }
    is named(:a :b), 2, 'a parenthesized argument list keeps every pair';
}

# --- `$@`, `%::{...}` ---------------------------------------------------------

lives-ok { EVAL '$@' }, '$@ alone is one term (roast S32-exceptions/misc2)';
throws-like q[%::{''}], X::Undeclared, 'a sigil with a bare :: is an undeclared variable';

# --- a routine declaration with a statement modifier ---------------------------

{
    my $topic;
    sub declared-here() { 5 } given ($topic = 3);
    is declared-here(), 5, 'sub ... given: the sub is declared';
    is $topic, 3, 'the modifier still evaluates its topic';
}

# --- a call ending in a block ends at the newline ----------------------------

{
    my @seen;
    sub take-block(*@) { @seen.push: 'call' }
    take-block 'x' => { 1 }
    if False {
        @seen.push: 'then';
    }
    else {
        @seen.push: 'else';
    }
    is-deeply @seen, ['call', 'else'], 'the next line if/else is its own statement';
}

# --- `only method`, compound assignment ending in a block -------------------

{
    my class OnlyM { only method m() { 7 } }
    is OnlyM.m, 7, 'only method declares a method';

    my $x;
    my @seen;
    $x //= do if True {
        5
    }
    if $x == 5 {
        @seen.push: 'if';
    }
    is $x, 5, '//= do if ... assigns';
    is-deeply @seen, ['if'], 'the next line if is its own statement';
}

# --- errors keep their own class -----------------------------------------------

throws-like 'for 1, 2 { my $p = {};', X::Syntax::Missing, 'an unclosed for block is a missing block';

# --- a leftover term on the same line is "Two terms in a row" ----------------

# rakudo reports the word as an undeclared routine; either way it is a
# compile error, not a bare-word statement.
throws-like 'my $x = 5; $x .=new: andthen say "then"', X::Comp,
    'a word infix in argument position does not become its own statement';
throws-like 'my $x = 1 2', X::Syntax::Confused, 'two literal terms in a row',
    reason => 'Two terms in a row';
throws-like 'say 1 "a"', X::Syntax::Confused, 'a string after a listop argument',
    reason => 'Two terms in a row';
