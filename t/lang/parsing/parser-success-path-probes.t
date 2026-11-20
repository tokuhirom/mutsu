use Test;

# The parser's success-path probes reject a non-matching start on its first
# bytes (whitespace, compound assignment, set operators, keyword terms). These
# pin the spellings each fast path must still let through.

plan 14;

# Blanks, comments and unspace after a token.
is (1#`(c) + 2), 3, 'an embedded comment right after a token';
is (1\ + 2), 3, 'unspace right after a token';
is EVAL("1\x0B+ 2"), 3, 'a vertical tab right after a token';

# Compound assignment, including the longest and the Unicode spellings.
my $a = Nil;
$a notandthen= 5;
is $a, 5, 'notandthen= is a compound assignment';
my $m = 3;
$m ×= 2;
is $m, 6, '×= is a compound assignment';
my $n = 7;
$n div= 2;
is $n, 3, 'div= is a compound assignment';

# Set operators: ASCII parenthesised and Unicode glyphs.
ok 1 (elem) (1, 2), '(elem)';
ok 1 ∈ (1, 2), '∈';
is (set(1, 2) ∪ set(3)).elems, 3, '∪';
ok 3 ∉ (1, 2), '∉';

# Keyword terms and their shadowing.
is pi.round(0.01), 3.14, 'pi is the setting term';
{
    my class Empty { method m { 'class' } }
    is Empty.m, 'class', 'a declared type shadows the Empty term';
}
{
    my enum Index <r w x e d>;
    is e.Int, 3, 'a declared enum value shadows e';
}

# The best error of a failed choice still names the alternatives.
throws-like { EVAL 'my $x = ;' }, X::Syntax, 'a missing term is a syntax error';
