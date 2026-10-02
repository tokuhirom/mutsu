use Test;

plan 22;

# A block's `}` followed by a newline ends the statement (rakudo's
# `$*ENDSTMT`), so a next-line `if`/`unless`/`for` starts a new statement
# instead of becoming a statement modifier. Every blockoid counts: bare and
# pointy blocks, hash composers, `do`/`try`/`gather` blocks, routine bodies and
# regex declarations — wherever it sits in the expression. mutsu used to
# re-derive this from the AST shape and missed several of them, dying with
# "Two terms in a row" on the next line's `{`.

{
    my $h = {a => 1}
    if False { flunk 'then-branch of a False if ran' }
    is-deeply $h, {a => 1}, 'hash composer at end of line ends the statement';
}

{
    my $h = {}
    if False { flunk 'then-branch of a False if ran' }
    is-deeply $h, {}, 'empty hash composer at end of line ends the statement';
}

{
    my @a = 1, -> { 2 }
    if False { flunk 'then-branch of a False if ran' }
    is @a.elems, 2, 'pointy block as the last list item ends the statement';
}

{
    my $x = 1 + do { 2 }
    if False { flunk 'then-branch of a False if ran' }
    is $x, 3, '`do` block as an infix operand ends the statement';
}

{
    my $x = { 1 } # trailing comment
    if False { flunk 'then-branch of a False if ran' }
    isa-ok $x, Block, 'a comment after the brace still ends the line';
}

{
    my $seen = '';
    my $x = sub { 1 }
    unless False { $seen = 'unless ran' }
    is $seen, 'unless ran', 'next-line `unless` after a sub body is a statement';
    is $x(), 1, '... and the declaration kept its value';
}

{
    my $x = 1 ?? 2 !! { 3 }
    if False { flunk 'then-branch of a False if ran' }
    is $x, 2, 'block as a ternary else-branch ends the statement';
}

{
    my $p = a => { 1 }
    if False { flunk 'then-branch of a False if ran' }
    is $p.key, 'a', 'block as a pair value ends the statement';
}

{
    my $x = gather { take 1 }
    if False { flunk 'then-branch of a False if ran' }
    is-deeply $x.List, (1,), '`gather` block ends the statement';
}

{
    my regex digitish { \d }
    my $m = '';
    if '5' ~~ /<digitish>/ { $m = 'matched' }
    is $m, 'matched', 'a regex declaration body ends the statement';
}

# Negative cases: the `}` does not close a block at end of statement, so the
# next-line keyword IS a statement modifier.
{
    my %h = a => 1;
    my $k = 'a';
    my $x = %h{$k}
    if False;
    nok $x.defined, 'a hash subscript `}` does not end the statement';
}

{
    my $x = ({ 1 }
    ) if False;
    nok $x.defined, 'a block closed inside parentheses does not end the statement';
}

{
    my @a = (1, 2, 3).grep({ $_ > 1
    })
    if False;
    is @a.elems, 0, 'a block argument inside a call\'s parentheses does not end it';
}

{
    my @a = 1, -> { 2 }  if False;
    is @a.elems, 0, 'a modifier on the same line as the brace still applies';
}

# The `for` parser reports a loop block gobbled by its iterable whenever a
# block term was parsed in it — but not a routine body or a `do` block.
my &gobbled = sub ($code) {
    throws-like $code, X::Comp::Group,
        sorrows => sub (@s) { @s[0] ~~ X::Syntax::BlockGobbled },
        "`$code` reports the gobbled block";
};
gobbled 'for 1, 2, {a => 1}';
gobbled 'for 1.. { }';
gobbled 'for (1, {2})';
gobbled 'for 1, { 2 }, 3';
gobbled "for 1, 2, \{ 3 } # comment";
throws-like 'for 1, sub { 3 }', X::Syntax::Missing, what => /^block/;
throws-like 'for 1, 2, do { 3 }', X::Syntax::Missing, what => /^block/;
