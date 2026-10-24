use Test;

# An assignment to a non-variable lvalue keeps the shape it was written in
# (#14911): no spurious `Call::Term`, the parentheses of `($x) = 1` stay, and a
# ternary target gets no parentheses of its own. Measured on rakudo 2026.09.

plan 16;

sub expr(Str $source, Int $index) { $source.AST.statements[$index].expression.raku }

# --- read direction ------------------------------------------------------

my $neg = expr(Q[my $x; -$x = 5], 1);
nok $neg.contains('Call::Term'), 'a prefix target is not a call';
ok $neg.contains('left  => RakuAST::ApplyPrefix.new(') && $neg.contains('Assignment.new(:item)'),
    'the prefix is the left side of an item assignment';

my $regex = Q[/(a) { $<c> = $0 }/].AST.raku;
nok $regex.contains('Call::Term'), 'a named capture target in a regex block is not a call';
ok $regex.contains('Var::NamedCapture') && $regex.contains('Assignment.new(:item)'),
    'the regex block assigns to the named capture';

my $tern = expr(Q[my $y; my $x; $y ?? 1 !! $x = 2], 2);
nok $tern.contains('Parentheses'), 'an unparenthesized ternary target has no parentheses';
ok $tern.contains('left  => RakuAST::Ternary.new(') && $tern.contains('Assignment.new(:item)'),
    'the ternary is the left side of an item assignment';

my $call = expr(Q[my $f; $f() = 1], 1);
ok $call.contains('Call::Term'), 'a written call keeps its Call::Term';

for Q[my $x; ($x) = 1], Q[my $x; ($x) = 1, 2], Q[my $x; say(($x) = 1)] -> $src {
    my $r = $src.AST.raku;
    ok $r.contains('Circumfix::Parentheses') && !$r.contains(':item'),
        "the parentheses of a list assignment survive: $src";
}

my $array = expr(Q[my @a; (@a) = 1], 1);
ok $array.contains('Circumfix::Parentheses'), 'an array target keeps its parentheses';

is expr(Q[my $x; (($x)) = 1], 1).comb('Circumfix::Parentheses').elems, 2,
    'each written pair of parentheses is kept';

my $paren-tern = expr(Q[my $y; my $x; ($y ?? 1 !! $x) = 2], 2);
ok $paren-tern.contains('Circumfix::Parentheses'), 'a parenthesized ternary keeps its parentheses';

# --- write direction -----------------------------------------------------

{
    my $x;
    EVAL(Q[($x) = 1, 2].AST);
    is-deeply $x, (1, 2), 'the round-tripped ($x) = 1, 2 assigns the whole list';
}
{
    my $v = 5;
    try EVAL(Q[-$v = 3].AST);
    ok $!.defined, 'assigning to a prefix expression is an error, as for the parsed program';
}
{
    my ($a, $b) = 0, 0;
    EVAL(Q[($b ?? $a !! $b) = 7].AST);
    is $b, 7, 'the round-tripped ternary lvalue writes through the selected branch';
}
