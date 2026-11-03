use Test;

# The arguments of a statement-level call carry their ranges even when two of
# them have the same shape (`is(2, 2, "x")`, `foo 1, 1`) (ADR-10723 Stage 2,
# #15780). Ranges measured on rakudo 2026.09.

plan 3;

# Every node below the first `Call::Name` of `$src`, with its range.
sub call(Str $src) {
    my @r;
    my $inside = False;
    sub walk($node) {
        my $name = $node.^name.subst('RakuAST::', '');
        my $was = $inside;
        $inside = True if $name.starts-with('Call::Name');
        @r.push: $name ~ ' ' ~ ($node.origin andthen "{.from}..{.to}" orelse '-') if $inside;
        $node.visit-children(&walk);
        $inside = $was;
    }
    $src.AST.visit-children(&walk);
    @r.join(', ')
}

is call('sub foo(*@a) {}; foo(2, 2, "x");'),
    'Call::Name 17..31, Name 17..20, ArgList 21..30, IntLiteral 21..22, IntLiteral 24..25, '
    ~ 'QuotedString 27..30, StrLiteral 28..29',
    'equal literals in a call';
is call('sub foo(*@a) {}; foo 2, 2;'),
    'Call::Name::WithoutParentheses 17..25, Name 17..20, ArgList 21..25, '
    ~ 'IntLiteral 21..22, IntLiteral 24..25',
    'equal literals in a listop call';
is call('sub foo(*@a) {}; foo(:a, :a);'),
    'Call::Name 17..28, Name 17..20, ArgList 21..27, ColonPair::True 21..23, ColonPair::True 25..27',
    'equal pairs';
