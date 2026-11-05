use Test;

# The subscript of an interpolated variable in a string (`"@r[0]"`,
# `"%h{$k}"`) carries the ranges Rakudo gives it (ADR-10723 Stage 2, #15780):
# the `SemiList`, its statement and the terms inside. Ranges measured on
# rakudo 2026.09.

plan 3;

# Every visited node of `$src` below the first `Postcircumfix::ArrayIndex` or
# `HashIndex`, with its range.
sub index(Str $src) {
    my @r;
    my $inside = False;
    sub walk($node) {
        my $name = $node.^name.subst('RakuAST::', '');
        my $was = $inside;
        $inside = True if $name eq 'Postcircumfix::ArrayIndex' | 'Postcircumfix::HashIndex';
        @r.push: $name ~ ' ' ~ ($node.origin andthen "{.from}..{.to}" orelse '-') if $inside;
        $node.visit-children(&walk);
        $inside = $was;
    }
    $src.AST.visit-children(&walk);
    @r.join(', ')
}

my $pre = 'my @r = 1, 2; my %h = a => 1; my $k = "a"; ';

is index($pre ~ 'say "v: @r[0] x";'),
    'Postcircumfix::ArrayIndex 53..56, SemiList 54..55, Statement::Expression 54..55, IntLiteral 54..55',
    'a literal index';
is index($pre ~ 'say "@r[1, 0]";'),
    'Postcircumfix::ArrayIndex 50..56, SemiList 51..55, Statement::Expression 51..55, '
    ~ 'ApplyListInfix 51..55, Infix 52..53, IntLiteral 51..52, IntLiteral 54..55',
    'two indices';
is index($pre ~ 'say "%h{$k} @r[0]";'),
    'Postcircumfix::HashIndex 50..54, SemiList 51..53, Statement::Expression 51..53, Var::Lexical 51..53, '
    ~ 'Postcircumfix::ArrayIndex 57..60, SemiList 58..59, Statement::Expression 58..59, IntLiteral 58..59',
    'a variable key; later segments keep their ranges';
