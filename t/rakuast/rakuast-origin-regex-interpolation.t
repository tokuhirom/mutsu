use Test;

# A variable interpolated into a regex (`$w`, `@x`, `<$w>`) carries the ranges
# Rakudo gives it (ADR-10723 Stage 2, #15780): the interpolation node and its
# variable. Ranges measured on rakudo 2026.09.

plan 4;

# Every visited node of `$src` whose class name contains `Interpolat` or is
# a `Var::Lexical` under one, with its range.
sub vars(Str $src) {
    my @r;
    my $inside = False;
    sub walk($node) {
        my $name = $node.^name.subst('RakuAST::', '');
        my $was = $inside;
        $inside = True if $name.contains('Interpolat');
        @r.push: $name.subst('Regex::', '') ~ ' ' ~ ($node.origin andthen "{.from}..{.to}" orelse '-') if $inside;
        $node.visit-children(&walk);
        $inside = $was;
    }
    $src.AST.visit-children(&walk);
    @r.join(', ')
}

my $pre = 'my $w = "a"; my @x = <a b>; ';

is vars($pre ~ 'my $r = / $w /;'), 'Interpolation 38..40, Var::Lexical 38..40',
    'a scalar';
is vars($pre ~ 'my $r = / $w @x /;'),
    'Interpolation 38..40, Var::Lexical 38..40, Interpolation 41..43, Var::Lexical 41..43',
    'a scalar and an array';
is vars($pre ~ 'my $r = / <$w> /;'),
    'Assertion::InterpolatedVar 38..42, Var::Lexical 39..41',
    'a bracketed scalar: the variable is inside the brackets';
is vars($pre ~ 'my $r = / <@x> a /;'),
    'Assertion::InterpolatedVar 38..42, Var::Lexical 39..41',
    'a bracketed array';
