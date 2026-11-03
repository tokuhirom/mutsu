use Test;

# The arguments of a regex call assertion `<name(ARGS)>` carry the ranges
# Rakudo gives them (ADR-10723 Stage 2, #15780): the `Name`, the `ArgList`,
# and every term inside the list. Ranges measured on rakudo 2026.09.

plan 5;

# Every visited node of `$src` below the first `Regex::Assertion::Named::Args`
# (its arguments), with its range.
sub args(Str $src) {
    my @r;
    my $inside = False;
    sub walk($node) {
        my $name = $node.^name.subst('RakuAST::', '');
        my $was = $inside;
        $inside = True if $name eq 'Regex::Assertion::Named::Args';
        @r.push: $name ~ ' ' ~ ($node.origin andthen "{.from}..{.to}" orelse '-') if $inside;
        $node.visit-children(&walk);
        $inside = $was;
    }
    $src.AST.visit-children(&walk);
    @r.join(', ')
}

my $pre = 'my $v = "x"; my $z = 1; ';

is args($pre ~ 'my $r = / <word($v.uc)> /;'),
    'Regex::Assertion::Named::Args 34..47, Name 35..39, ArgList 40..45, '
    ~ 'ApplyPostfix 40..45, Var::Lexical 40..42, Call::Method 43..45, Name 43..45, ArgList 45..45',
    'a method call as the argument';

is args($pre ~ 'my $r = / <.sep(1, 2)> /;'),
    'Regex::Assertion::Named::Args 34..46, Name 36..39, ArgList 40..44, '
    ~ 'IntLiteral 40..41, IntLiteral 43..44',
    'two literals, a non-capturing call';

is args($pre ~ 'my $r = / <y( $z )> /;'),
    'Regex::Assertion::Named::Args 34..43, Name 35..36, ArgList 38..40, Var::Lexical 38..40',
    'blanks inside the parentheses are not part of the list';

is args($pre ~ 'my $r = / <s("a)b", \'c\')> /;'),
    'Regex::Assertion::Named::Args 34..49, Name 35..36, ArgList 37..47, '
    ~ 'QuotedString 37..42, StrLiteral 38..41, QuotedString 44..47, StrLiteral 45..46',
    'quoted arguments, one holding a bracket';

my $value = 'x';
grammar G { token TOP { <word($value.uc)> }; token word($w) { $w } }
ok G.parse('X'), 'the grammar still compiles and runs the call';
