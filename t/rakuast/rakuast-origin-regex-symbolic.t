use Test;

# A symbolic subrule call `<::("name")>` has the ranges Rakudo gives it
# (ADR-10723 Stage 2, #15780): the `Assertion::Named`, its `WithWhitespace`,
# its `Name` from the `::` to the `)`, and the string that computes the
# name. Its `Name` visits that string. Ranges measured on rakudo 2026.09.

plan 3;

sub ranges(Str $src, *@classes) {
    my @r;
    sub walk($node) {
        my $name = $node.^name.subst('RakuAST::', '');
        if $name eq @classes.any {
            with $node.origin { @r.push: "$name {.from}..{.to}" }
        }
        $node.visit-children(&walk);
    }
    $src.AST.visit-children(&walk);
    @r.sort.join(', ')
}

is ranges('my $x = / <::("w")> /;', 'Regex::Assertion::Named', 'Regex::WithWhitespace', 'Name'),
    'Name 11..18, Regex::Assertion::Named 10..19, Regex::WithWhitespace 10..20',
    'a capturing symbolic call';
is ranges('my $y = / <.::(' ~ "'digit'" ~ ')> /;', 'QuotedString', 'StrLiteral'),
    'QuotedString 15..22, StrLiteral 16..21',
    'a dotted call with a single-quoted string';
is ranges('my $z = / <::("w")> <foo> /;', 'Regex::Assertion::Named'),
    'Regex::Assertion::Named 10..19, Regex::Assertion::Named 20..25',
    'a symbolic call in a sequence';
