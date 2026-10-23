use Test;

# The traits of a package header (`class A is B does R is rw`) and the type
# and `is` traits of a variable declaration (`my Int $x is default(3)`) carry
# the ranges Rakudo gives them (ADR-10723 Stage 2), and a term's copy never
# takes the range of a term written in another statement. Ranges measured on
# rakudo 2026.09.

plan 8;

# Each visited node of `$src` whose class matches `$classes`, with its range.
sub nodes(Str $src, $classes) {
    my @r;
    sub walk($node) {
        my $name = $node.^name.subst('RakuAST::', '');
        if $name ~~ $classes {
            @r.push: $name ~ ' ' ~ ($node.origin andthen "{.from}..{.to}" orelse '-');
        }
        $node.visit-children(&walk);
    }
    $src.AST.visit-children(&walk);
    @r.join(', ')
}

my $header = "role R \{ }\nclass B \{ }\nclass A is B does R is rw \{ }\n";
is nodes($header, /^Trait/), 'Trait::Is 31..35, Trait::Does 36..42, Trait::Is 43..48',
    'the traits of a class header';
is nodes($header.subst('B', 'H', :g).subst('R', 'Q', :g).subst('A', 'I'), /^ 'Type::Simple'/),
    'Type::Simple 34..35, Type::Simple 41..42',
    'and the types they name';
is nodes("role S \{ }\nclass C does S is export \{ }", /^Trait|^Name/).split(', ')[*-5..*].join(', '),
    'Name 17..18, Trait::Does 19..25, Name 24..25, Trait::Is 26..35, Name 29..35',
    'an exported class keeps its name and traits';
is nodes("class D \{ }\nclass F hides D \{ }\ngrammar G is Grammar \{ }", /^Trait/),
    'Trait::Hides 20..27, Trait::Is 42..52',
    'hides, and a grammar';

is nodes('my Int $z is default(3) = 4;', /Type|Trait|Initializer|Circumfix|^Name/),
    'Type::Simple 3..6, Name 3..6, Initializer::Assign 24..27, Trait::Is 10..23, Name 13..20, Circumfix::Parentheses 20..23',
    'a typed declaration with a trait, visited after its initializer';
is nodes('my Str:D $s = "";', /Type/),
    'Type::Definedness 3..8, Type::Simple 3..8',
    'a type with a smiley';
is nodes('class J { has $.a is rw is required; }', /^Trait/),
    'Trait::Is 18..23, Trait::Is 24..35',
    'the traits of an attribute';

is nodes("class E \{ has \$.l = 7; }\nmy \$x = 7;", /IntLiteral/).split(', ').grep(!*.ends-with('-')).unique.join(', '),
    'IntLiteral 20..21, IntLiteral 33..34',
    "an attribute default does not take a later statement's range";
