use Test;

# `enum` and `subset` declarations carry the origins the parser read them
# from (ADR-10723 Stage 2): the name, and a subset's `of` and `is export`
# traits. Ranges measured on rakudo 2026.09.

plan 13;

sub range($node) { with $node.origin { "{.from}..{.to}" } else { 'none' } }
sub first-child($node) { my @c; $node.visit-children(-> $c { @c.push: $c }); @c[0] }
sub trait-of($decl, $class) { $decl.traits.first(*.^name eq $class) }

my $src = q:to/END/;
my enum E <a b>;
enum Color (red => 1, green => 2);
subset S of Int where * > 0;
subset Pos where * > 0;
subset T of Str:D is export;
END
my @d = $src.AST.statements.map(*.expression);

is range(@d[0]), '0..15', 'a my-scoped enum';
is range(@d[0].name), '8..9', 'its name';
is range(@d[1].name), '22..27', 'the name of an enum over pairs';

is range(@d[2]), '52..79', 'a subset';
is range(@d[2].name), '59..60', 'its name';
my $of = trait-of(@d[2], 'RakuAST::Trait::Of');
is range($of), '61..67', 'its of trait';
is range(first-child($of)), '64..67', 'the type it names';

is range(@d[3].name), '88..91', 'the name of a subset with no of';

is range(@d[4].name), '112..113', 'the name of an exported subset';
is range(trait-of(@d[4], 'RakuAST::Trait::Of')), '114..122', 'an of trait with a smiley';
my $export = trait-of(@d[4], 'RakuAST::Trait::Is');
is range($export), '123..132', 'its is export trait';
is range(first-child($export)), '126..132', 'the trait name';

is EVAL(q[subset Small of Int where * < 10; enum Dir <up down>; (5 ~~ Small) && down.value].AST), 1,
    'the round trip still runs';
