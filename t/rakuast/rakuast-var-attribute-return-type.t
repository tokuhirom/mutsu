use Test;

# `Var::Attribute.return-type` is the declared type of the attribute it names,
# as static::Analyzer reads it (#15610). Expected values are rakudo 2026.09's.

plan 4;

class K { has Int $.at; has Str:D $.s = "x"; has $.u; method m { $!at }; method n { $!s }; method o { $!u } }

my %seen;
sub walk($node) {
    return unless $node ~~ RakuAST::Node;
    %seen{$node.name} = $node.return-type.^name if $node ~~ RakuAST::Var::Attribute;
    $node.visit-children({ walk($_) });
}
CHECK { walk($*CU) }

is %seen<$!at>, 'Int', 'a typed attribute answers its type';
is %seen<$!s>, 'Str:D', 'a definite type keeps its smiley';
is %seen<$!u>, 'Mu', 'an untyped attribute is Mu';
ok RakuAST::Var::Attribute.^can('return-type'), 'the class can return-type';
