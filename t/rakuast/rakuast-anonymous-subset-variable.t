use Test;

# At CHECK time static::Analyzer reads `my Int $x where ...` as a variable of an
# anonymous subset, and a named `subset` as a type it can look into (#15610).
# Expected values are rakudo 2026.09's.

plan 9;

subset Positive of Int where * > 0;
my Int $x where * > 1 = 3;
my Positive $p = 2;

my (@kinds, @subsets, %decls);
sub walk($node) {
    return unless $node ~~ RakuAST::Node;
    given $node {
        when RakuAST::VarDeclaration::Simple {
            %decls{$node.name} = $node;
        }
        when RakuAST::Type::Subset {
            @subsets.push: $node.compile-time-value;
        }
    }
    $node.visit-children({ walk($_) });
}
CHECK { walk($*CU) }

my $anon = %decls<$x>;
isa-ok $anon.type, RakuAST::Type::Subset, 'a variable with where is typed by a Type::Subset';
ok $anon.where.defined, 'its where is still reachable from the declaration';
is $anon.return-type.^name, '<anon>', 'its type is the anonymous subset';
is $anon.return-type.HOW.^name, 'Perl6::Metamodel::SubsetHOW', '... a real subset';
is $anon.return-type.^refinee.^name, 'Int', '... refining the written type';
is %decls<$p>.return-type.^name, 'Positive', 'a variable of a named subset has that type';
is %decls<$p>.return-type.HOW.^name, 'Perl6::Metamodel::SubsetHOW', '... declared at CHECK time';
ok @subsets.elems >= 2, 'both subsets were reached';
ok RakuAST::VarDeclaration::Simple.^can('return-type'), 'the declaration can return-type';
