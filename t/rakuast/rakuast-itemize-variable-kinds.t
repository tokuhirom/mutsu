use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

plan 10;

sub expression($source) { $source.AST.statements[*-1].expression }

my $dynamic-hash = expression(Q[$%*ENV]);
isa-ok $dynamic-hash, RakuAST::Contextualizer::Item,
    'a dynamic hash has an item contextualizer';
isa-ok $dynamic-hash.target, RakuAST::Var::Dynamic,
    'the contextualizer holds a dynamic variable';
is $dynamic-hash.target.name, '%*ENV', 'the dynamic variable keeps its sigil and twigil';
is EVAL($dynamic-hash).^name, 'Hash', 'a dynamic hash item lowers through EVAL';

my @*a = 1, 2;
my $dynamic-array = expression(Q[$@*a]);
isa-ok $dynamic-array.target, RakuAST::Var::Dynamic,
    'a dynamic array also remains a variable target';
is EVAL($dynamic-array).^name, 'Array', 'a dynamic array item lowers through EVAL';

my $package-hash = expression(Q[$%VIP::h]);
isa-ok $package-hash.target, RakuAST::Var::Package,
    'a qualified hash stays a package variable';
is $package-hash.target.sigil, '%', 'its hash sigil is preserved';
package VIP { our %h = a => 1 }
is EVAL($package-hash).raku, '${:a(1)}', 'a package hash item lowers through EVAL';

is EVAL(Q[my %h = a => 1; $%h].AST).raku, '${:a(1)}',
    'lexical hash itemization still round-trips';
