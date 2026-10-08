use Test;

# `**` as a term crosses the RakuAST boundary both ways: `.AST` renders
# `RakuAST::Term::HyperWhatever` (measured on rakudo 2026.09) and EVAL of that
# node lowers back to the HyperWhatever value.

plan 6;

my $ast = Q[my $h = **; $h].AST;
ok $ast.gist.contains('RakuAST::Term::HyperWhatever.new'), 'a `**` term is a HyperWhatever node';

my $term = RakuAST::Term::HyperWhatever.new;
isa-ok $term, RakuAST::Term::HyperWhatever, 'it can be built by hand';
isa-ok EVAL($term), HyperWhatever, 'and evaluates to the HyperWhatever value';
ok EVAL($term) ~~ HyperWhatever:D, 'a defined one, not the type object';

is EVAL(Q[(**).WHAT.gist].AST), '(HyperWhatever)', 'the round trip keeps the value';
is EVAL(Q[my @a = 1..5; @a[**].elems].AST), 5, '`**` as a subscript';

done-testing;
