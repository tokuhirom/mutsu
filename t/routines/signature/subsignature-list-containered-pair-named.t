use Test;

# Destructuring a List takes it apart as `List.Capture` does: a Pair the list
# holds in a variable's container (`('?', $p)`) is a named argument, like a
# literal one. mutsu counted it as a positional ("Too many positional
# arguments") or left a named slurpy empty. From CSS::Specification::Compiler's
# `multi sub compile(:occurs(@)! ($quant!, :$trailing, *%term))`, reached with
# `$spec.value.List`. Expected values checked against raku.

plan 6;

my $p = :keyw<thin>;
sub f(@a ($q, :$keyw)) { "q=$q k={$keyw // 'none'}" }
sub g(@a ($q, *%h)) { "q=$q h={%h.raku}" }

is f(('?', $p)), 'q=? k=thin', 'a List element holding a Pair binds a named parameter';
my $l = ('?', $p);
is f($l), 'q=? k=thin', '... through a variable holding the List';
is f(['?', $p]), 'q=? k=thin', 'an Array element too';
is f(('?', $(:keyw<y>))), 'q=? k=y', 'an itemized Pair literal too';
is g(('?', $p)), 'q=? h={:keyw("thin")}', 'a named slurpy collects it';

sub h(:occurs(@)! ($quant!, :$trailing, *%term)) { "$quant {%term.keys}" }
is h(:occurs(('?', $p).List)), '? keyw', 'a named parameter destructured the same way';
