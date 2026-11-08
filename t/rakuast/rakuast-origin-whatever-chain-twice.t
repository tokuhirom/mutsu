use Test;
use experimental :rakuast;

plan 3;

# A whatever-curried chain written twice in one statement keeps the origins of
# both occurrences: the marker and the body of the first must not use up the
# range of the second.
sub origins(Str $code) {
    my @l;
    sub w($n) { @l.push($n.^name.split('::').tail ~ '@' ~ .from ~ '-' ~ .to) with $n.origin; $n.visit-children(&w) }
    w($code.AST);
    @l
}

my @twice = origins(Q[sub f($a,$b){}; my $x; f $x.map(*.head), $x.map(*.head)]);
is @twice.grep(/^ApplyPostfix\@48'-'54/).elems, 1, 'the second *.head chain has its ApplyPostfix origin';
is @twice.grep(/^ArgList\@48'-'54/).elems, 1, 'and its ArgList origin';
is-deeply @twice.List, ("StatementList@0-55", "Expression@0-14", "Sub@0-14", "Name@4-5", "Signature@6-11", "Parameter@6-8", "Var@6-8", "Simple@6-8", "Parameter@9-11", "Var@9-11", "Simple@9-11", "Blockoid@12-14", "StatementList@13-13", "Expression@16-21", "Simple@16-21", "Expression@23-55", "WithoutParentheses@23-55", "Name@23-24", "ArgList@25-55", "ApplyPostfix@25-39", "Lexical@25-27", "Method@28-39", "Name@28-31", "ArgList@32-38", "ApplyPostfix@32-38", "Method@34-38", "Name@34-38", "ArgList@38-38", "ApplyPostfix@41-55", "Lexical@41-43", "Method@44-55", "Name@44-47", "ArgList@48-54", "ApplyPostfix@48-54", "Method@50-54", "Name@50-54", "ArgList@54-54",), 'the whole walk matches rakudo';
