use Test;

# Distilled from CSS::Specification (`token usage($*USAGE)` whose action reads
# `$*USAGE`): a rule's dynamically-scoped parameter must still be visible when
# the rule's action runs, after the rule itself has finished matching.
plan 2;

grammar G {
    token TOP { <val('x', 'hello')> }
    token val($*EXPR, $*USAGE = '') { <proforma> || <usage($*USAGE)> }
    token proforma { 'zzz' }
    token usage($*USAGE) { <[a..z]>+ }
}

class A {
    method usage($/) { make ~$*USAGE }
    method val($/)   { make $<usage>.ast }
    method TOP($/)   { make $<val>.ast }
}

is G.parse('hello', :actions(A)).made, 'hello', 'action sees the rule parameter';

grammar H {
    token TOP { <usage('abc')> }
    token usage($*USAGE) { <[a..z]>+ }
}
class B { method usage($/) { make ~$*USAGE } method TOP($/) { make $<usage>.ast } }
is H.parse('abc', :actions(B)).made, 'abc', 'also for a direct subrule call';
