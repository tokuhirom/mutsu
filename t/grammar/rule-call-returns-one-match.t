use Test;

# From the CSS::Grammar distribution (#16372): a call of a `rule` or `token`
# returns ONE match to its caller. When the caller (a `regex`, which does not
# ratchet) fails afterwards it must not re-enter the callee, so the callee's
# action runs once per alternative of the caller, not once per alternative of
# the callee.

plan 6;

my @log;
class Act { method e($/) { @log.push(~$/) } }

grammar WsFirst {
    regex TOP { <e> 'x' || <e> 'z' }
    rule e { <.ws>? <d>+ }
    token d { \d+ }
}
grammar OptionalFirst {
    regex TOP { <e> 'x' || <e> 'z' }
    rule e { <d>? <d>+ }
    token d { \d+ }
}
grammar Literal {
    regex TOP { <e> 'x' || <e> 'z' }
    rule e { 'a'? <d>+ }
    token d { \d+ }
}
for (WsFirst, OptionalFirst, Literal) -> $g {
    @log = ();
    lives-ok { $g.parse('1 2 3 z', :actions(Act.new)) }, "{$g.^name} runs";
    is @log.elems, 2, "{$g.^name}: the rule's action ran once per alternative of TOP";
}

done-testing;
