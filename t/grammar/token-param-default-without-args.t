use Test;

plan 4;

# A rule called without arguments binds its defaulted parameters.
grammar G {
    token TOP { <s> }
    token ind(Int $n) { 'x' ** { $n } }
    token s(Int $indent = 0) { <ind($indent + 1)> }
}
ok G.parse("x"), 'default bound for an argument-less <s>';
nok G.parse("xx"), 'the default drives the quantifier';

grammar K {
    token TOP { <s> }
    token s(Int $indent = 1) { 'x' ** { $indent + 1 } }
}
ok K.parse("xx"), 'default used in a code-block quantifier';

# TAP::Grammar's nested sub-test shape.
grammar T {
    token TOP { <entry>+ }
    token entry { ^^ [ <t> | <sub-test> || <unk> ] \n }
    token t { 'ok' }
    token unk { \N* }
    token sub-entry(Int $indent) { <t> | <sub-test($indent)> }
    token indent(Int $indent) { '    ' ** { $indent } }
    token sub-test(Int $indent = 0) {
        '    '
        [ <sub-entry($indent + 1)> \n ]+ % [ <.indent($indent+1)> ]
        <.indent($indent)> <t>
    }
}
ok T.parse("ok\n    ok\n        ok\n    ok\nok\n"), 'nested sub-test';
