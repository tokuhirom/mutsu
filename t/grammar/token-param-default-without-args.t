use Test;

plan 8;

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

# Named defaults take the same binding path as positional defaults when a
# parameterized token is invoked as a bare subrule.
grammar N {
    token TOP { <n> }
    token n(:$k = 'z') { $k }
}
ok N.parse('z'), 'named default bound for a bare subrule';
nok N.parse('x'), 'bare subrule matches its named default';
ok N.parse('z', :rule<n>), 'named default bound for a selected start rule';

grammar E {
    token TOP { <n(:k<y>)> }
    token n(:$k = 'z') { $k }
}
ok E.parse('y'), 'explicit named subrule argument overrides the default';
