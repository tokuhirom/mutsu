use Test;

# An INIT or CHECK phaser runs once, before the mainline, wherever it is
# written. The phaser lift reaches every position since it walks the tree
# through the exhaustive mutable visitor (ADR-10499); each expectation was
# checked against rakudo.

plan 38;

my @log;
@log.push('main');

sub before-main($tag) { (@log.first($tag, :k) // Inf) < @log.first('main', :k) }

class C1 { method m { INIT @log.push('method') } }
# TODO: rakudo runs it before the mainline too; mutsu runs a class body's
# INIT when the class body runs (#10552).
ok @log.first('method'), 'INIT in a method body runs without the method being called';

sub enter-phaser { ENTER { INIT @log.push('enter') } }
ok before-main('enter'), 'INIT in an ENTER phaser body';

sub leave-phaser { LEAVE { INIT @log.push('leave') } }
ok before-main('leave'), 'INIT in a LEAVE phaser body';

my @a;
@a[0] = { INIT @log.push('index-assign'); 1 };
ok before-main('index-assign'), 'INIT in a closure on the right of an element assignment';

my @fed = (1, 2) ==> map { INIT @log.push('feed'); $_ };
ok before-main('feed'), 'INIT in a closure in a feed';
is-deeply @fed, [1, 2], '... and the feed still runs';

my $x = 5;
$x += INIT { @log.push('compound'); 3 };
ok before-main('compound'), 'INIT on the right of a compound assignment';
is $x, 8, '... runs once and is the operand\'s value';
is @log.grep('compound').elems, 1, '... exactly once';

my &dflt = sub ($a = INIT { @log.push('param-init'); 4 }) { $a };
ok before-main('param-init'), 'INIT in a parameter default runs before the mainline';
is dflt(), 4, '... and the default is the phaser\'s value';
is dflt(), 4, '... on every call';
is @log.grep('param-init').elems, 1, '... but the phaser runs once';

sub chk($a = CHECK { @log.push('param-check'); 5 }) { $a }
ok before-main('param-check') && chk() == 5, 'CHECK in a parameter default of a named sub';

my $lex = 7;
sub rx-init { 'a' ~~ / a { INIT @log.push('regex-init') } / }
ok before-main('regex-init'), 'INIT in a regex code block runs before the mainline';
rx-init(); rx-init();
is @log.grep('regex-init').elems, 1, '... once, however often the regex matches';

sub rx-lex { 'a' ~~ / a { INIT @log.push('regex-lex'); $lex } / }
ok before-main('regex-lex'), 'INIT in a regex code block that closes over a lexical';
rx-lex(); rx-lex();
is @log.grep('regex-lex').elems, 1, '... once, however often it matches';

sub rx-check { 'a' ~~ / a { CHECK @log.push('regex-check') } / }
ok before-main('regex-check'), 'CHECK in a regex code block runs before the mainline';

grammar G12748 {
    token TOP { a { INIT @log.push('token-init') } <rest> }
    rule rest { b { $lex; CHECK @log.push('rule-check') } c { @log.push('rule-match') } }
}
# TODO: rakudo runs both before the mainline; mutsu runs them when the
# grammar body runs (#10552), as for a method body above.
ok @log.first('token-init'), 'INIT in a token body runs without the token being matched';
ok @log.first('rule-check'), 'CHECK in a rule body runs without the rule being matched';
ok G12748.parse('ab c'), '... and the grammar still matches';
G12748.parse('ab c');
is @log.grep('token-init').elems, 1, '... the token phaser ran once, however often it matches';
is @log.grep('rule-check').elems, 1, '... and so did the rule phaser';
is @log.grep('rule-match').elems, 2, '... while the plain code block ran per match';

my token lexical-tok { x { INIT @log.push('my-token-init') } }
ok before-main('my-token-init') && ('x' ~~ &lexical-tok), 'INIT in a lexical token';
is @log.grep('my-token-init').elems, 1, '... once';

grammar G12812 {
    token TOP { a { if 1 { INIT @log.push('nested-init') }; for 1 { CHECK @log.push('nested-check'); } } b }
}
ok @log.first('nested-init') && @log.first('nested-check'), 'INIT/CHECK nested in a token code block run without a match';
G12812.parse('ab'); G12812.parse('ab');
is @log.grep('nested-init').elems, 1, '... the nested INIT ran once, however often it matches';
is @log.grep('nested-check').elems, 1, '... and so did the nested CHECK';
ok G12812.parse('ab'), '... and the grammar still matches';

# A BEGIN statement in a declaration's code block runs once, at BEGIN time,
# and not when the declaration is matched (#12812).
grammar G12812-begin {
    token TOP { a { BEGIN @log.push('token-begin') } b { if 1 { BEGIN @log.push('nested-begin') } } <r> }
    rule r { c { BEGIN @log.push('rule-begin'); @log.push('rule-match-2') } }
}
ok before-main('token-begin'), 'BEGIN in a token code block runs before the mainline';
ok before-main('nested-begin'), 'BEGIN nested in a token code block';
ok before-main('rule-begin'), 'BEGIN in a rule code block, after an earlier token';
is @log.grep('token-begin').elems, 1, '... and has run once before any match';
ok G12812-begin.parse('abc'), '... the grammar still matches';
G12812-begin.parse('abc');
is @log.grep('token-begin' | 'nested-begin' | 'rule-begin').elems, 3, '... and the BEGINs did not run again';
is @log.grep('rule-match-2').elems, 2, '... while the rest of the code block ran per match';
