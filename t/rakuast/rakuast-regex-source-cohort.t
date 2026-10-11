use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

sub body($source) { $source.AST.statements.head.expression.body }
sub same($source, $expected, $description) {
    is EVAL($source.AST), $expected, $description;
}

for '0', '1', '3' -> $slot {
    my $node = body('rx/$' ~ $slot ~ '=(a)/');
    isa-ok $node, RakuAST::Regex::NamedCapture, 'numbered alias is a named-capture node';
    is $node.name, $slot, 'alias retains its numeric name';
    isa-ok $node.regex, RakuAST::Regex::CapturingGroup, 'alias retains the capturing group';
    same('my $m = "ab" ~~ /$' ~ $slot ~ '=(a) (b)/; ~$m[' ~ $slot ~ ']', 'a',
        'numbered alias survives the frontend round trip');
}
same Q[my $m = "xyb" ~~ /$0=(x) $0=(y) (b)/; $m[0].map(~*).join(',')], 'x,y',
    'repeated numbered alias retains static slot aggregation';
same Q[my $m = "12" ~~ /$0=(\d)+/; $m[0].map(~*).join(',')], '1,2',
    'quantified numbered alias retains every iteration';
same Q[my $m = "ab" ~~ /$1=[a] (b)/; ~$m[2]], 'b',
    'group after an alias continues static numbering';
my $constructed = RakuAST::QuotedRegex.new(body => RakuAST::Regex::NamedCapture.new(
    name => '2', regex => RakuAST::Regex::Literal.new('a')));
is ~('a' ~~ EVAL($constructed))[2], 'a', 'constructed numeric alias fills a positional slot';

for Q[rx/<?!alpha>/], Q[rx/<?!before a>/], Q[rx/<?!after a>/], Q[rx/<?![a]>/] -> $source {
    my $node = body($source);
    isa-ok $node, RakuAST::Regex::Assertion::Lookahead, 'outer assertion remains a lookahead';
    nok $node.negated, 'outer question marker is positive';
    isa-ok $node.assertion, RakuAST::Regex::Assertion::Lookahead, 'inner assertion remains nested';
    ok $node.assertion.negated, 'inner exclamation marker is negative';
}
same Q[~('a;' ~~ /a <?!alpha>/)], 'a', 'double-prefix assertion passes without consuming';
same Q[so('ab' ~~ /a <?!alpha>/)], False, 'double-prefix assertion rejects a letter';
same Q[~('ca' ~~ /<?!after b> a/)], 'a', 'nested negative lookbehind survives';

for Q[rx/‚a%>’/], Q[rx/‚a%>‘/], Q[rx/„a%>”/], Q[rx/“a%>”/], Q[rx/‘a%>’/], Q[rx/｢a%>｣/] -> $source {
    my $node = body($source);
    isa-ok $node, RakuAST::Regex::Quote, 'Unicode quote is a quote node';
    is $node.quoted.segments.head.value, 'a%>', 'metacharacters remain literal data';
    ok 'a%>' ~~ EVAL($source.AST), 'Unicode quote survives the round trip';
}
same Q[~('a' ~~ /<!before ‘%>’> ./)], 'a', 'Unicode quote cannot prematurely close an assertion';
same Q[my $x = 'b'; ~('abc' ~~ /a “$x” c/)], 'abc', 'smart double quote retains interpolation';
same Q[my @a = <a b>; ~('a,b' ~~ /"@a.join(",")"/)], 'a,b',
    'nested ASCII quotes in a method argument do not end the regex quote';
same Q[my $m = '1,2' ~~ /$<x>=\d+ % ","/; ~$m<x>], '1,2',
    'alias owns its complete separated quantifier';
same Q[~('a%b' ~~ /“a%b”/)], 'a%b', 'a percent inside a smart qq quote is ordinary text';
same Q[~('a , a , a' ~~ /:s a+% "," /)], 'a',
    'separator without preceding whitespace keeps that distinction';
same Q[~('a, a, a' ~~ /:s a+ % ","/)], 'a',
    'separator without following whitespace keeps that distinction';
same Q[~('a , a , a' ~~ /:s a +% "," /)], 'a , a , a',
    'whitespace inside a quantified atom survives per item';
same Q[my $m = '1, 2' ~~ /:s $<x>=\d ** 2 % "," /; ~$m<x>], '1, 2',
    'sigspace alias retains its complete separated span';
my $alias = body(Q[rx/$<x>=\d ** 2 % "," /]);
isa-ok $alias.regex, RakuAST::Regex::QuantifiedAtom,
    'alias body has no extra wrapper for whitespace before its separator';
isa-ok $alias.regex.atom, RakuAST::Regex::WithWhitespace,
    'pre-quantifier whitespace belongs to each aliased atom';
isa-ok $alias.regex.separator, RakuAST::Regex::WithWhitespace,
    'trailing separator whitespace stays on the separator';
my $aggregate = body(Q[rx/@<x>=<digit>+/]);
ok $aggregate.array, 'aggregate subrule alias retains its sigil';
isa-ok $aggregate.regex, RakuAST::Regex::QuantifiedAtom,
    'aggregate alias retains its quantified subrule';
same Q[my $m = '12' ~~ /@<x>=<digit>+/; $m<x>.map(~*).join(',')], '1,2',
    'aggregate subrule alias retains per-iteration captures';
same Q[my $m = '12' ~~ /$<x>=<.digit>+/; $m<x>.map(~*).join(',')], '1,2',
    'dot-suppressed subrule alias retains per-iteration captures';
same Q[~('a"b' ~~ /“a"b”/)], 'a"b', 'ASCII double quote is data inside smart double quotes';
same Q[~('a"b' ~~ /„a"b”/)], 'a"b', 'ASCII double quote is data inside low smart quotes';
dies-ok { Q[rx/"%b\x[QQ]"/].AST }, 'malformed escapes cannot become a partial quoted literal';
same Q[~('%h.keys' ~~ /“%h.keys”/)], '%h.keys',
    'a hash-looking method without parentheses remains literal qq text';
done-testing;
