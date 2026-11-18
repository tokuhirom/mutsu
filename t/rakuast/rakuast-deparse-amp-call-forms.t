use Test;
use experimental :rakuast;

plan 12;

# The `.&` call forms keep their `&` in `.DEPARSE` (#14845).
sub dep($src) { $src.AST.DEPARSE.lines.tail.trim }

is dep('my $x = "ab".&({ $^x ~ $^x });'), 'my $x = "ab".&({ $^x ~ $^x })', '.&(block)';
is dep('my $f = sub ($a) { $a }; my $z = 5.&($f);'), 'my $z = 5.&($f)', '.&($var)';
is dep('my $n = 5; my $z = $n.&(* + 1);'), 'my $z = $n.&(* + 1)', '.&(WhateverCode)';
is dep('my $x = 5.&{ $^a + $^b }(3);'), 'my $x = 5.&{ $^a + $^b }(3)', '.&{ placeholders }(args)';

# On the topic the call is a `Term::TopicCall`.
my $topic = 'for 1,2 { my $y = .&{ $_ * 2 }; }'.AST;
is $topic.find-nodes(RakuAST::Term::TopicCall).elems, 1, 'the topic block call is a TopicCall';
is $topic.find-nodes(RakuAST::Call::TermAsMethod).elems, 1, 'over a TermAsMethod';
is $topic.DEPARSE.lines[1].trim, 'my $y = .&{ $_ * 2 }', 'topic .&{ }';
is $topic.DEPARSE.lines.elems, 3, 'one statement in the loop body';

is EVAL('"ab".&({ $^x ~ $^x })'.AST), 'abab', 'EVAL of .&(block)';
is EVAL('my $n = 5; $n.&(* + 1)'.AST), 6, 'EVAL of .&(WhateverCode)';
is EVAL('(3,4).map({ .&{ $^a + $^b }(5) }).raku'.AST), '(8, 9).Seq', 'EVAL of the topic call with arguments';
is EVAL('sub foo($a, $b = 10) { $a + $b }; (3,4).map({ .&foo(1) }).raku'.AST), '(4, 5).Seq', 'EVAL of topic .&name(args)';
