use Test;

plan 7;

my $sub = Q[sub f(--> "foo") { }].AST.raku;
ok $sub.contains('returns    => RakuAST::QuotedString.new('), 'sub returns a QuotedString';
ok $sub.contains('RakuAST::StrLiteral.new("foo")'), 'with the string as its segment';
ok Q[sub f(--> 'bar') { }].AST.raku.contains('RakuAST::StrLiteral.new("bar")'), 'single quotes give the same node';
ok Q[-> --> "done" { 42 }].AST.raku.contains('RakuAST::StrLiteral.new("done")'), 'a pointy block carries it too';

my $f = EVAL Q[sub (--> "foo") { }].AST;
is $f(), 'foo', 'EVAL: a routine pinned to a string literal returns it';
my $p = EVAL Q[-> --> "done" { 42 }].AST;
is $p(), 'done', 'EVAL: so does a pointy block';
my $e = EVAL Q[sub (--> '') { }].AST;
is $e(), '', 'EVAL: the empty string literal';
