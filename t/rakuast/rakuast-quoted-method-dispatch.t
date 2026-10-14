use Test;

plan 8;

my $ast = Q[-> $x { $x.?"foo"() }].AST;
my $text = $ast.raku;
ok $text.contains('RakuAST::Call::QuotedMethod.new('), '.?"foo" is a Call::QuotedMethod';
ok $text.contains('dispatch => ".?"'), 'the dispatch string is kept';
ok Q[-> $x { $x."foo"() }].AST.raku !~~ /dispatch/, 'a plain quoted call has no dispatch field';
ok Q[-> $x { $x.+"foo"() }].AST.raku.contains('dispatch => ".+"'), '.+ dispatch';
ok Q[-> $x { $x.*"foo"() }].AST.raku.contains('dispatch => ".*"'), '.* dispatch';

my $f = EVAL Q[-> $x { $x.?"no-such-method"() // 'absent' }].AST;
is $f(1), 'absent', 'EVAL of .?"name" falls back when the method is missing';
my $g = EVAL Q[-> $x { $x.?"abs"() }].AST;
is $g(-3), 3, 'EVAL of .?"name" calls an existing method';
my $name = 'chars';
my $h = EVAL Q[-> $x, $n { $x.?"$n"() }].AST;
is $h("abc", $name), 3, 'an interpolated quoted name keeps its dispatch modifier';
