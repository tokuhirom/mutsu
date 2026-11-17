use experimental :rakuast;
use Test;

# A shell quote (`qx`, `qqx`, `q:x`, `qq:x`) is a `RakuAST::QuotedString` with
# the `exec` processor, as rakudo's RakuAST frontend views it; a written
# `QX(...)` stays a call (#17950).

plan 11;

my $x = 'hi';
sub expression($src) { $src.AST.statements[0].expression }

for q{qx/echo hi/}, q{qq:x/echo hi/}, q{qqx/echo $x/}, q{q:x/echo a/} -> $src {
    my $e = expression($src);
    ok $e ~~ RakuAST::QuotedString && $e.processors eqv ('exec',),
        "$src is a QuotedString with the exec processor";
}
isa-ok expression(q{QX("echo hi")}), RakuAST::Call::Name, 'a written QX(...) is a call';
is expression(q{qqx/echo $x/}).segments[1].^name, 'RakuAST::Var::Lexical',
    'qqx keeps its interpolated segment';

is EVAL(q{qx/echo round/}.AST).trim, 'round', 'the view runs through EVAL';
is EVAL(RakuAST::QuotedString.new(
    segments => (RakuAST::StrLiteral.new('echo built'),), processors => ('exec',),
)).trim, 'built', 'a hand-built exec quote runs its command';
is qx/echo run/.trim, 'run', 'qx runs';
is qqx/echo $x/.trim, 'hi', 'qqx interpolates';
is QX('echo call').trim, 'call', 'QX(...) runs';
