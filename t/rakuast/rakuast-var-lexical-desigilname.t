use v6.e.PREVIEW;
use experimental :rakuast;
use Test;

# `RakuAST::Var::Lexical.desigilname` (#14916): the name without its sigil, as
# one `Name` identifier leaf (a `::` is not split).

my @cases = '$foo', '@a', '%h', '&f', '$Foo::bar', '$foo-bar', '$', '$_', '$/',
    '$!', '$1', '$a::b::c', '$::x';
plan 5 + @cases;

for @cases -> $spelling {
    my $v = RakuAST::Var::Lexical.new($spelling);
    my $want = $spelling.substr(1);
    is $v.desigilname.raku, RakuAST::Name.from-identifier($want).raku,
        "desigilname of $spelling";
}

my $v = RakuAST::Var::Lexical.new('$x');
is $v.desigilname.^name, 'RakuAST::Name', 'the answer is a Name node';
is $v.name, '$x', 'name keeps the sigil';
ok $v.^can('desigilname').so, '.^can sees it';
my $parsed = 'my $z = 1; say $z'.AST.statements[1].expression.args.args[0];
is $parsed.desigilname.raku, RakuAST::Name.from-identifier('z').raku,
    'a parsed variable answers it too';
nok RakuAST::Var::Dynamic.^can('desigilname').so, 'Var::Dynamic has none';
