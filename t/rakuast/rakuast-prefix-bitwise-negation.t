use v6;
use experimental :rakuast;
use Test;

plan 8;

for '+^ 1', '?^ 1', '~^ "a"' -> $source {
    my $operator = $source.substr(0, 2);
    ok $source.AST.gist.contains('RakuAST::Prefix.new("' ~ $operator ~ '")'),
        "$operator reads as a Prefix node";
}

is Q|+^ 1|.AST.EVAL, -2, '+^ survives the AST round trip';
is Q|?^ 1|.AST.EVAL, False, '?^ survives the AST round trip';

is RakuAST::ApplyPrefix.new(
    prefix => RakuAST::Prefix.new('+^'),
    operand => RakuAST::IntLiteral.new(1),
).EVAL, -2, 'a constructed +^ prefix evaluates';
is RakuAST::ApplyPrefix.new(
    prefix => RakuAST::Prefix.new('?^'),
    operand => RakuAST::IntLiteral.new(1),
).EVAL, False, 'a constructed ?^ prefix evaluates';

# Rakudo parses ~^ but currently reports "prefix:<~^> not yet implemented".
if $*VM.name eq 'mutsu' {
    is Q|~^ "a"|.AST.EVAL.ords.join(','), (~^ "a").ords.join(','),
        '~^ uses the existing compiled prefix operation after lowering';
} else {
    skip 'Rakudo does not implement prefix ~^ yet';
}
