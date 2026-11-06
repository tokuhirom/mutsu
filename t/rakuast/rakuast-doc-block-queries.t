use Test;
use experimental :rakuast;

# `Doc::Block.allowed-markup` (a `Map` subclass type object) and
# `.visual-table` (`True` for a table of rows, a junction for a non-table),
# measured on rakudo 2026.09.

plan 18;

sub block(Str $type, *@p) { RakuAST::Doc::Block.new(:$type, :paragraphs(@p)) }

for <code implicit-code defn> -> $type {
    my $m = block($type, "x").allowed-markup;
    is $m.^name, 'RakuAST::Doc::Block::NOK', "$type: verbatim types allow no markup";
}
for <para item table head comment pod nested foo> -> $type {
    is block($type, "x").allowed-markup.^name, 'RakuAST::Doc::Block::OK',
        "$type allows markup";
}

my $ok = block('para', "x").allowed-markup;
is-deeply $ok<B>, True, 'OK answers True for any letter';
is-deeply block('code', "x").allowed-markup<B>, False, 'NOK answers False for any letter';
nok $ok.defined, 'the answer is a type object';
ok $ok ~~ Map, 'it is a Map subclass';

my $row = RakuAST::Doc::LegacyRow.new(:cells<a b>);
is-deeply block('table', $row).visual-table, True, 'table of rows is visual';
is-deeply block('table', "x").visual-table, False, 'table of strings is not';
is block('para', $row).visual-table.raku, 'any(Bool::False, Bool::False)',
    'a non-table answers a junction';
