use Test;
use experimental :rakuast;

# `RakuAST::Assignment.item` is the `:item` flag, and `properties` / `reducer-name`
# answer item or list assignment. Measured on Rakudo 2026.11 (#14916).

plan 12;

my $list = RakuAST::Assignment.new;
my $item = RakuAST::Assignment.new(:item);

is-deeply $list.item, False, 'a list assignment is not an item assignment';
is-deeply $item.item, True, ':item is an item assignment';
is $list.properties.dba, 'list assignment', 'list assignment properties';
is $item.properties.dba, 'item-assignment', 'item assignment properties';
is $list.properties.precedence, 'i=', 'precedence of a list assignment';
is $item.properties.precedence, 'i=', 'precedence of an item assignment';
is $list.reducer-name, '&METAOP_REDUCE_RIGHT', 'assignment reduces to the right';
is $item.reducer-name, '&METAOP_REDUCE_RIGHT', 'so does an item assignment';

my $scalar = Q|my $x; $x = 1|.AST.statements[1].expression.infix;
is-deeply $scalar.item, True, 'a scalar assignment from source is an item assignment';
my $array = Q|my @x; @x = 1|.AST.statements[1].expression.infix;
is-deeply $array.item, False, 'an array assignment from source is a list assignment';
is $scalar.properties.dba, 'item-assignment', 'its properties follow';
is $array.properties.dba, 'list assignment', 'and for the list form';
