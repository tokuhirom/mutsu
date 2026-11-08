use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# A slang `statement-prefix:sym<>` token reads the core `<blorst>` after its
# keyword (ADR-15762, #13370): `{ ... }` is a RakuAST::Block, anything else one
# RakuAST::Statement::Expression.

plan 2;

use StatementPrefixSlang;

my $block = (TRACKED { 1 + 2 });
like $block, /^ 'RakuAST::Block RakuAST::Block.new(' .* 'RakuAST::IntLiteral.new(1)'/,
    'a braced blorst is a Block';
my $stmt = (TRACKED 3 * 4);
like $stmt, /^ 'RakuAST::Statement::Expression RakuAST::Statement::Expression.new('/,
    'an unbraced blorst is a Statement::Expression';
