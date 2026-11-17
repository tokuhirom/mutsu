use experimental :rakuast;
use Test;

# A hash literal entry whose key is a quoted string or a number keeps the key
# as written: RakuAST holds an `ApplyInfix` over it, where a bare `a => 1` is a
# `FatArrow` (#14880, the `.DEPARSE` corpus's integration/advent2010-day08.t).

plan 12;

sub init($src) { $src.AST.statements[0].expression.initializer.expression }
sub deparse($src) { $src.AST.statements[0].DEPARSE.chomp }

is deparse(Q{my $h = {"a" => 1, b => 2}}), 'my $h = {"a" => 1, b => 2}', 'a quoted key beside a bare one';
is deparse(Q{my $h = {"a b" => 1, "c" => 2}}), 'my $h = {"a b" => 1, "c" => 2}', 'a key with a space';
is deparse(Q[my $h = {'a' => 1, 'b' => 2}]), 'my $h = {"a" => 1, "b" => 2}', 'a single-quoted key';
is deparse(Q{my $h = {"a" => 1}}), 'my $h = {"a" => 1}', 'a lone quoted pair';
is deparse(Q{my $h = {"a" => 1, :b}}), 'my $h = {"a" => 1, :b}', 'beside a colonpair';
is deparse(Q{my $h = {a => 1, b => 2}}), 'my $h = {a => 1, b => 2}', 'bare keys stay fat arrows';

my $pairs = init(Q{my $h = {1 => 2, "a" => 3, b => 4}}).expression.operands;
isa-ok $pairs[0], RakuAST::ApplyInfix, 'a numeric key is an ApplyInfix';
isa-ok $pairs[0].left, RakuAST::IntLiteral, 'over the number';
isa-ok $pairs[1].left, RakuAST::QuotedString, 'a quoted key is over the string';
isa-ok $pairs[2], RakuAST::FatArrow, 'a bare key is a FatArrow';

my $h = {"a b" => 1, 2 => 3, c => 4};
is $h.keys.sort.join(','), '2,a b,c', 'the hash has the keys';
is-deeply EVAL(Q[{"x" => 1, 2 => 2}].AST), {x => 1, "2" => 2}, 'EVAL of the .AST builds the same hash';
