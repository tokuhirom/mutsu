use Test;

# The blocks of expression terms, `use` / `need` module names and
# parenthesized or colon-called method calls carry the origins Rakudo gives
# them (ADR-10723 Stage 2): `{ ... }`, `do` / `start` / `gather` / `try`
# blocks, an anonymous `sub { ... }`, a pointy `-> $x { ... }`, a `whenever`
# block, and the `Call::Method` of `(1, 2).map({ ... })` and `.map: -> ...`.
# Ranges measured on rakudo 2026.09.

plan 33;

sub range($node) { with $node.origin { "{.from}..{.to}" } else { 'none' } }

my $src = q:to/END/;
use Test;
need Test;
my $g = { $_ };
my $c = do { 2 };
my $a = start { 1; 2 };
my $b = gather { take 1 };
my $e = sub { 3 };
my $f = -> $x { $x };
my $k = -> Int $x, $y { $x };
my $j = try { 4 };
my @i = (1, 2).map({ $_ + 1 });
my @h = (1, 2).map: -> $x { $x };
my $m = do { # c
  5 };
react { whenever $a -> $v { say $v } }
END

my %by;
sub walk($node) {
    %by{$node.^name.subst('RakuAST::', '')}.push: range($node);
    $node.visit-children(&walk);
}
$src.AST.visit-children(&walk);

# Rakudo also lists a range-less `Signature` for the anonymous sub, which mutsu
# does not have; the ranged ones are compared.
my @sig = %by<Signature>.grep(* ne 'none');

is %by<Statement::Use>[0], '0..8', 'use';
is %by<Name>[0], '4..8', 'the module name of use';
is %by<Statement::Need>[0], '10..19', 'need';
is %by<Name>[1], '15..19', 'the module name of need';

is %by<Block>[0], '29..35', 'a bare block';
is %by<StatementPrefix::Do>[0], '45..53', 'a do prefix';
is %by<Block>[1], '48..53', 'its block';
is %by<Blockoid>[1], '48..53', 'its blockoid';
is %by<StatementList>[1], '50..51', 'and statement list';

is %by<StatementPrefix::Start>[0], '63..77', 'a start prefix';
is %by<Block>[2], '69..77', 'its block';
is %by<StatementList>[2], '71..75', 'and a statement list of two statements';

is %by<StatementPrefix::Gather>[0], '87..104', 'a gather prefix';
is %by<Block>[3], '94..104', 'its block';

is %by<Sub>[0], '114..123', 'an anonymous sub';
is %by<Blockoid>[4], '118..123', 'its blockoid';
is %by<StatementList>[4], '120..121', 'and statement list';

is %by<PointyBlock>[0], '133..145', 'a pointy block';
is @sig[0], '136..138', 'its signature';
is %by<Parameter>[0], '136..138', 'its parameter';
is %by<Blockoid>[5], '139..145', 'its blockoid';

is %by<PointyBlock>[1], '155..175', 'a pointy block with two parameters';
is @sig[1], '158..168', 'its signature';
is %by<Parameter>[1], '158..164', 'a typed parameter';
is %by<Parameter>[2], '166..168', 'and an untyped one';

is %by<StatementPrefix::Try>[0], '185..194', 'a try prefix';

is %by<Call::Method>[0], '211..226', 'a method call on a parenthesized operand';
is %by<Block>[5], '215..225', 'and its block argument';

is %by<Call::Method>[1], '243..260', 'a colon-called method';
is %by<PointyBlock>[2], '248..260', 'and its pointy block argument';

is %by<PointyBlock>[3], '306..322', 'the pointy block of a whenever';

# A comment inside a block could start its statement list later than the text
# does, so mutsu leaves the list absent (Rakudo has 281..282).
is %by<Blockoid>[10], '273..284', 'a block with a comment keeps its blockoid';
is %by<StatementList>[10], 'none', 'but has no statement list range';
