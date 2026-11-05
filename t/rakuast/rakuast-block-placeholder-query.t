use Test;

# `RakuAST::Block.has-placeholder-parameters`: true when the block's own body
# declares a placeholder; a nested block, pointy block or routine owns its
# own. Measured on rakudo 2026.09 (#14916).

plan 10;

use experimental :rakuast;

sub q(Str $src) { $src.AST.statements[0].expression.initializer.expression.has-placeholder-parameters }

is q('my $g = { $^a + 1 }'), True, 'a positional placeholder';
is q('my $b = { 1 }'), False, 'none';
is q('my $n = { my $z = { $^q }; 3 }'), False, 'a placeholder of a nested block is the nested one\'s';
is q('my $m = { $^a + $^b }'), True, 'two placeholders';
is q('my $p = { @_ }'), True, '@_';
is q('my $q = { if True { $^x } }'), False, 'inside an if block';
is q('my $r = { for 1,2 { $^y } }'), False, 'inside a for block';
is q('my $s = { sub { $^z } }'), False, 'inside an anonymous sub';
is q('my $t = { $^a; { $^b } }'), True, 'own placeholder plus a nested one';
is q('my $u = { :$^n }'), True, 'a named placeholder';
