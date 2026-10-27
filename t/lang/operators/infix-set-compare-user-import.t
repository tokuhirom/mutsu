use lib $?FILE.IO.parent(3).add('lib');
use Test;
use SetCompareUserInfix;

plan 11;

my $c = Col.new(:name<x>);
is ($c (<=) <a b>), 'le', 'imported (<=) overload';
is ($c ⊆ <a b>), 'le-u', 'imported ⊆ overload';
is ($c ⊇ <a b>), 'ge-u', 'imported ⊇ overload';
is ($c (<) <a b>), 'lt', 'imported (<) overload';
is ($c ⊂ <a b>), 'lt-u', 'imported ⊂ overload';
is ($c (>) <a b>), 'gt', 'imported (>) overload';
is ($c ⊃ <a b>), 'gt-u', 'imported ⊃ overload';

is-deeply (<a> (<) <a b>), True, 'the core operator still answers other operands';
is-deeply (<a b> (>) <a>), True, 'the core (>) still answers other operands';
is-deeply (<a b> (<=) <a>), False, 'the core (<=) still answers other operands';
is-deeply (<a b> ⊇ <a>), True, 'the core ⊇ still answers other operands';
