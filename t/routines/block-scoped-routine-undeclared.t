use Test;

# A routine declared in a block is lexical to that block (#15861): a bare call
# after the block is an `Undeclared routine` at compile time, as in rakudo.

plan 9;

throws-like { EVAL '{ sub foo15861 { 1 } }; foo15861' }, X::Undeclared::Symbols,
    'a sub of a bare block is not callable after it';
throws-like { EVAL 'if 1 { sub foo15861b { 1 } }; foo15861b' }, X::Undeclared::Symbols,
    'a sub of an if block is not callable after it';
throws-like { EVAL 'for 1 { sub foo15861c { 1 } }; foo15861c' }, X::Undeclared::Symbols,
    'a sub of a loop body is not callable after it';

is EVAL('{ my $r = foo15861d(); sub foo15861d { "hoisted" }; $r }'), 'hoisted',
    'a call before the declaration in the same block works (hoisting)';
is EVAL('{ sub foo15861e { "in" }; { foo15861e } }'), 'in',
    'a nested block sees the routine of its enclosing block';
is EVAL('{ sub foo15861f { "in" }; my $r = foo15861f; $r }'), 'in',
    'a call in the declaring block works';
is EVAL('sub foo15861g { "top" }; { foo15861g }'), 'top',
    'a unit-level routine is visible in a block';
is EVAL('module M15861 { sub foo15861h is export { "exp" } }; import M15861; foo15861h'), 'exp',
    'an exported routine of a package block is reachable after an import';
is EVAL('{ my sub foo15861i { "my" }; foo15861i }'), 'my', 'a my sub in its block';
