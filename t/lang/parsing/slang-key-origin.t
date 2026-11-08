use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# A slang token reads the core `sub` definition after a keyword of its own with
# `.key-origin('routine-def', 'sub')` (ADR-15762, #13370, #16888): the node is
# the RakuAST::Sub the core rule builds for the rest of the declaration.

plan 3;

use KeyOriginSlang;

fn twice($n) { $n * 2 }
is twice(21), 42, 'a named routine with a signature';
fn greet(Str $who, :$punct = '!') { "hi $who$punct" }
is greet('bob'), 'hi bob!', 'typed and named parameters';
is greet('bob', :punct('?')), 'hi bob?', 'named arguments bind';
